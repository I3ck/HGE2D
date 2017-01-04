-- |
-- Module      :  HGE2D.Engine
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing functions for the engine, mostly to interact with GLUT and OpenGL

module HGE2D.Engine where

import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Time
import HGE2D.Render ()

import Control.Concurrent (newMVar, readMVar, takeMVar, putMVar, swapMVar, MVar)
import Graphics.UI.GLUT

--------------------------------------------------------------------------------

-- | Main function to run the engine
runEngine :: EngineState a -> a -> IO ()
runEngine es impl = do
    secs <- getSeconds
    let ms     = toMilliSeconds secs
        (w, h) = getSize es impl

    state <- newMVar $ setTime es ms impl

    (_progName, _args)    <- getArgsAndInitialize
    initialDisplayMode    $= [DoubleBuffered]
    initialWindowSize     $= Size (round w) (round h)
    _window               <- createWindow $ getTitle es impl
    keyboardMouseCallback $= Just (keyboardMouse es state)
    motionCallback        $= Just (mouseGrab es state)
    passiveMotionCallback $= Just (mouseHover es state)
    blend                 $= Enabled
    blendFunc             $= (SrcAlpha, OneMinusSrcAlpha)
    displayCallback       $= display es state
    reshapeCallback       $= Just (reshape es state)
    idleCallback          $= Just (idle es state)
    mainLoop

--------------------------------------------------------------------------------

-- | Function to render the current state of the engine
display :: EngineState a -> MVar a -> IO ()
display es mvarGs = do
  clear [ColorBuffer]
  gs <- readMVar mvarGs
  glRender $ toGlInstr es gs
  swapBuffers

--------------------------------------------------------------------------------

-- | Function to react to changes of the window size
reshape :: EngineState a -> MVar a -> Size -> IO ()
reshape es mvarGs s@(Size width height) = do
    gs <- takeMVar mvarGs

    let newState = resize es (realToFrac width, realToFrac height) gs

    putMVar mvarGs newState

    viewport $= (Position 0 0, s)
    postRedisplay Nothing


--------------------------------------------------------------------------------

---TODO here named grab, but engine method named drag, name both the same

-- | Mouse grab interactions with the engine
mouseGrab :: EngineState a -> MVar a -> Position -> IO ()
mouseGrab es mvarGs (Position x y) = do
    gs <- takeMVar mvarGs ---TODO rename

    let w          = fst $ getSize es gs
        h          = snd $ getSize es gs
        correctedX = realToFrac x * (fst $ getSize es gs) / w
        correctedY = realToFrac y * (snd $ getSize es gs) / h
        newState   = drag es correctedX correctedY gs

    putMVar mvarGs newState
    return ()

-- | Mouse hover interactions with the engine
mouseHover :: EngineState a -> MVar a -> Position -> IO ()
mouseHover es mvarGs (Position x y) = do
    gs <- readMVar mvarGs ---TODO rename

    let w          = fst $ getSize es gs
        h          = snd $ getSize es gs
        correctedX = realToFrac x * (fst $ getSize es gs) / w
        correctedY = realToFrac y * (snd $ getSize es gs) / h
        newState   = hover es correctedX correctedY gs

    swapMVar mvarGs newState
    return ()


-- | Keyboard and mouse interactions with the engine
keyboardMouse :: EngineState a -> MVar a -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse es mvarGs (MouseButton LeftButton) Down _modifiers (Position x y) = mouseDown es mvarGs x y
keyboardMouse es mvarGs (MouseButton LeftButton) Up   _modifiers (Position x y) = mouseUp   es mvarGs x y
keyboardMouse es mvarGs (Char        c)          Down _modifiers (Position x y) = keyDown   es mvarGs x y c
keyboardMouse es mvarGs (Char        c)          Up   _modifiers (Position x y) = keyUp     es mvarGs x y c
keyboardMouse _ _ _ _ _ _ =  return ()

--------------------------------------------------------------------------------

-- | MouseDown interaction with the engine
mouseDown :: EngineState a -> MVar a -> GLint -> GLint -> IO ()
mouseDown es mvarGs x y = do
    gs <- readMVar mvarGs ---TODO rename

    ---TODO define method for corrections since used here and in hover
    let w          = fst $ getSize es gs
        h          = snd $ getSize es gs
        correctedX = realToFrac x * (fst $ getSize es gs) / w
        correctedY = realToFrac y * (snd $ getSize es gs) / h
        newState   = click es correctedX correctedY gs

    swapMVar mvarGs newState
    return ()

-- | MouseUp interaction with the engine
mouseUp :: EngineState a -> MVar a -> GLint -> GLint -> IO ()
mouseUp es mvarGs x y = do
    gs <- readMVar mvarGs ---TODO rename

    ---TODO define method for corrections since used here and in hover
    let w          = fst $ getSize es gs
        h          = snd $ getSize es gs
        correctedX = realToFrac x * (fst $ getSize es gs) / w
        correctedY = realToFrac y * (snd $ getSize es gs) / h
        newState   = mUp es correctedX correctedY gs

    swapMVar mvarGs newState
    return ()

--------------------------------------------------------------------------------

-- | KeyPress interaction with the engine
keyDown :: EngineState a -> MVar a -> GLint -> GLint -> Char -> IO ()
keyDown es mvarGs x y c = do
    gs <- readMVar mvarGs ---TODO rename

    ---TODO define method for corrections since used here and in hover
    let w          = fst $ getSize es gs
        h          = snd $ getSize es gs
        correctedX = realToFrac x * (fst $ getSize es gs) / w
        correctedY = realToFrac y * (snd $ getSize es gs) / h
        newState   = kDown es correctedX correctedY c gs

    swapMVar mvarGs newState
    return ()

-- | KeyRelease interaction with the engine
keyUp :: EngineState a -> MVar a -> GLint -> GLint -> Char -> IO ()
keyUp es mvarGs x y c = do
    gs <- readMVar mvarGs ---TODO rename

    ---TODO define method for corrections since used here and in hover
    let w          = fst $ getSize es gs
        h          = snd $ getSize es gs
        correctedX = realToFrac x * (fst $ getSize es gs) / w
        correctedY = realToFrac y * (snd $ getSize es gs) / h
        newState   = kUp es correctedX correctedY c gs

    swapMVar mvarGs newState
    return ()

--------------------------------------------------------------------------------

-- | Idle function of the engine. Used to e.g. apply changes in time to the game state
idle :: EngineState a -> MVar a -> IdleCallback
idle es mvarGs = do
  gs    <- readMVar mvarGs
  secs  <- getSeconds

  let ms       = toMilliSeconds secs
      deltaMs  = ms - getTime es gs
      newState = moveTime es deltaMs (setTime es ms gs) ---TODO currently bot setting the time AND transforming

  swapMVar mvarGs newState
  postRedisplay Nothing
