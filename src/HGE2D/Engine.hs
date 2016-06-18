module HGE2D.Engine where

import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Time
import HGE2D.Render ()

import Control.Concurrent (newMVar, readMVar, takeMVar, putMVar, MVar)
import Graphics.UI.GLUT

runEngine :: EngineState a -> a -> IO ()
runEngine es impl = do

    state <- newMVar impl

    (_progName, _args)    <- getArgsAndInitialize
    initialDisplayMode    $= [DoubleBuffered]
    initialWindowSize     $= Size (round $ fst $ getSize es impl) (round $ snd $ getSize es impl)
    _window               <- createWindow $ getTitle es impl
    keyboardMouseCallback $= Just (keyboardMouse es state)
    motionCallback        $= Just (mouseGrab es state)
    passiveMotionCallback $= Just (mouseHover es state)
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    displayCallback       $= display es state
    reshapeCallback       $= Just (reshape es state)
    idleCallback          $= Just (idle es state)
    mainLoop

--------------------------------------------------------------------------------

display :: EngineState a -> MVar (a) -> IO ()
display es mvarGs = do
  clear [ColorBuffer]
  gs <- readMVar mvarGs
  glRender $ toGlInstr es gs
  swapBuffers

--------------------------------------------------------------------------------

reshape :: EngineState a -> MVar (a) -> Size -> IO ()
reshape es mvarGs (Size width height) = do
    gs <- takeMVar mvarGs

    let newState = resize es (realToFrac width, realToFrac height) gs

    putMVar mvarGs newState

    viewport $= (Position 0 0, Size width height)
    postRedisplay Nothing


--------------------------------------------------------------------------------

---TODO here named grab, but engine method named drag, name both the same
mouseGrab :: EngineState a -> MVar (a) -> Position -> IO ()
mouseGrab es mvarGs (Position x y) = do
    gs <- takeMVar mvarGs ---TODO rename

    let w          = fst $ getSize es gs
        h          = snd $ getSize es gs
        correctedX = (realToFrac x) * (fst $ getSize es gs) / w
        correctedY = (realToFrac y) * (snd $ getSize es gs) / h
        newState   = drag es correctedX correctedY gs

    putMVar mvarGs newState
    return ()

mouseHover :: EngineState a -> MVar (a) -> Position -> IO ()
mouseHover es mvarGs (Position x y) = do
    gs <- takeMVar mvarGs ---TODO rename

    let w          = fst $ getSize es gs
        h          = snd $ getSize es gs
        correctedX = (realToFrac x) * (fst $ getSize es gs) / w
        correctedY = (realToFrac y) * (snd $ getSize es gs) / h
        newState   = hover es correctedX correctedY gs

    putMVar mvarGs newState
    return ()


keyboardMouse :: EngineState a -> MVar (a) -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse es mvarGs (MouseButton LeftButton) Down _modifiers (Position x y) = mouseDown es mvarGs x y
keyboardMouse es mvarGs (MouseButton LeftButton) Up   _modifiers (Position x y) = mouseUp   es mvarGs x y
keyboardMouse es mvarGs (Char        c)          Down _modifiers (Position x y) = keyDown   es mvarGs x y c
keyboardMouse es mvarGs (Char        c)          Up   _modifiers (Position x y) = keyUp     es mvarGs x y c
keyboardMouse _ _ _ _ _ _ =  return ()

--------------------------------------------------------------------------------

mouseDown :: EngineState a -> MVar (a) -> GLint -> GLint -> IO ()
mouseDown es mvarGs x y = do
    gs <- takeMVar mvarGs ---TODO rename

    ---TODO define method for corrections since used here and in hover
    let w          = fst $ getSize es gs
        h          = snd $ getSize es gs
        correctedX = (realToFrac x) * (fst $ getSize es gs) / w
        correctedY = (realToFrac y) * (snd $ getSize es gs) / h
        newState   = click es correctedX correctedY gs

    putMVar mvarGs newState
    return ()

mouseUp :: EngineState a -> MVar (a) -> GLint -> GLint -> IO ()
mouseUp es mvarGs x y = do
    gs <- takeMVar mvarGs ---TODO rename

    ---TODO define method for corrections since used here and in hover
    let w          = fst $ getSize es gs
        h          = snd $ getSize es gs
        correctedX = (realToFrac x) * (fst $ getSize es gs) / w
        correctedY = (realToFrac y) * (snd $ getSize es gs) / h
        newState   = mUp es correctedX correctedY gs

    putMVar mvarGs newState
    return ()

--------------------------------------------------------------------------------

keyDown :: EngineState a -> MVar (a) -> GLint -> GLint -> Char -> IO ()
keyDown es mvarGs x y c = do
    gs <- takeMVar mvarGs ---TODO rename

    ---TODO define method for corrections since used here and in hover
    let w          = fst $ getSize es gs
        h          = snd $ getSize es gs
        correctedX = (realToFrac x) * (fst $ getSize es gs) / w
        correctedY = (realToFrac y) * (snd $ getSize es gs) / h
        newState   = kDown es correctedX correctedY c gs

    putMVar mvarGs newState
    return ()

keyUp :: EngineState a -> MVar (a) -> GLint -> GLint -> Char -> IO ()
keyUp es mvarGs x y c = do
    gs <- takeMVar mvarGs ---TODO rename

    ---TODO define method for corrections since used here and in hover
    let w          = fst $ getSize es gs
        h          = snd $ getSize es gs
        correctedX = (realToFrac x) * (fst $ getSize es gs) / w
        correctedY = (realToFrac y) * (snd $ getSize es gs) / h
        newState   = kUp es correctedX correctedY c gs

    putMVar mvarGs newState
    return ()

--------------------------------------------------------------------------------

idle :: EngineState a -> MVar (a) -> IdleCallback
idle es mvarGs = do
  gs   <- takeMVar mvarGs
  secs <- getSeconds

  let ms       = toMilliSeconds secs
      deltaMs  = ms - (getTime es gs)
      newState = moveTime es deltaMs (setTime es ms gs) ---TODO currently bot setting the time AND transforming

  putMVar mvarGs newState
  postRedisplay Nothing
