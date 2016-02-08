module HGE2D.Engine where

import HGE2D.Classes
import HGE2D.Time
import HGE2D.Render ()

import Control.Concurrent (newMVar, readMVar, takeMVar, putMVar, MVar)
import Graphics.UI.GLUT

runEngine :: (EngineState a) => a -> IO ()
runEngine es = do

    state <- newMVar es

    (_progName, _args)    <- getArgsAndInitialize
    initialDisplayMode    $= [DoubleBuffered]
    initialWindowSize     $= Size (round $ fst $ getSize es) (round $ snd $ getSize es)
    _window               <- createWindow $ getTitle es
    keyboardMouseCallback $= Just (keyboardMouse state)
    motionCallback        $= Just (mouseGrab state)
    passiveMotionCallback $= Just (mouseHover state)
    blend $= Enabled
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    displayCallback       $= display state
    reshapeCallback       $= Just (reshape state)
    idleCallback          $= Just (idle state)
    mainLoop

--------------------------------------------------------------------------------

display :: (EngineState a) => MVar (a) -> IO ()
display mvarGs = do
  clear [ColorBuffer]
  gs <- readMVar mvarGs
  glRender $ toGlInstruction gs
  swapBuffers

--------------------------------------------------------------------------------

reshape :: (EngineState a) => MVar (a) -> Size -> IO ()
reshape mvarGs (Size width height) = do
    gs <- takeMVar mvarGs

    let newState = resize (realToFrac width) (realToFrac height) gs
    
    putMVar mvarGs newState

    viewport $= (Position 0 0, Size width height)
    postRedisplay Nothing


--------------------------------------------------------------------------------

mouseGrab :: (EngineState a) => MVar (a) -> Position -> IO ()
mouseGrab _ (Position x y) = putStrLn $ (show x) ++ (show y)

mouseHover :: (EngineState a) => MVar (a) -> Position -> IO ()
mouseHover mvarGs (Position x y) = do
    gs <- takeMVar mvarGs ---TODO rename

    let w          = fst $ getSize gs
        h          = snd $ getSize gs
        correctedX = (realToFrac x) * (getW gs) / w
        correctedY = (realToFrac y) * (getH gs) / h
        newState   = hover correctedX correctedY gs

    putMVar mvarGs newState
    return ()


keyboardMouse :: (EngineState a) => MVar (a) -> Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse mvarGs (MouseButton LeftButton) Down _modifiers (Position x y) = mouseDown mvarGs x y
keyboardMouse mvarGs (MouseButton LeftButton) Up   _modifiers (Position x y) = mouseUp   mvarGs x y
keyboardMouse _ _ _ _ _ =  return ()


mouseDown :: (EngineState a) => MVar (a) -> GLint -> GLint -> IO ()
mouseDown mvarGs x y = do
    gs <- takeMVar mvarGs ---TODO rename

    ---TODO define method for corrections since used here and in hover
    let w          = fst $ getSize gs
        h          = snd $ getSize gs
        correctedX = (realToFrac x) * (getW gs) / w
        correctedY = (realToFrac y) * (getH gs) / h
        newState   = click correctedX correctedY gs

    putMVar mvarGs newState
    return ()

mouseUp :: (EngineState a) => MVar (a) -> GLint -> GLint -> IO ()
mouseUp _ _ _ = return ()

--------------------------------------------------------------------------------

idle :: (EngineState a) => MVar (a) -> IdleCallback
idle mvarGs = do
  gs   <- takeMVar mvarGs
  secs <- getSeconds

  let ms       = toMilliSeconds secs
      deltaMs  = ms - (getTime gs)
      newState = moveInTime deltaMs (setTime ms gs) ---TODO currently bot setting the time AND transforming

  putMVar mvarGs newState
  postRedisplay Nothing


