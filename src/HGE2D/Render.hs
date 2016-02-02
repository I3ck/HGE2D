module HGE2D.Render where

import HGE2D.Datas
import HGE2D.ShapeFactory

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT

class GlRender a where
    glRender :: a -> IO ()

class GlInstructable a where
    toGlInstruction :: a -> RenderInstruction


instance GlRender RenderInstruction where
    glRender renderInstruction = case renderInstruction of
            (RenderNothing)             -> return ()
            (RenderText text)           -> currentRasterPosition $= Vertex4 0 0 0 1 >> renderString TimesRoman24 text --- TODO font as option?
            (RenderLineStrip shape w)   -> lineWidth $= w >> (renderPrimitive LineStrip $ mapM_ vertex shape) ---TODO lineWidth not working currently
            (RenderTriangle shape)      -> renderPrimitive Triangles $ mapM_ vertex shape
            (RenderLineLoop shape w)    -> lineWidth $= w >> (renderPrimitive LineLoop $ mapM_ vertex shape)
            (RenderTranslate w h)       -> translate2 w h
            (RenderRotate rad)          -> rotate2 rad
            (RenderColorize rgb)        -> colorRGB rgb
            (RenderColorizeAlpha rgba)  -> colorRGBA rgba
            (RenderPreserve instrs)     -> preservingMatrix (mapM_ glRender instrs)
            (RenderMany instrs)         -> mapM_ glRender instrs
