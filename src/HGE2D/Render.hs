module HGE2D.Render where

import HGE2D.Datas
import HGE2D.Classes
import HGE2D.ShapeFactory

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT

--------------------------------------------------------------------------------

instance GlRender RenderInstruction where
    glRender renderInstruction = case renderInstruction of
        RenderNothing                     -> return ()
        RenderWithCamera w h sX sY instrs -> glRender $ RenderPreserve ([RenderScale sX sY, RenderTranslate w h] ++ instrs)  
        RenderText text                   -> do currentRasterPosition $= Vertex4 0 0 0 1 
                                                renderString TimesRoman24 text

        RenderLineStrip shape w           -> do lineWidth $= w 
                                                (renderPrimitive LineStrip $ mapM_ vertex shape)

        RenderTriangle shape              -> renderPrimitive Triangles $ mapM_ vertex shape
        RenderLineLoop shape w            -> do lineWidth $= w 
                                                (renderPrimitive LineLoop $ mapM_ vertex shape)
        RenderScale sX sY                 -> scale2 sX sY
        RenderTranslate w h               -> translate2 w h
        RenderRotate rad                  -> rotate2 rad
        RenderColorize rgb                -> colorRGB rgb
        RenderColorizeAlpha rgba          -> colorRGBA rgba
        RenderPreserve instrs             -> preservingMatrix (mapM_ glRender instrs)
        RenderMany instrs                 -> mapM_ glRender instrs
