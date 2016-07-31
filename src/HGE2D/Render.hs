-- |
-- Module      :  HGE2D.Render
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing the instance of GlRender for a RenderInstruction

module HGE2D.Render where

import HGE2D.Datas
import HGE2D.Classes
import HGE2D.GlFunctions

import Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT

--------------------------------------------------------------------------------

-- | Instance of GLRender for a RenderInstruction
--   Makes it possible to render the RenderInstruction primitives
instance GlRender RenderInstruction where
    glRender renderInstruction = case renderInstruction of
        RenderNothing                     -> return ()
        RenderWithCamera w h sX sY instrs -> glRender $ RenderPreserve $ RenderMany ([RenderTranslate w h, RenderScale sX sY] ++ [instrs])
        RenderText text                   -> do currentRasterPosition $= Vertex4 0 0 0 1
                                                renderString TimesRoman24 text

        RenderPoints shape                -> do renderPrimitive Points $ mapM_ vertex shape
        
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
        RenderPreserve instrs             -> preservingMatrix $ glRender instrs
        RenderMany instrs                 -> mapM_ glRender instrs

--------------------------------------------------------------------------------

---TODO move definition somewhere else?
---TODO several version with different origin points

-- | Adding a default camera instruction to a given RenderInstruction
withCamera :: EngineState a -> a -> RenderInstruction -> RenderInstruction
withCamera es impl = RenderWithCamera (-1.0) (1.0) (realToFrac $ 2.0 / (fst $ getSize es impl)) (negate $ realToFrac $ 2.0 / (snd $ getSize es impl))
