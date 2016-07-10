-- |
-- Module      :  HGE2D.GlFunctions
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing functions used to interact with OpenGL

module HGE2D.GlFunctions where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Geometry

import qualified Graphics.Rendering.OpenGL as GL

--------------------------------------------------------------------------------

-- | Renders a RGB color
colorRGB :: GlColorRGB -> IO ()
colorRGB (r, g, b) = GL.color $ col3 r g b
  where
    col3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color3 GL.GLfloat
    col3 f1 f2 f3 = GL.Color3 f1 f2 f3

-- | Renders a RGBA color
colorRGBA :: GlColorRGBA -> IO ()
colorRGBA (r, g, b, a) = GL.color $ col4 r g b a
  where
    col4 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color4 GL.GLfloat
    col4 f1 f2 f3 f4 = GL.Color4 f1 f2 f3 f4

-- | Adds an alpha value to a RGB color, turning it into a RGBA color
addAlpha :: GL.GLfloat -> GlColorRGB -> GlColorRGBA
addAlpha a (r, g, b) = (r, g, b, a)

--------------------------------------------------------------------------------

-- | Sends a translation instruction to OpenGL
translate2 :: GlPosX -> GlPosY -> IO ()
translate2 x y = GL.translate $ point3 x y 0.0

-- | Sends a scale instruction to OpenGL
scale2 :: GlScaleX -> GlScaleY -> IO ()
scale2 x y = GL.scale x y 1.0

--------------------------------------------------------------------------------

-- | Transforms a string to a RenderInstruction
text :: String -> RenderInstruction
text str = RenderText str

--------------------------------------------------------------------------------

-- | Sends a rotation instruction to OpenGL
rotate2 :: Double -> IO ()
rotate2 rad = rotTmp (realToFrac $ rad2deg rad)
  where
    rotTmp :: GL.GLfloat -> IO ()
    rotTmp deg = GL.rotate deg $ GL.Vector3 0 0 1

--------------------------------------------------------------------------------

-- | Builds a GLPoint2
point2 :: GlPosX -> GlPosY -> GlPoint2
point2 f1 f2 = GL.Vertex2 f1 f2

-- | Builds a GLPoint3
point3 :: GlPosX -> GlPosY -> GlPosZ -> GlPoint3
point3 f1 f2 f3 = GL.Vector3 f1 f2 f3

-- | Builds a GLVer3
vertex3 :: GlPosX -> GlPosY -> GlPosZ -> GlVer3
vertex3 f1 f2 f3 = GL.Vertex3 f1 f2 f3
