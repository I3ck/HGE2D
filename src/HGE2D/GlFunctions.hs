module HGE2D.GlFunctions where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Geometry

import qualified Graphics.Rendering.OpenGL as GL

colorRGB (r, g, b) = GL.color $ col3 r g b

colorRGBA (r, g, b, a) = GL.color $ col4 r g b a

addAlpha :: GL.GLfloat -> GlColorRGB -> GlColorRGBA
addAlpha a (r, g, b) = (r, g, b, a)


translate2 x y = GL.translate $ point3 x y 0.0

scale2 x y = GL.scale x y 1.0

text str = RenderText str

rotate2 :: Double -> IO ()
rotate2 rad = rotTmp (realToFrac $ rad2deg rad)
  where
    rotTmp :: GL.GLfloat -> IO ()
    rotTmp deg = GL.rotate deg $ GL.Vector3 0 0 1

point2 :: GlPosX -> GlPosY -> GlPoint2
point2 f1 f2 = GL.Vertex2 f1 f2

vertex3 :: GlPosX -> GlPosY -> GlPosZ -> GlVer3
vertex3 f1 f2 f3 = GL.Vertex3 f1 f2 f3

col3 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color3 GL.GLfloat
col3 f1 f2 f3 = GL.Color3 f1 f2 f3

col4 :: GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.GLfloat -> GL.Color4 GL.GLfloat
col4 f1 f2 f3 f4 = GL.Color4 f1 f2 f3 f4

point3 :: GlPosX -> GlPosY -> GlPosZ -> GlPoint3
point3 f1 f2 f3 = GL.Vector3 f1 f2 f3


