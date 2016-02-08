module HGE2D.ShapeFactory where

import HGE2D.Settings
import HGE2D.Types
import HGE2D.Datas

import qualified Graphics.Rendering.OpenGL as GL

--------------------------------------------------------------------------------


---TODO rename simply to shapes
---TODO move below to math file
---TODO move these to Geometry (currently circular reference if moved there)
rad2deg :: Double -> Double
rad2deg rad = rad * 180 / pi

deg2rad :: Double -> Double
deg2rad deg = deg * pi / 180


---TODO to different file

---TODO move these elsewhere and properly define their types

borderedRectangle w h t colorInner colorBorder = RenderMany 
    [ RenderColorize colorInner
    , rectangle w h
    , RenderColorizeAlpha colorBorder
    , wireFrame w h t
    ]

colorRGB (r, g, b) = GL.color $ col3 r g b

colorRGBA (r, g, b, a) = GL.color $ col4 r g b a

addAlpha :: GL.GLfloat -> GlColorRGB -> GlColorRGBA
addAlpha a (r, g, b) = (r, g, b, a)


translate2 x y = GL.translate $ point3 x y 0.0

scale2 x y = GL.scale  x y 1.0

text str = RenderText str

rotate2 :: Double -> IO ()
rotate2 rad = rotTmp (realToFrac $ - rad2deg rad)
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


line :: GlPoint2 -> GlPoint2 -> GlThickness -> RenderInstruction
line start end w = RenderLineStrip [start, end] w

wireFrame :: GlWidth -> GlHeight -> GlThickness -> RenderInstruction
wireFrame w h t = RenderLineLoop [ll, lr, ur, ul] t
  where
    ll = point2 xMin yMin
    lr = point2 xMax yMin
    ur = point2 xMax yMax
    ul = point2 xMin yMax

    xMin = - (w/2)
    xMax =   (w/2)
    yMin = - (h/2)
    yMax =   (h/2)


rectangle :: GlWidth -> GlHeight -> RenderInstruction
rectangle w h = RenderTriangle [ll, lr, ur, ur, ul, ll]
  where
    ll = point2 xMin yMin
    lr = point2 xMax yMin
    ur = point2 xMax yMax
    ul = point2 xMin yMax

    xMin = - (w/2)
    xMax =   (w/2)
    yMin = - (h/2)
    yMax =   (h/2)

ring :: GlRadius -> GlThickness -> RenderInstruction
ring r w = RenderLineLoop (buildRing 0 []) w
  where
    buildRing :: Int -> GlShape -> GlShape
    buildRing nVertex prevShape
      | nVertex >= nFacesCircle = prevShape
      | otherwise = buildRing (nVertex+1) (prevShape ++ [vertex])
        where
          vertex = pointOnCircle nVertex

          pointOnCircle :: Int -> GlPoint2 ---TODO defined twice, move somewhere else
          pointOnCircle i = GL.Vertex2 x y
            where
              x = r * cos phi
              y = r * sin phi
              phi = 2.0 * pi * (fromIntegral i / fromIntegral nFacesCircle)


circle :: GlRadius -> RenderInstruction
circle r = RenderTriangle (buildCircle 0 [])
  where
    buildCircle :: Int -> GlShape -> GlShape
    buildCircle nFace prevShape
      | nFace >= nFacesCircle = prevShape
      | otherwise = buildCircle (nFace+1) (prevShape ++ face)
        where
          face   = [(GL.Vertex2 0 0), first, second]
          first  = pointOnCircle nFace
          second = pointOnCircle (nFace+1)

          pointOnCircle :: Int -> GlPoint2
          pointOnCircle i = GL.Vertex2 x y
            where
              x = r * cos phi
              y = r * sin phi
              phi = 2.0 * pi * (fromIntegral i / fromIntegral nFacesCircle)
