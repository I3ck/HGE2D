module HGE2D.Types where

import qualified Graphics.Rendering.OpenGL as GL

--------------------------------------------------------------------------------

type Second         = Int
type Millisecond    = Int

--------------------------------------------------------------------------------

type Degree         = Double
type Drag           = Double
type Mass           = Double
type Meter          = Double
type MeterPmsec     = Double
type MeterPmsecSqr  = Double
type Pixel          = Double
type Radian         = Double
type RotationAcceleration = Radian
type RotationSpeed  = Radian

--------------------------------------------------------------------------------

type Width  = Double
type Height = Double
type PosX   = Double
type PosY   = Double

--------------------------------------------------------------------------------

type Acceleration   = (MeterPmsecSqr, MeterPmsecSqr)
type RealPosition   = (Meter, Meter)
type Velocity       = (MeterPmsec, MeterPmsec)

--------------------------------------------------------------------------------

type GlPoint2    = GL.Vertex2 GL.GLfloat
type GlPoint3    = GL.Vector3 GL.GLfloat
type GlVer3      = GL.Vertex3 GL.GLfloat
type GlPosX      = GL.GLfloat
type GlPosY      = GL.GLfloat
type GlPosZ      = GL.GLfloat
type GlWidth     = GL.GLfloat
type GlHeight    = GL.GLfloat
type GlScaleX    = GL.GLfloat
type GlScaleY    = GL.GLfloat
type GlThickness = GL.GLfloat
type GlRadius    = GL.GLfloat
type GlColorRGB  = (GL.GLfloat, GL.GLfloat, GL.GLfloat)
type GlColorRGBA = (GL.GLfloat, GL.GLfloat, GL.GLfloat, GL.GLfloat)
type GlShape     = [GlPoint2]
