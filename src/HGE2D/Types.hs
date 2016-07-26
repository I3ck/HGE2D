-- |
-- Module      :  HGE2D.Types
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing type definitions used within HGE2D

module HGE2D.Types where

import qualified Graphics.Rendering.OpenGL as GL

--------------------------------------------------------------------------------

-- | Time in seconds
type Second         = Int

-- | Time in milliseconds
type Millisecond    = Int

--------------------------------------------------------------------------------

-- | Maximum depth of a tree
type MaxDepth = Int

--------------------------------------------------------------------------------

-- | Angle in degrees
type Degree         = Double

-- | The drag of an object
type Drag           = Double

-- | The mass of an object in kg
type Mass           = Double

-- | Distance in meters
type Meter          = Double

-- | Velocity in meters per millisecond
type MeterPmsec     = Double

-- | Acceleration in meters per millisecond squared
type MeterPmsecSqr  = Double

-- | Distance or size in pixels
type Pixel          = Double

-- | Angle in radians
type Radian         = Double

-- | Rotation acceleration in radians per millisecond squared
type RotationAcceleration = Radian

-- | Rotation speed in radians per millisecond
type RotationSpeed  = Radian

--------------------------------------------------------------------------------

-- | Width of an object
type Width  = Double

-- | Height of an object
type Height = Double

-- | Position in x-direction of an oject
type PosX   = Double

-- | Position in y-direction of an object
type PosY   = Double

--------------------------------------------------------------------------------

-- | Acceleration split in x and y component
type Acceleration   = (MeterPmsecSqr, MeterPmsecSqr)

-- | Position of an object split in x and y component
type RealPosition   = (Meter, Meter)

-- | Velocity of an object split in x and y component
type Velocity       = (MeterPmsec, MeterPmsec)

--------------------------------------------------------------------------------

-- | Point in 2D defined by GLfloat values
type GlPoint2    = GL.Vertex2 GL.GLfloat

-- | Point in 3D defined by GLfloat values
type GlPoint3    = GL.Vector3 GL.GLfloat

-- | Vertex in 3D defined by GLfloat values
type GlVer3      = GL.Vertex3 GL.GLfloat

-- | Position in x-direction defined by GLfloat value
type GlPosX      = GL.GLfloat

-- | Position in y-direction defined by GLfloat value
type GlPosY      = GL.GLfloat

-- | Position in z-direction defined by GLfloat value
type GlPosZ      = GL.GLfloat

-- | Width of an object defined by GLfloat value
type GlWidth     = GL.GLfloat

-- | Height of an object defined by GLfloat value
type GlHeight    = GL.GLfloat

-- | Scale in x-direction of an object defined by GLfloat value
type GlScaleX    = GL.GLfloat

-- | Scale in y-direction of an object defined by GLfloat value
type GlScaleY    = GL.GLfloat

-- | Thickness of e.g. a line defined by GLfloat value
type GlThickness = GL.GLfloat

-- | Radius of e.g. a circle defined by GLfloat value
type GlRadius    = GL.GLfloat

-- | RGB color defined by 3 GLfloat values
type GlColorRGB  = (GL.GLfloat, GL.GLfloat, GL.GLfloat)

-- | RGBA color defined by 4 GLfloat values
type GlColorRGBA = (GL.GLfloat, GL.GLfloat, GL.GLfloat, GL.GLfloat)

-- | A shape defined by several points in 2D space
type GlShape     = [GlPoint2]
