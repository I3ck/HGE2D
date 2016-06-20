{-# LANGUAGE ConstraintKinds #-}
module HGE2D.Classes where

import HGE2D.Types
import HGE2D.Datas

--------------------------------------------------------------------------------

class Dynamic a where
    moveInTime :: Millisecond -> a -> a

--------------------------------------------------------------------------------

class GlRender a where
    glRender :: a -> IO ()

--------------------------------------------------------------------------------

class GlInstructable a where
    toGlInstruction :: a -> RenderInstruction

--------------------------------------------------------------------------------

class HasBoundingBox a where
    getBB :: a -> BoundingBox

--------------------------------------------------------------------------------

class IsPhysicalObject a where
    getPhys :: a -> PhysicalObject
    setPhys :: PhysicalObject -> a -> a

--------------------------------------------------------------------------------

class Positioned a where
    getPos :: a -> RealPosition
    getX   :: a -> Double
    getY   :: a -> Double

--------------------------------------------------------------------------------

class Moveable a where
    moveBy :: RealPosition -> a -> a
    moveTo :: RealPosition -> a -> a

--------------------------------------------------------------------------------

class Acceleratable a where
    accBy :: Velocity -> a -> a
    accTo :: Velocity -> a -> a
