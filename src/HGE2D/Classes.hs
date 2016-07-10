-- |
-- Module      :  HGE2D.Classes
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing class definitions used within HGE2D

{-# LANGUAGE ConstraintKinds #-}

module HGE2D.Classes where

import HGE2D.Types
import HGE2D.Datas

--------------------------------------------------------------------------------

-- | For types which are affected by time
class Dynamic a where
    moveInTime :: Millisecond -> a -> a

--------------------------------------------------------------------------------

-- | For types than can be directly rendered to GL (only required by the engine)
--   Use GlInstructable to define your rendereble types
class GlRender a where
    glRender :: a -> IO ()

--------------------------------------------------------------------------------

-- | For types which can be turned into render instructions to be rendered by the engine
class GlInstructable a where
    toGlInstruction :: a -> RenderInstruction

--------------------------------------------------------------------------------

-- | For types which have a bounding box
class HasBoundingBox a where
    getBB :: a -> BoundingBox

--------------------------------------------------------------------------------

-- | For types which have / are a physical object
class IsPhysicalObject a where
    getPhys :: a -> PhysicalObject
    setPhys :: PhysicalObject -> a -> a

--------------------------------------------------------------------------------

-- | For types which are positioned in space
class Positioned a where
    getPos :: a -> RealPosition
    getX   :: a -> Double
    getY   :: a -> Double

--------------------------------------------------------------------------------

-- | For types which can be moved in space
class Moveable a where
    moveBy :: RealPosition -> a -> a
    moveTo :: RealPosition -> a -> a

--------------------------------------------------------------------------------

-- | For types which can be accelerated
class Acceleratable a where
    accBy :: Velocity -> a -> a
    accTo :: Velocity -> a -> a
