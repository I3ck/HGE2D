-- |
-- Module      :  HGE2D.Instances
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing instance definitions for the classes / types of HGE2D

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module HGE2D.Instances where

import HGE2D.Geometry
import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes

--------------------------------------------------------------------------------

-- | Instance of Monoid for BoundingBox
instance Monoid BoundingBox where
    mempty = BBEmpty
    mappend = mergeBB

--------------------------------------------------------------------------------

-- | Instance of HasBoundingBox for BoundingBox
instance HasBoundingBox BoundingBox where
    getBB = id

-- | Instance of HasBoundingBox for RigidBody
instance HasBoundingBox RigidBody where
    getBB = rigidBB

-- | Instance of HasBoundingBox for PhysicalObject
instance HasBoundingBox PhysicalObject where
    getBB = physicalBB

--------------------------------------------------------------------------------

-- | Instance of Positioned for RealPosition
instance Positioned RealPosition where
    getPos = id
    getX = fst
    getY = snd

-- | Instance of Positioned for RigidBody
instance Positioned RigidBody where
    getPos = getPos . rigidPos
    getX = getX . rigidPos
    getY = getY . rigidPos

-- | Instance of Positioned for BoundingBox
instance Positioned BoundingBox where
    getPos bb = (x, y)
      where
        x = 0.5 * ((fst $ bbMin bb) + (fst $ bbMax bb))
        y = 0.5 * ((snd $ bbMin bb) + (snd $ bbMax bb))
    getX = fst . getPos
    getY = snd . getPos

--------------------------------------------------------------------------------

-- | Instance of Moveable for RealPosition
instance Moveable RealPosition where
    moveBy by pos = (newX, newY)
      where
        newX = fst pos + fst by
        newY = snd pos + snd by
    moveTo to _ = ((fst to), (snd to))

-- | Instance of Moveable for BoundingBox
instance Moveable BoundingBox where
    moveBy by bb = BB { bbMin = newMinPos, bbMax = newMaxPos }
      where
        newMinPos = moveBy by $ bbMin bb
        newMaxPos = moveBy by $ bbMax bb

    moveTo by bb = BB { bbMin = newMinPos, bbMax = newMaxPos }
      where
        newMinPos = moveTo by $ bbMin bb
        newMaxPos = moveTo by $ bbMax bb

-- | Instance of Moveable for RigidBody
instance Moveable RigidBody where
    moveBy by rb = rb { rigidPos = newPos , rigidBB = newBB }
      where
        newBB = moveBy by (rigidBB rb)
        newPos = moveBy by (rigidPos rb)
    moveTo to rb = rb { rigidPos = newPos, rigidBB = newBB }
      where
        newBB = moveTo to (rigidBB rb)
        newPos = moveTo to $ rigidPos rb

-- | Instance of Moveable for PhysicalObject
instance Moveable PhysicalObject where
    moveBy by po = po { physicalPos = newPos, physicalBB = newBB }
      where
        newBB = moveBy by (physicalBB po)
        newPos = moveBy by (physicalPos po)
    moveTo to po = po { physicalPos = newPos, physicalBB = newBB }
      where
        newBB = moveTo to (physicalBB po)
        newPos = moveTo to (physicalPos po)

--------------------------------------------------------------------------------

-- | Instance of Acceleratable for RigidBody
instance Acceleratable RigidBody where
    accBy by rb = rb { rigidVel = newVel }
      where
        newVel = ((fst oldVel) + (fst by) , (snd oldVel) + (snd by))
        oldVel = rigidVel rb
    accTo to rb = rb { rigidVel = to }

-- | Instance of Acceleratable for PhysicalObject
instance Acceleratable PhysicalObject where
    accBy by po = po { physicalVel = newVel }
      where
        newVel = ((fst oldVel) + (fst by) , (snd oldVel) + (snd by))
        oldVel = physicalVel po
    accTo to po = po { physicalVel = to }

--------------------------------------------------------------------------------

-- | Instance of Dynamic for RigidBody
instance Dynamic RigidBody where
    moveInTime time rb = rb { rigidPos = newPos , rigidBB = newBB }
      where
        newBB = (rigidBB rb) { bbMin = newMinPos, bbMax = newMaxPos}
        newPos = applyVelocity (rigidPos rb) (rigidVel rb) time
        newMinPos = applyVelocity (bbMin $ rigidBB rb) (rigidVel rb) time
        newMaxPos = applyVelocity (bbMax $ rigidBB rb) (rigidVel rb) time
