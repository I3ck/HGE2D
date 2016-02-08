module HGE2D.Instances where

import HGE2D.Datas
import HGE2D.Classes

--------------------------------------------------------------------------------

instance HasBoundingBox BoundingBox where
    getBB = id

instance HasBoundingBox RigidBody where
    getBB = rigidBB

--------------------------------------------------------------------------------

instance Positioned RealPosition where
    getPos = id
    getX = realX
    getY = realY

instance Positioned RigidBody where
    getPos = getPos . rigidPos
    getX = getX . rigidPos
    getY = getY . rigidPos

--------------------------------------------------------------------------------

instance Moveable RealPosition where
    moveBy by pos = RealPosition newX newY
      where
        newX = realX pos + realX by
        newY = realY pos + realY by
    moveTo to _ = RealPosition (realX to) (realY to)

instance Moveable RigidBody where
    moveBy by rb = rb { rigidPos = newPos , rigidBB = newBB }
      where
        newBB = (rigidBB rb) { bbMin = newMinPos, bbMax = newMaxPos}
        newPos = moveBy by (rigidPos rb)
        newMinPos = moveBy by $ bbMin $ rigidBB rb
        newMaxPos = moveBy by $ bbMax $ rigidBB rb
    moveTo to rb = rb { rigidPos = newPos, rigidBB = newBB }
      where
        newBB = (rigidBB rb) { bbMin = newMinPos, bbMax = newMaxPos}
        newPos = moveTo to $ rigidPos rb
        newMinPos = moveBy by $ bbMin $ rigidBB rb
        newMaxPos = moveBy by $ bbMax $ rigidBB rb
        by = RealPosition (realX newPos - getX rb) (realY newPos - getY rb)
