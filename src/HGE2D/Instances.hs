module HGE2D.Instances where

import HGE2D.Datas
import HGE2D.Classes

--------------------------------------------------------------------------------

instance HasBoundingBox BoundingBox where
    getBB bb = bb

instance HasBoundingBox RigidBody where
    getBB rb = rigidBB rb

--------------------------------------------------------------------------------

instance Positioned RealPosition where
    getPos rp = rp
    getX rp = realX rp
    getY rp = realY rp

instance Positioned RigidBody where
    getPos rb = getPos $ rigidPos rb
    getX rb = getX $ rigidPos rb
    getY rb = getY $ rigidPos rb

--------------------------------------------------------------------------------

instance Moveable RealPosition where
    moveBy by pos = RealPosition newX newY
      where
        newX = (realX pos) + (realX by)
        newY = (realY pos) + (realY by)
    moveTo to _ = RealPosition (realX to) (realY to)

instance Moveable RigidBody where
    moveBy by rb = rb { rigidPos = newPos , rigidBB = newBB }
      where
        newBB = (rigidBB rb) { bbMin = newMinPos, bbMax = newMaxPos}
        newPos = moveBy by (rigidPos rb)
        newMinPos = moveBy by (bbMin $ rigidBB rb)
        newMaxPos = moveBy by (bbMax $ rigidBB rb)
    moveTo to rb = rb { rigidPos = newPos, rigidBB = newBB }
      where
        newBB = (rigidBB rb) { bbMin = newMinPos, bbMax = newMaxPos}
        newPos = moveTo to (rigidPos rb)
        newMinPos = moveBy by (bbMin $ rigidBB rb)
        newMaxPos = moveBy by (bbMax $ rigidBB rb)
        by = RealPosition (  (realX newPos) - (getX rb)  )   (  (realY newPos) - (getY rb)  )

