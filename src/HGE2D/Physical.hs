-- |
-- Module      :  HGE2D.Physical
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing functions for PhysicalObjects

module HGE2D.Physical where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Instances ()

--------------------------------------------------------------------------------

---TODO not fully implemented
---TODO rename
---TODO rename class to HasPhysicalObject

-- | Applies physics to a physical object. E.g. velocity changing position, acceleration changing velocity ...
applyPhysics :: (IsPhysicalObject a) => Millisecond -> a -> a
applyPhysics ms x = setPhys newPo x
  where
    newPo = (applyVel . applyAcc . applyRotVel . applyRotAcc . applyDrag . applyRotDrag) (getPhys x) ---TODO more consistent order acc > vel > abs

    applyVel po = moveBy (dX, dY) po ---TODO define more general for reuse
      where
        dX = (fromIntegral ms) * (fst $ physicalVel po)
        dY = (fromIntegral ms) * (snd $ physicalVel po)

    applyAcc po = po { physicalVel = (vX, vY) } ---TODO see above
      where
        vX = (fst $ physicalVel po) + (fromIntegral ms) * (fst $ physicalAcc po)
        vY = (snd $ physicalVel po) + (fromIntegral ms) * (snd $ physicalAcc po)

    applyRotVel po = po { physicalRot = fixedRot } ---TODO see above
      where
        fixedRot | newRot > 2.0 * pi = newRot - 2.0 * pi
                 | newRot < 0        = newRot + 2.0 * pi
                 | otherwise         = newRot
        newRot = (physicalRot po)  + dRot
        dRot = (fromIntegral ms) * (physicalRotSpeed po)

    applyRotAcc po = po { physicalRotSpeed = newRotSpeed } ---TODO see above
      where
        newRotSpeed = (physicalRotAcceleration po) + dRotSpeed
        dRotSpeed = (fromIntegral ms) * (physicalRotAcceleration po)

    applyDrag = id ---TODO
    applyRotDrag = id ---TODO
