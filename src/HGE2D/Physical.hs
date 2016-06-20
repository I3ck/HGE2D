module HGE2D.Physical where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes

--------------------------------------------------------------------------------

---TODO not fully implemented
applyPhysics :: (IsPhysicalObject a) => Millisecond -> a -> a
applyPhysics ms x = setPhys newPo x
    where
      newPo = (applyVel . applyAcc) (getPhys x)

      applyVel po = po { physicalPos = (RealPosition sX sY)}
        where
          sX = (realX $ physicalPos po) + (fromIntegral ms) * (velX $ physicalVel po)
          sY = (realY $ physicalPos po) + (fromIntegral ms) * (velY $ physicalVel po)

      applyAcc po = po { physicalVel = (Velocity vX vY) }
        where
          vX = (velX $ physicalVel po) + (fromIntegral ms) * (accX $ physicalAcc po)
          vY = (velY $ physicalVel po) + (fromIntegral ms) * (accY $ physicalAcc po)
