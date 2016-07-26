-- |
-- Module      :  HGE2D.Geometry
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing geometrical functions

module HGE2D.Geometry where

import Data.List

import HGE2D.Math
import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes

import Safe

---TODO rewrite most / all functions to use classes
---TODO use typedefs

-- | Transform an angle in radians to an angle in degrees
rad2deg :: Double -> Double
rad2deg rad = rad * 180 / pi

-- | Transform an angle in degrees to an angle in radians
deg2rad :: Double -> Double
deg2rad deg = deg * pi / 180

-- | Calculate angle in radians between two positions in space, from the first to the secind
radRealPos :: RealPosition -> RealPosition -> Radian
radRealPos p1 p2 = atan2 dY dX
  where
    dX = (fst p2) - (fst p1)
    dY = (snd p2) - (snd p1)


-- | Calculate angle of a velocity
velAngle :: Velocity -> Radian
velAngle v = atan2 (fst v) (snd v)

-- | Distance between two positions
distance :: (Positioned a, Positioned b) => a -> b -> Double
distance x y = sqrt $ distanceSqr x y

-- | Squared distance between two positions
--   Faster than calculating the distance. Can be used to e.g. compare distances cheaply
distanceSqr :: (Positioned a, Positioned b) => a -> b -> Double
distanceSqr x y = (fst p1 - fst p2)**2 + (snd p1 - snd p2)**2
  where
    p1 = getPos x
    p2 = getPos y

-- | Distance between a position and a bounding box
distanceBB :: (Positioned a, HasBoundingBox b) => a -> b -> Double
distanceBB p bb = sqrt $ distanceBBSqr p bb

-- | Squared distance between a position and a bounding box
--   Faster than calculating the distance. Can be used to e.g. compare distances cheaply
distanceBBSqr :: (Positioned a, HasBoundingBox b) => a -> b -> Double
distanceBBSqr p bb = dx * dx + dy * dy
  where
      dx = max 0 $ abs (xP - xBB) - w / 2.0
      dy = max 0 $ abs (yP - yBB) - h / 2.0
      xP = getX p
      yP = getY p
      xBB = fst $ centerBB $ getBB bb
      yBB = snd $ centerBB $ getBB bb
      (w, h) = sizeBB $ getBB bb

-- | Calculate the direction vector between two positions
direction :: (Positioned a, Positioned b) => a -> b -> RealPosition
direction x y = (newX, newY)
  where
    newX = ((fst p2) - (fst p1)) / l
    newY = ((snd p2) - (snd p1)) / l
    l = distance x y
    p1 = getPos x
    p2 = getPos y

-- | Find the closest in [b] to a
closest :: (Positioned a, Positioned b) => a -> [b] -> Maybe b
closest a bs = minimumByMay (  \ x y -> compare (distanceSqr a x) (distanceSqr a y)  ) bs

-- | Find the furthest in [b] to a
furthest :: (Positioned a, Positioned b) => a -> [b] -> Maybe b
furthest a bs = minimumByMay (  \ x y -> compare (distanceSqr a x) (distanceSqr a y)  ) bs

-- | Given a position and projectile speed of a gun / turret and an object defined by its current position and velocity
--   Calculates the position where both will intercept. (useful for pre-aiming)
interceptionPos :: (RealPosition, Double) -> (RealPosition, Velocity) -> RealPosition
interceptionPos (p1, v) (p2, v2) = (newX, newY)
  where
    tx = (fst p2) - (fst p1)
    ty = (snd p2) - (snd p1)
    tvx = fst v2
    tvy = snd v2

    a = tvx*tvx + tvy*tvy - v*v :: Double
    b = 2 * (tvx * tx + tvy * ty) :: Double
    c = tx*tx + ty*ty :: Double

    ts = quadraticEquation a b c
    t0 = fst ts
    t1 = snd ts
    temp = min t0 t1
    t | temp > 0 = temp
      | otherwise = max t0 t1

    newX = (fst p2) + (fst v2) * t
    newY = (snd p2) + (snd v2) * t

-- | Builder for a rigidbody
makeRB :: RealPosition -> Velocity -> Pixel -> Pixel -> RigidBody
makeRB center vel width height = RigidBody { rigidPos = center, rigidVel = vel, rigidBB = sizedBB center width height }

-- | Builder to get a BoundingBox from its center position and sizes
sizedBB :: RealPosition -> Pixel -> Pixel -> BoundingBox
sizedBB center width height = BoundingBox posMin posMax
  where
    posMin = (minX, minY)
    posMax = (maxX, maxY)
    minX = (fst center) - width / 2
    minY = (snd center) - height / 2
    maxX = (fst center) + width / 2
    maxY = (snd center) + height / 2

-- | Calculates the size of a BoundingBox
sizeBB :: BoundingBox -> (Pixel, Pixel)
sizeBB bb = (width, height)
  where
    width  = (fst $ bbMax bb) - (fst $ bbMin bb)
    height = (snd $ bbMax bb) - (snd $ bbMin bb)

-- | Calculates the center of a BoundingBox
centerBB :: BoundingBox -> RealPosition
centerBB bb = (newX, newY)
  where
    newX = (fst $ bbMin bb) + (width / 2)
    newY = (snd $ bbMin bb) + (height / 2)
    (width, height) = sizeBB bb

-- | Calculates the bounding box of multiple positions
bbFromList :: (Positioned a) => [a] -> BoundingBox
bbFromList []  = nullBB
bbFromList [x] = nullBB
bbFromList xs  = BoundingBox (minX, minY) (maxX, maxY)
  where
    minX = minimum $ map getX xs
    minY = minimum $ map getY xs
    maxX = maximum $ map getX xs
    maxY = maximum $ map getY xs

-- | Testing whether a BoundingBox has the same min and max values
isNullBB :: BoundingBox -> Bool
isNullBB bb = (bbMin bb) == (bbMax bb)

-- | A BoundingBox which counts as null, having the same min and max position
nullBB :: BoundingBox
nullBB = BoundingBox (0,0) (0,0)

-- | Merges two bounding boxes, creating a new one which wraps around the inputs
--   In case a nullBB is passed as one parameter, the other BoundingBox is returned
mergeBB :: BoundingBox -> BoundingBox -> BoundingBox
mergeBB bb1 bb2 | isNullBB bb1 = bb2
                | isNullBB bb2 = bb1
                | otherwise    = BoundingBox newMin newMax
  where
    newMin = mergeMin poss
    newMax = mergeMax poss
    poss = [bbMin bb1, bbMin bb2, bbMax bb1, bbMax bb2]

    mergeMin :: [RealPosition]-> RealPosition
    mergeMin poss = (x, y)
      where
       x = fst $ minimumBy compareX poss
       y = snd $ minimumBy compareY poss

    mergeMax :: [RealPosition] -> RealPosition
    mergeMax poss = (x, y)
      where
       x = fst $ maximumBy compareX poss
       y = snd $ maximumBy compareY poss

    compareX a b = compare (fst a) (fst b)
    compareY a b = compare (snd a) (snd b)

{- see above
tilePosToBB :: TilePosition -> BoundingBox
tilePosToBB pos = BoundingBox minPos maxPos
  where
    minPos = toRealPos $ pos
    maxPos = RealPosition maxX maxY
    maxX = (fst minPos) + tileSize
    maxY = (snd minPos) + tileSize
-}

---TODO sizedBB and makeBB are duplicates

-- | Builds a BoundingBox
makeBB :: RealPosition -> Pixel -> Pixel -> BoundingBox
makeBB center width height = BoundingBox newMin newMax
  where
    newMin = ((fst center - width / 2), (snd center - height / 2))
    newMax = ((fst center + width / 2), (snd center + height / 2))

-- | Given a position, time and veilocty it calculates the position where the moving object would be
applyVelocity :: RealPosition -> Velocity -> Millisecond -> RealPosition
applyVelocity oldPos vel time =
    (((fst oldPos) + (fromIntegral time) * (fst vel)),
    ((snd oldPos) + (fromIntegral time) * (snd vel)))
