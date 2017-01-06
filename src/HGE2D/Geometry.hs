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
import Data.Function (on)

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
radRealPos (p1x, p1y) (p2x, p2y) = atan2 dY dX
  where
    dX = p2x - p1x
    dY = p2y - p1y


-- | Calculate angle of a velocity
velAngle :: Velocity -> Radian
velAngle = uncurry atan2

-- | Distance between two positions
distance :: (Positioned a, Positioned b) => a -> b -> Double
distance x y = sqrt $ distanceSqr x y

-- | Squared distance between two positions
--   Faster than calculating the distance. Can be used to e.g. compare distances cheaply
distanceSqr :: (Positioned a, Positioned b) => a -> b -> Double
distanceSqr x y = (p1x - p2x)**2 + (p1y - p2y)**2
  where
    (p1x, p1y) = getPos x
    (p2x, p2y) = getPos y

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
      (xBB, yBB) = centerBB $ getBB bb
      (w, h)     = sizeBB   $ getBB bb

-- | Calculate the direction vector between two positions
direction :: (Positioned a, Positioned b) => a -> b -> RealPosition
direction x y   = (dirX, dirY)
  where
    dirX        = (p2x - p1x) / l
    dirY        = (p2y - p1y) / l
    l           = distance x y
    (p1x, p1y)  = getPos x
    (p2x, p2y)  = getPos y

-- Gives the Order of 2 elements based on their distance to a third element
-- {Hidden function: reduce redundancy}
distanceSqrComparison :: (Positioned a, Positioned b) => a -> b -> b -> Ordering
distanceSqrComparison a = compare `on` distanceSqr a

-- | Find the closest in [b] to a
closest :: (Positioned a, Positioned b) => a -> [b] -> Maybe b
closest a = minimumByMay (distanceSqrComparison a)

-- | Find the furthest in [b] to a
furthest :: (Positioned a, Positioned b) => a -> [b] -> Maybe b
furthest a = maximumByMay (distanceSqrComparison a)

-- | Given a position and projectile speed of a gun / turret and an object defined by its current position and velocity
--   Calculates the position where both will intercept. (useful for pre-aiming)
interceptionPos :: (RealPosition, Double) -> (RealPosition, Velocity) -> RealPosition
interceptionPos (p1, v) (p2, v2) = (newX, newY)
  where
    tx  = fst p2 - fst p1
    ty  = snd p2 - snd p1
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

    newX = fst p2 + fst v2 * t
    newY = snd p2 + snd v2 * t

-- | Builder for a rigidbody
makeRB :: RealPosition -> Velocity -> Pixel -> Pixel -> RigidBody
makeRB center vel width height = RigidBody { rigidPos = center, rigidVel = vel, rigidBB = sizedBB center width height }

-- | Builder to get a BoundingBox from its center position and sizes
sizedBB :: RealPosition -> Pixel -> Pixel -> BoundingBox
sizedBB (cX, cY) w h = BB posMin posMax
  where
    posMin  = (minX, minY)
    posMax  = (maxX, maxY)
    minX    = cX - w / 2
    minY    = cY - h / 2
    maxX    = cX + w / 2
    maxY    = cY + h / 2

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
bbFromList []  = BBEmpty
bbFromList [_] = BBEmpty
bbFromList xs  = BB (minX, minY) (maxX, maxY)
  where
    minX = minimum xxs
    minY = minimum yxs
    maxX = maximum xxs
    maxY = maximum yxs
    xxs  = fmap getX xs
    yxs  = fmap getY xs

-- | Merges two bounding boxes, creating a new one which wraps around the inputs
--   In case a nullBB is passed as one parameter, the other BoundingBox is returned
mergeBB :: BoundingBox -> BoundingBox -> BoundingBox
mergeBB BBEmpty bb2 = bb2
mergeBB bb1 BBEmpty = bb1
mergeBB bb1 bb2     = BB newMin newMax
  where
    newMin = mergeMin poss
    newMax = mergeMax poss
    poss = [bbMin bb1, bbMin bb2, bbMax bb1, bbMax bb2]

    mergeMin :: [RealPosition] -> RealPosition
    mergeMin poss = (x, y)
      where
       x = fst $ minimumBy compareX poss
       y = snd $ minimumBy compareY poss

    mergeMax :: [RealPosition] -> RealPosition
    mergeMax poss = (x, y)
      where
       x = fst $ maximumBy compareX poss
       y = snd $ maximumBy compareY poss

    compareX (ax, _) (bx, _) = compare ax bx
    compareY (_, ay) (_, by) = compare ay by

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
makeBB = sizedBB

-- | Given a position, time and veilocty it calculates the position where the moving object would be
applyVelocity :: RealPosition -> Velocity -> Millisecond -> RealPosition
applyVelocity (oldPosX, oldPosY) (velX, velY) time =
    (oldPosX + fromIntegral time * velX,
     oldPosY + fromIntegral time * velY)
