module HGE2D.Geometry where

import HGE2D.Math
import HGE2D.Types
import HGE2D.Datas

---TODO rewrite most / all functions to use classes

radRealPos :: RealPosition -> RealPosition -> Radian
radRealPos p1 p2 = atan2 dY dX
  where
    dX = (realX p2) - (realX p1)
    dY = (realY p2) - (realY p1)


velAngle :: Velocity -> Radian
velAngle v = atan2 (velY v) (velX v)


distance :: RealPosition -> RealPosition -> Double
distance v1 v2 = sqrt $ (realX v1 - realX v2)**2 + (realY v1 - realY v2)**2

direction :: RealPosition -> RealPosition -> RealPosition --- TODO define different types?
direction pos1 pos2 = RealPosition newX newY
  where
    newX = ((realX pos2) - (realX pos1)) / l
    newY = ((realY pos2) - (realY pos1)) / l
    l = distance pos1 pos2


interceptionPos :: (RealPosition, Double) -> (RealPosition, Velocity) -> RealPosition
interceptionPos (p1, v) (p2, v2) = RealPosition newX newY
  where
    tx = (realX p2) - (realX p1)
    ty = (realY p2) - (realY p1)
    tvx = velX v2
    tvy = velY v2

    a = tvx*tvx + tvy*tvy - v*v :: Double
    b = 2 * (tvx * tx + tvy * ty) :: Double
    c = tx*tx + ty*ty :: Double

    ts = quadraticEquation a b c
    t0 = fst ts
    t1 = snd ts
    temp = min t0 t1
    t | temp > 0 = temp
      | otherwise = max t0 t1

    newX = (realX p2) + (velX v2) * t
    newY = (realY p2) + (velY v2) * t

makeRB :: RealPosition -> Velocity -> Pixel -> Pixel -> RigidBody
makeRB center vel width height = RigidBody { rigidPos = center, rigidVel = vel, rigidBB = sizedBB center width height }

sizedBB :: RealPosition -> Pixel -> Pixel -> BoundingBox
sizedBB center width height = BoundingBox posMin posMax
  where
    posMin = RealPosition minX minY
    posMax = RealPosition maxX maxY
    minX = (realX center) - width / 2
    minY = (realY center) - height / 2
    maxX = (realX center) + width / 2
    maxY = (realY center) + height / 2

sizeBB :: BoundingBox -> (Pixel, Pixel)
sizeBB bb = (width, height)
  where
    width  = (realX $ bbMax bb) - (realX $ bbMin bb)
    height = (realY $ bbMax bb) - (realY $ bbMin bb)

centerBB :: BoundingBox -> RealPosition
centerBB bb = RealPosition newX newY
  where
    newX = (realX $ bbMin bb) + (width / 2)
    newY = (realY $ bbMin bb) + (height / 2)
    (width, height) = sizeBB bb

mergeBB :: BoundingBox -> BoundingBox -> BoundingBox
mergeBB bb1 bb2 = BoundingBox newMin newMax
  where
    newMin = mergeMin (bbMin bb1) (bbMin bb2)
    newMax = mergeMax (bbMax bb1) (bbMax bb2)

    mergeMin :: RealPosition -> RealPosition -> RealPosition
    mergeMin pos1 pos2 = RealPosition x y
      where
       x = min (realX pos1) (realX pos2)
       y = min (realY pos1) (realY pos2)

    mergeMax :: RealPosition -> RealPosition -> RealPosition
    mergeMax pos1 pos2 = RealPosition x y
      where
       x = max (realX pos1) (realX pos2)
       y = max (realY pos1) (realY pos2)

{- see above
tilePosToBB :: TilePosition -> BoundingBox
tilePosToBB pos = BoundingBox minPos maxPos
  where
    minPos = toRealPos $ pos
    maxPos = RealPosition maxX maxY
    maxX = (realX minPos) + tileSize
    maxY = (realY minPos) + tileSize
-}


makeBB :: RealPosition -> Pixel -> Pixel -> BoundingBox
makeBB center width height = BoundingBox newMin newMax
  where
    newMin = RealPosition (realX center - width / 2) (realY center - height / 2)
    newMax = RealPosition (realX center + width / 2) (realY center + height / 2)
