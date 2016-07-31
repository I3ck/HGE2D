-- |
-- Module      :  HGE2D.Collision
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing functions for collision detection

module HGE2D.Collision where

import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Geometry
import HGE2D.Instances ()

--------------------------------------------------------------------------------

-- | Tests whether two objects collide (overlap in any way)
doCollide :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
doCollide hasBB1 hasBB2 | bb1 == BBEmpty = False
                        | bb2 == BBEmpty = False
                        | bb1 == bb2     = True
                        | otherwise = abs (xcenterthis - xcenterother) < 0.5 * (xsizethis + xsizeother) &&
                                      abs (ycenterthis - ycenterother) < 0.5 * (ysizethis + ysizeother)
    where
      (bb1, bb2)                    = (getBB hasBB1, getBB hasBB2)
      (xsizethis, ysizethis)        = (fst $ sizeBB bb1, snd $ sizeBB bb1)
      (xsizeother, ysizeother)      = (fst $ sizeBB bb2, snd $ sizeBB bb2)
      (xcenterthis, ycenterthis)    = (getX bb1, getY bb1)
      (xcenterother, ycenterother)  = (getX bb2, getY bb2)

--------------------------------------------------------------------------------

-- | Tests whether either of the two objects fully contain the other
doContain :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
doContain hasBB1 hasBB2 =   (isInside hasBB1 hasBB2)
                         || (isInside hasBB2 hasBB1)

--------------------------------------------------------------------------------

-- | Tests whether the first is fully in the second
isInside :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
isInside hasBBIn hasBBOut | bbIn == BBEmpty     = False
                          | bbOut == BBEmpty    = False
                          | bbIn == bbOut       = True
                          | otherwise           =   (fst minIn) > (fst minOut)
                                                 && (snd minIn) > (snd minOut)
                                                 && (fst maxIn) < (fst maxOut)
                                                 && (snd maxIn) < (snd maxOut)
  where
    (bbIn, bbOut)       = (getBB hasBBIn, getBB hasBBOut)
    (minIn, maxIn)      = (bbMin bbIn, bbMax bbIn)
    (minOut, maxOut)    = (bbMin bbOut, bbMax bbOut)

--------------------------------------------------------------------------------

-- | Tests whether a position is within the bounding box
isInsideRP :: (Positioned a, HasBoundingBox b) => a -> b -> Bool
isInsideRP pos hasBB | bb == BBEmpty = False
                     | otherwise     =    (posX > bbLeft)
                                       && (posX < bbRight)
                                       && (posY > bbTop)
                                       && (posY < bbBottom)
  where
    posX     = fst $ getPos pos
    posY     = snd $ getPos pos
    bbTop    = snd $ bbMin bb
    bbRight  = fst $ bbMax bb
    bbBottom = snd $ bbMax bb
    bbLeft   = fst $ bbMin bb
    bb       = getBB hasBB
