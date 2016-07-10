-- |
-- Module      :  HGE2D.Collision
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing functions for collision detection

module HGE2D.Collision where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes

--------------------------------------------------------------------------------

-- | Tests whether two objects collide (overlap in any way)
doCollide :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
doCollide hasBB1 hasBB2 = 2.0 * xcenterthis - xcenterother < (xsizethis + xsizeother)
                       && 2.0 * ycenterthis - ycenterother < (ysizethis + ysizeother)
    where
      (bb1, bb2)                    = (getBB hasBB1, getBB hasBB2)
      (minthis, maxthis)            = (bbMin bb1, bbMax bb1)
      (minother, maxother)          = (bbMin bb2, bbMax bb2)
      (xsizethis, ysizethis)        = (abs $ (fst minthis) - (fst maxthis),    abs $ (snd minthis) - (snd maxthis))
      (xsizeother, ysizeother)      = (abs $ (fst minother) - (fst maxother),    abs $ (snd minother) - (snd maxother))
      (xcenterthis, ycenterthis)    = ((fst minthis) + (fst maxthis) / 2.0,    (snd minthis) + (snd maxthis) / 2.0)
      (xcenterother, ycenterother)  = ((fst minother) + (fst maxother) / 2.0,    (snd minother) + (snd maxother) / 2.0)

--------------------------------------------------------------------------------

-- | Tests whether either of the two objects fully contain the other
doContain :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
doContain hasBB1 hasBB2 =   (isInside hasBB1 hasBB2)
                         || (isInside hasBB2 hasBB1)

--------------------------------------------------------------------------------

-- | Tests whether the first is fully in the second
isInside :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
isInside hasBBIn hasBBOut =  (fst minIn) > (fst minOut)
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
isInsideRP pos hasBB =  (posX > bbLeft)
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
