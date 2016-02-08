module HGE2D.Collision where

import HGE2D.Datas
import HGE2D.Classes

--------------------------------------------------------------------------------

doCollide :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
doCollide hasBB1 hasBB2 = not $    (bb2Left   > bb1Right)
                                || (bb2Right  < bb1Left)
                                || (bb2Top    > bb1Bottom)
                                || (bb2Bottom < bb1Top)
  where
    bb1Top    = realY $ bbMin bb1
    bb1Right  = realX $ bbMax bb1
    bb1Bottom = realY $ bbMax bb1
    bb1Left   = realX $ bbMin bb1

    bb2Top    = realY $ bbMin bb2
    bb2Right  = realX $ bbMax bb2
    bb2Bottom = realY $ bbMax bb2
    bb2Left   = realX $ bbMin bb2

    bb1 = getBB hasBB1
    bb2 = getBB hasBB2

--------------------------------------------------------------------------------

doOverlap :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
doOverlap hasBB1 hasBB2 =   (isInside hasBB1 hasBB2) 
                         || (isInside hasBB2 hasBB1)

--------------------------------------------------------------------------------

isInside :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
isInside hasBBIn hasBBOut =    (bbInTop    > bbOutTop)
                            && (bbInBottom < bbOutBottom)
                            && (bbInLeft   > bbOutLeft)
                            && (bbInRight  < bbOutRight)
  where
      bbInTop     = realY $ bbMin bbIn
      bbInRight   = realX $ bbMax bbIn
      bbInBottom  = realY $ bbMax bbIn
      bbInLeft    = realX $ bbMin bbIn

      bbOutTop    = realY $ bbMin bbOut
      bbOutRight  = realX $ bbMax bbOut
      bbOutBottom = realY $ bbMax bbOut
      bbOutLeft   = realX $ bbMin bbOut

      bbIn        = getBB hasBBIn
      bbOut       = getBB hasBBOut

--------------------------------------------------------------------------------

isInsideRP :: (HasBoundingBox a) => RealPosition -> a -> Bool
isInsideRP pos hasBB =  (posX > bbLeft)
                     && (posX < bbRight)
                     && (posY > bbTop)
                     && (posY < bbBottom)
  where
    posX     = realX pos
    posY     = realY pos
    bbTop    = realY $ bbMin bb
    bbRight  = realX $ bbMax bb
    bbBottom = realY $ bbMax bb
    bbLeft   = realX $ bbMin bb
    bb       = getBB hasBB
