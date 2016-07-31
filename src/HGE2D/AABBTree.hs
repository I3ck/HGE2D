-- |
-- Module      :  HGE2D.AABBTree
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing the definition and functions for an axis aligned bounding box tree

module HGE2D.AABBTree where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Geometry
import HGE2D.Collision

import Data.Maybe

--------------------------------------------------------------------------------

-- | A bounding box tree for fast collision detection
data (HasBoundingBox a) => AABBTree a = AABBTreeEmpty
                                      | AABBTreeLeaf [a] BoundingBox
                                      | AABBTreeBranch (AABBTree a) (AABBTree a) BoundingBox

---TODO fmap
---TODO fmapRebuild

--------------------------------------------------------------------------------

-- | Builds an AABBTree from a list of elements with bounding boxes.
aaBBTreeBuild :: (HasBoundingBox a) => MaxDepth -> [a] -> AABBTree a
aaBBTreeBuild maxDepth xs = aaBBTreeBuildRec False 0 maxDepth xs
  where
    aaBBTreeBuildRec _ _ _ []                = AABBTreeEmpty
    aaBBTreeBuildRec _ _ _ [x]               = AABBTreeLeaf [x] (getBB x)
    aaBBTreeBuildRec compX depth maxDepth xs | done         = AABBTreeLeaf xs mergedBB
                                             | otherwise    = AABBTreeBranch (aaBBTreeBuildRec (not compX) (depth+1) maxDepth left)
                                                                             (aaBBTreeBuildRec (not compX) (depth+1) maxDepth right)
                                                                             mergedBB
      where
        done        = depth >= maxDepth
        mergedBB    = mconcat bbs
        bbs         = map getBB xs
        left        = filter (isLeft . getBB) xs
        right       = filter (isRight . getBB) xs
        splitPos    = centerBB mergedBB
        isLeft bb   = (dim $ bbMin bb) < dim splitPos
        isRight bb  = (dim $ bbMax bb) >= dim splitPos
        dim         | compX     = fst
                    | otherwise = snd

--------------------------------------------------------------------------------

-- | Finds all items of the tree which collide with the search item.
aaBBTreeCollisions :: (HasBoundingBox a, HasBoundingBox b) => b -> AABBTree a -> [a]
aaBBTreeCollisions _ AABBTreeEmpty                = []
aaBBTreeCollisions search (AABBTreeLeaf xs _)     = filter (doCollide search) xs
aaBBTreeCollisions search (AABBTreeBranch l r _)  = (nodeResult l) ++ (nodeResult r)
  where
    nodeResult n | collides  = aaBBTreeCollisions search n
                 | otherwise = []
      where
        collides = fromMaybe False mayCollide
        mayCollide = fmap (doCollide search) mayBB
        mayBB = case n of
            AABBTreeEmpty           -> Nothing
            (AABBTreeLeaf _ x)      -> Just x
            (AABBTreeBranch _ _ x)  -> Just x

--------------------------------------------------------------------------------

-- | Puts the elements of an AABBTree into a list.
aaBBTreeToList :: (HasBoundingBox a) => AABBTree a -> [a]
aaBBTreeToList AABBTreeEmpty          = []
aaBBTreeToList (AABBTreeLeaf xs _)    = xs
aaBBTreeToList (AABBTreeBranch l r _) = (aaBBTreeToList l) ++ (aaBBTreeToList r)

---TODO rebuild
---TODO filter
---TODO add
---TODO remove
{-
quadRebuild :: (Positioned a) => QuadTree a -> QuadTree a

quadFilter :: (Positioned a) => (a -> Bool) -> QuadTree a -> QuadTree a
-}
