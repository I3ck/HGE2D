-- |
-- Module      :  HGE2D.QuadTree
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing the definition and functions for a quad tree

module HGE2D.QuadTree where

import HGE2D.Datas
import HGE2D.Collision
import HGE2D.Geometry
import HGE2D.Classes

import Data.List

--------------------------------------------------------------------------------

-- | A QuadTree for faster search queries
data (Positioned a) => QuadTree a = QuadEmpty   -- empty node
                                   | QuadLeaf a -- node with one position
                                   | QuadBranch (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a) BoundingBox -- branch which holds several subtrees

-- | Posibble directions within a branch (negative x && negative y | negative x && positive y ...)
data QuadDir = NN | NP | PN | PP

--------------------------------------------------------------------------------

-- | Mapping function for the QuadTree. Do not use with functions which change the position of items,
--   since they would invalidate the search structure (use fmapQuadRebuild instead)
fmapQuad :: (Positioned a, Positioned b) => (a -> b) -> QuadTree a -> QuadTree b
fmapQuad _ QuadEmpty                    = QuadEmpty
fmapQuad f (QuadLeaf x)                 = (QuadLeaf (f x))
fmapQuad f (QuadBranch nn np pn pp bb)  = (QuadBranch (fmapQuad f nn) (fmapQuad f np) (fmapQuad f pn) (fmapQuad f pp) bb)

-- | Mapping function for the QuadTree. This rebuilds the entire tree, but allows the usage of
--   position changing functions. Use this only for positioned changing functions, and fmapQuad otherwise,
--   since it is faster
fmapQuadRebuild :: (Positioned a, Positioned b) => (a -> b) -> QuadTree a -> QuadTree b
fmapQuadRebuild f old = buildQuadTree newXs
  where
    newXs = map f $ quadTreeToList old

--------------------------------------------------------------------------------

-- | Builds a QuadTree from a list of elements with positions.
buildQuadTree :: (Positioned a) => [a] -> QuadTree a
buildQuadTree []  = QuadEmpty
buildQuadTree [x] = QuadLeaf x
buildQuadTree xs  = QuadBranch nn np pn pp bb
  where
    bb = bbFromList xs

    nn = buildQuadTree $ filter isnn xs
    np = buildQuadTree $ filter isnp xs
    pn = buildQuadTree $ filter ispn xs
    pp = buildQuadTree $ filter ispp xs

    center = centerBB bb

    isnn x = getX x <= getX center  && getY x <= getY center
    isnp x = getX x <= getX center  && getY x >  getY center
    ispn x = getX x >  getX center  && getY x <= getY center
    ispp x = getX x >  getX center  && getY x >  getY center

--------------------------------------------------------------------------------

-- | Helper function to decide into which sub-node a position belongs
calcQuadDir :: (Positioned a) => a -> BoundingBox -> QuadDir
calcQuadDir p bb
  | (getX p) <  (getX $ centerBB bb)  && (getY p) <  (getY $ centerBB bb) = NN
  | (getX p) <  (getX $ centerBB bb)  && (getY p) >= (getY $ centerBB bb) = NP
  | (getX p) >= (getX $ centerBB bb)  && (getY p) <  (getY $ centerBB bb) = PN
  | otherwise                                                             = PP

--------------------------------------------------------------------------------

-- | Finds the nearest within the tree to the search position (if there is any)
nearestQuad :: (Positioned a, Positioned b) => a -> QuadTree b -> Maybe b
nearestQuad _ QuadEmpty       = Nothing
nearestQuad _ (QuadLeaf x)    = Just x
nearestQuad search (QuadBranch nn np pn pp bb) = foldResults best nodesToCheck
  where
    foldResults best []     = best
    foldResults best [x]    | mustCheck best x = minimumBy (\ a b -> compare (mayDist a) (mayDist b)) [best, nearestQuad search x]
                            | otherwise = best
    foldResults best (x:xs) = foldResults (foldResults best [x]) xs


    nodesToCheck = case calcQuadDir search bb of
        NN -> [    np, pn, pp]
        NP -> [nn,     pn, pp]
        PN -> [nn, np,     pp]
        PP -> [nn, np, pn    ]


    best = case calcQuadDir search bb of
        NN -> nearestQuad search nn
        NP -> nearestQuad search np
        PN -> nearestQuad search pn
        PP -> nearestQuad search pp

    ---TODO define these outside?
    mayDist p = case p of
        Nothing -> 1e300 ---TODO use double max
        Just x  -> distanceSqr search x

    mustCheck _ QuadEmpty                   = False
    mustCheck best (QuadLeaf x)             = distanceSqr search x < mayDist best
    mustCheck best (QuadBranch _ _ _ _ bb)  = isInsideRP search bb || distanceBBSqr search bb < mayDist best

--------------------------------------------------------------------------------

-- | Puts the elements of a QuadTree into a list
quadTreeToList :: (Positioned a) => QuadTree a -> [a]
quadTreeToList QuadEmpty = []
quadTreeToList (QuadLeaf x) = [x]
quadTreeToList (QuadBranch nn np pn pp _) = (quadTreeToList nn) ++ (quadTreeToList np) ++ (quadTreeToList pn) ++ (quadTreeToList pp)

{-
quadRebuild :: (Positioned a) => QuadTree a -> QuadTree a

quadFilter :: (Positioned a) => (a -> Bool) -> QuadTree a -> QuadTree a
-}
