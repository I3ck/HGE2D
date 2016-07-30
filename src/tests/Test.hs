---TODO copy
---TODO module

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Instances
import HGE2D.Geometry
import HGE2D.Time

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "Time.hs" $ do
        it "toMilliSeconds" $ do
            toMilliSeconds 0.73     `shouldBe`      (730 :: Int)
            toMilliSeconds 0.0      `shouldBe`      (0 :: Int)
            toMilliSeconds (-10.0)  `shouldBe`      (-10000 :: Int)

    describe "Geometry.hs" $ do
        it "rad2deg" $
            property $ \ x -> x * 180.0 / pi    == rad2deg ( x :: Double)
        it "deg2rad" $
            property $ \x -> x * pi / 180.0     == deg2rad ( x :: Double)

        it "radRealPos" $ do
            radRealPos (0.0, 0.0) (1.0, 0.0)    `shouldBe` (0.0 * pi)
            radRealPos (0.0, 0.0) (0.0, 1.0)    `shouldBe` (0.5 * pi)
            radRealPos (0.0, 0.0) (-1.0, 0.0)   `shouldBe` (1.0 * pi)
            radRealPos (0.0, 0.0) (0.0, -1.0)   `shouldBe` (-0.5 * pi)

        it "velAngle" $
            property $ \ x1 y1 x2 y2    -> abs ((radRealPos (x1, y1) (x2, y2)) - (velAngle (x2 - x1, y2 - y1))) < 0.0001

        it "sqrDistance" $
            property $ \ x1 y1 x2 y2 -> distanceSqr (x1,y1) (x2,y2) == (x2 - x1)**2 + (y2 - y1)**2

        it "distance" $
            property $ \ x1 y1 x2 y2 -> (distance (x1,y1) (x2, y2)) == (sqrt $ distanceSqr (x1 :: Double, y1 :: Double) (x2 :: Double, y2 :: Double))

        it "distanceBB" $ --- TODO must also test distance function itself
            property $ \ x y -> distanceBB (x,y) (BoundingBox (0, 0) (10, 10)) == (sqrt $ distanceBBSqr (x :: Double, y :: Double) (BoundingBox (0, 0) (10, 10)))

        it "direction" $ do
            direction ((0.0, 0.0) :: RealPosition) ((1.0, 0.0) :: RealPosition)     `shouldBe` ((1.0, 0.0) :: RealPosition)
            direction ((0.0, 0.0) :: RealPosition) ((2.0, 0.0) :: RealPosition)     `shouldBe` ((1.0, 0.0) :: RealPosition)
            direction ((0.0, 0.0) :: RealPosition) ((0.0, 2.0) :: RealPosition)     `shouldBe` ((0.0, 1.0) :: RealPosition)

        it "closest" $ do
            closest ((0.0, 0.0) :: RealPosition) ([] :: [RealPosition])                         `shouldBe` Nothing
            closest ((0.0, 0.0) :: RealPosition) ([(5.0, 5.0)] :: [RealPosition])               `shouldBe` Just (5.0, 5.0)
            closest ((0.0, 0.0) :: RealPosition) ([(1.0, 1.0), (5.0, 5.0)] :: [RealPosition])   `shouldBe` Just (1.0, 1.0)

        it "furthest" $ do
            furthest ((0.0, 0.0) :: RealPosition) ([] :: [RealPosition])                         `shouldBe` Nothing
            furthest ((0.0, 0.0) :: RealPosition) ([(5.0, 5.0)] :: [RealPosition])               `shouldBe` Just (5.0, 5.0)
            furthest ((0.0, 0.0) :: RealPosition) ([(1.0, 1.0), (5.0, 5.0)] :: [RealPosition])   `shouldBe` Just (5.0, 5.0)

        --- TODO interceptionPos

        it "makeRB" $
            property $ \ center vel width height -> makeRB center vel width height == RigidBody { rigidPos = center, rigidVel = vel, rigidBB = sizedBB center width height }

        it "sizedBB" $
            sizedBB (0.0, 0.0) 10 20 `shouldBe` BoundingBox (-5.0, -10.0) (5.0, 10.0)

        it "sizeBBW" $
            property $ \ center width height -> (abs $ (fst $ sizeBB $ sizedBB center width height) - width) < 0.001

        it "sizeBBH" $
            property $ \ center width height -> (abs $ (snd $ sizeBB $ sizedBB center width height) - height) < 0.001

        it "centerBB" $
            property $ \ center width height -> distance center (centerBB (sizedBB center width height)) < 0.001

    describe "RealPosition" $ do
        it "Positioned getX" $
            property $ \ x y -> fst (x,y)       == getX   ( (x,y) :: RealPosition )
        it "Positioned getY" $
            property $ \ x y -> snd (x,y)       == getY   ( (x,y) :: RealPosition )
        it "Positioned getPos" $
            property $ \ x y ->     (x,y)       == getPos ( (x,y) :: RealPosition )
