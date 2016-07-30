---TODO copy
---TODO module

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes
import HGE2D.Instances
import HGE2D.Geometry
import HGE2D.Time
import HGE2D.Collision

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

        --it "velAngle" $ ---TODO not working
        --    property $ \ x1 y1 x2 y2    -> abs ((radRealPos (x1, y1) (x2, y2)) - (velAngle (x2 - x1, y2 - y1))) < 0.0001

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

        it "bbFromList" $ do
            bbFromList ([] :: [RealPosition])                                       `shouldBe` BBEmpty
            bbFromList ([(0.0, 0.0)] :: [RealPosition])                             `shouldBe` BBEmpty
            bbFromList ([(0.0, 0.0), (1.0, 1.0)] :: [RealPosition])                 `shouldBe` BoundingBox (0.0, 0.0) (1.0, 1.0)
            bbFromList ([(0.0, 0.0), (1.0, 1.0), (1.5, 0.8)] :: [RealPosition])     `shouldBe` BoundingBox (0.0, 0.0) (1.5, 1.0)

        it "mergeBBEmpty" $
            property $ \ minPos maxPos -> mergeBB (BoundingBox minPos maxPos) BBEmpty == (BoundingBox minPos maxPos)

        it "mergeBB" $ do
            mergeBB (BoundingBox (0.0, 5.0) (1.0, 6.0)) (BoundingBox (-5.0, 2.0) (2.0, 12.0)) `shouldBe` (BoundingBox (-5.0, 2.0) (2.0, 12.0))

        it "makeBB" $ ---TODO same as sizedBB (drop one of the functions)
            property $ \ center width height -> makeBB center width height == sizedBB center width height

        it "applyVelocity" $
            property $ \ pos vel time -> applyVelocity pos vel time == (((fst pos) + (fromIntegral time * (fst vel))), ((snd pos) + (fromIntegral time * (snd vel))))

    describe "Collision.hs" $ do
        it "doCollideNull1" $
            property $ \ pMin pMax -> doCollide (BBEmpty) (BoundingBox pMin pMax) == False
        it "doCollideNull2" $
            property $ \ pMin pMax -> doCollide (BoundingBox pMin pMax) (BBEmpty) == False
        it "doCollideSelf" $
            property $ \pMin pMax -> doCollide (BoundingBox pMin pMax) (BoundingBox pMin pMax) == True
        it "doCollide" $ do
            doCollide (BoundingBox (0.0, 0.0) (1.0, 1.0)) (BoundingBox (1.1, 1.1) (2.0, 2.0)) `shouldBe` False
            doCollide (BoundingBox (0.0, 0.0) (1.0, 1.0)) (BoundingBox (0.99, 0.99) (2.0, 2.0)) `shouldBe` True
        it "isInsideNull1" $
            property $ \ pMin pMax -> isInside (BBEmpty) (BoundingBox pMin pMax) == False
        it "isInsideNull2" $
            property $ \ pMin pMax -> isInside (BoundingBox pMin pMax) (BBEmpty) == False
        it "isInside" $ do
            isInside (BoundingBox (0.0, 0.0) (1.0, 1.0)) (BoundingBox (1.1, 1.1) (2.0, 2.0)) `shouldBe` False
            isInside (BoundingBox (0.0, 0.0) (1.0, 1.0)) (BoundingBox (0.99, 0.99) (2.0, 2.0)) `shouldBe` False
            isInside (BoundingBox (0.5, 0.5) (1.0, 1.0)) (BoundingBox (0.0, 0.0) (2.0, 2.0)) `shouldBe` True
        it "isInsideRPNull1" $
            property $ \ p -> isInsideRP (p :: RealPosition) BBEmpty == False
        it "isInsideRP" $ do
            isInsideRP ((0.0, 0.0) :: RealPosition) (BoundingBox (0.5, 0.5) (1.0, 1.0)) `shouldBe` False
            isInsideRP ((0.7, 0.0) :: RealPosition) (BoundingBox (0.5, 0.5) (1.0, 1.0)) `shouldBe` False
            isInsideRP ((0.7, 0.7) :: RealPosition) (BoundingBox (0.5, 0.5) (1.0, 1.0)) `shouldBe` True
        it "doContainNull1" $
            property $ \ pMin pMax -> doContain (BBEmpty) (BoundingBox pMin pMax) == False
        it "doContainNull2" $
            property $ \ pMin pMax -> doContain (BoundingBox pMin pMax) (BBEmpty) == False
        it "doContain" $ do
            doContain (BoundingBox (0.0, 0.0) (1.0, 1.0)) (BoundingBox (1.1, 1.1) (2.0, 2.0)) `shouldBe` False
            doContain (BoundingBox (0.0, 0.0) (1.0, 1.0)) (BoundingBox (0.99, 0.99) (2.0, 2.0)) `shouldBe` False
            doContain (BoundingBox (0.5, 0.5) (1.0, 1.0)) (BoundingBox (0.0, 0.0) (2.0, 2.0)) `shouldBe` True
            doContain (BoundingBox (0.0, 0.0) (2.0, 2.0)) (BoundingBox (0.5, 0.5) (1.0, 1.0)) `shouldBe` True

    describe "RealPosition" $ do
        it "Positioned getX" $
            property $ \ x y -> fst (x,y)       == getX   ( (x,y) :: RealPosition )
        it "Positioned getY" $
            property $ \ x y -> snd (x,y)       == getY   ( (x,y) :: RealPosition )
        it "Positioned getPos" $
            property $ \ x y ->     (x,y)       == getPos ( (x,y) :: RealPosition )

    ---TODO AABBTree
    ---TODO QuadTree
    ---TODO instances
