---TODO copy
---TODO module

import HGE2D.Types
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

    describe "RealPosition" $ do
        it "Positioned getX" $
            property $ \ x y -> fst (x,y)       == getX   ( (x,y) :: RealPosition )
        it "Positioned getY" $
            property $ \ x y -> snd (x,y)       == getY   ( (x,y) :: RealPosition )
        it "Positioned getPos" $
            property $ \ x y ->     (x,y)       == getPos ( (x,y) :: RealPosition )
