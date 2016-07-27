import HGE2D.Types
import HGE2D.Classes
import HGE2D.Instances
import HGE2D.Time

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
    describe "Time.hs" $ do
        it "transforms a floating point seconds into integer milliseconds" $ do
            toMilliSeconds 0.73     `shouldBe`      (730 :: Int)
            toMilliSeconds 0.0      `shouldBe`      (0 :: Int)
            toMilliSeconds (-10.0)  `shouldBe`      (-10000 :: Int)

    describe "RealPosition" $ do
        it "Positioned getX" $
            property $ \ x y -> fst (x,y)       == getX   ( (x,y) :: RealPosition )
        it "Positioned getY" $
            property $ \ x y -> snd (x,y)       == getY   ( (x,y) :: RealPosition )
        it "Positioned getPos" $
            property $ \ x y ->     (x,y)       == getPos ( (x,y) :: RealPosition )
