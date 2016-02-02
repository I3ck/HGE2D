module HGE2D.Time where

import HGE2D.Types

import Data.Time.Clock ---TODO move once the time methods moved

getSeconds :: IO Double
getSeconds = getCurrentTime >>= return . fromRational . toRational . utctDayTime

toMilliSeconds :: Double -> Millisecond
toMilliSeconds secs = floor $ 1000 * secs
