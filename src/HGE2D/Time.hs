-- |
-- Module      :  HGE2D.Time
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing functions to get the current time or transform times

module HGE2D.Time where

import HGE2D.Types

import Data.Time.Clock

--------------------------------------------------------------------------------

-- | Get the current time in seconds
getSeconds :: IO Double
getSeconds = getCurrentTime >>= return . fromRational . toRational . utctDayTime

--------------------------------------------------------------------------------

-- | Transform seconds to milliseconds
toMilliSeconds :: Double -> Millisecond
toMilliSeconds secs = floor $ 1000 * secs
