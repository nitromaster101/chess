{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import Test.QuickCheck.Arbitrary
import Text.Printf
import Data.Int
import System.Random
import Board

instance Arbitrary Int8 where
  arbitrary = fmap fromIntegral $ choose (0, 7::Int)
  --coarbitrary c = variant (c `rem` 4)


main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

s2p_p2s s = (str2pos . pos2str) s == id s

tests = [("str2pos.pos2str/id", quickCheck s2p_p2s)]

