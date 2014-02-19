module Encodings.IntegerCompression where

import Data.List
import Data.Word
import Data.Bits


import Encodings.EncodingBase

rotate :: (Num a, Ord a) => Compressor a a
rotate dt = Just (mn, map ((-) mn) dt)
  where mn = minimum dt

unRotate :: (Num a, Ord a) => Decompressor a a
unRotate  mn dt  = map (+mn) dt

squash :: (Integral a) => Compressor a a
squash dt = if g == 0 then Nothing else Just (g, map (`div` g) dt)
  where g = foldr gcd 0 dt
        
unSquash :: (Integral a) => Decompressor a a
unSquash g dt = map (*g) dt

