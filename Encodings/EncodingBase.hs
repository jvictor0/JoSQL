module Encodings.EncodingBase where

import Data.List

type Compressor input meta = [input] -> Maybe (meta,[input])
type Decompressor input meta = meta -> [input] -> [input]


compressorCompose :: Compressor a b -> Compressor a c -> Compressor a (b,c)
compressorCompose f1 f2 dt = do
  (b, dt1) <- f1 dt
  (c, result) <- f2 dt1
  return ((b,c), result)
  
decompressorCompose :: Decompressor a b -> Decompressor a c -> Decompressor a (b,c)
decompressorCompose f1 f2 (b,c) dt = f1 b (f2 c dt)
  