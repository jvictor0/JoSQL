module Verify where

import Control.Exception
import Utils

class Verify v where
  isVerifiedError :: v -> Maybe String
  isVerifiedError v = let errs = verifyConditions v
                      in case cim "\n" snd $ filter (not.fst) errs of
                        "" -> Nothing
                        e  -> Just e
                        
  isVerified :: v -> Bool
  isVerified v = Nothing == (isVerifiedError v)
  
  verifyEither :: v -> Either String v
  verifyEither v = case isVerifiedError v of
    Nothing -> return v
    (Just err) -> Left err
  
  verifyConditions :: v -> [(Bool,String)]
  verifyConditions v = [(isVerified v, "Verification Error")]

  verify :: (Monad m) => v -> m ()
  verify v = case isVerifiedError v of
    (Just error) -> fail $ "verify error: " ++ error
    Nothing      -> return ()

  verified :: v -> v
  verified v = assert (isVerified v) v