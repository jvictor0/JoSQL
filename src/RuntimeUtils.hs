module RuntimeUtils where

import System.IO
import Control.Monad.Trans.Either
import Types
import Control.Exception

fromEither (Left err) = error err
fromEither (Right a) = return a

fromEitherUnsafe preerr (Left err) = error $ preerr ++ ": " ++ err
fromEitherUnsafe _ (Right a) = a

isLeft (Left _) = True
isLeft _ = False

fromRight (Right a) = a

readJustMaybe        :: (Read a) => Maybe String -> Maybe (Maybe a)
readJustMaybe Nothing = Just Nothing
readJustMaybe (Just s)=  case [x | (x,t) <- reads s, ("","") <- lex t] of
  [x] -> Just (Just x)
  _   -> Nothing

hGetTransmission :: Handle -> ErrorT IO [String]
hGetTransmission hdl = do
  l <- tryErrorT $ hGetLine hdl
  if l == "\EOT" then return [] else fmap (l:) $ hGetTransmission hdl
 
tryErrorT :: IO a -> ErrorT IO a
tryErrorT act = do
  eexct <- EitherT $ fmap return $ try act 
  case eexct of
    (Left err) -> left $ show (err :: IOError)
    (Right a) -> return a
