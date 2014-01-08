module RuntimeUtils where

fromEither (Left err) = error err
fromEither (Right a) = return a

fromEitherUnsafe preerr (Left err) = error $ preerr ++ ": " ++ err
fromEitherUnsafe _ (Right a) = a

isLeft (Left _) = True
isLeft _ = False

fromRight (Right a) = a

readMaybe        :: (Read a) => String -> Maybe a
readMaybe s      =  case [x | (x,t) <- reads s, ("","") <- lex t] of
  [x] -> Just x
  _   -> Nothing