module RuntimeUtils where

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
