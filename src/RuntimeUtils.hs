module RuntimeUtils where

fromEither (Left err) = error err
fromEither (Right a) = return a

fromEitherUnsafe preerr (Left err) = error $ preerr ++ ": " ++ err
fromEitherUnsafe _ (Right a) = a