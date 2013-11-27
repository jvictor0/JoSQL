module RuntimeUtils where

fromEither (Left err) = error err
fromEither (Right a) = return a