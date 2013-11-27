module RuntimeUtils where

fromEither (Left err) = fail err
fromEither (Right a) = return a