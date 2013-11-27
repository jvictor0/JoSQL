{-# LANGUAGE ScopedTypeVariables #-}
{- Ripped off from Jani Hartikainen, thanks Jani! -}
module DynLoad (
loadSourceGhc,
execFnGhc
) where
 
import Control.Exception (throw)
import GHC hiding (loadModule)
 
execFnGhc :: String -> String -> Ghc a
execFnGhc modname fn = do
mod <- findModule (mkModuleName modname) Nothing
setContext [] [mod]
value <- compileExpr (modname ++ "." ++ fn)
 
let value' = (unsafeCoerce value) :: a
return value'
 
loadSourceGhc :: String -> Ghc (Maybe String)
loadSourceGhc path = let
throwingLogger (Just e) = throw e
throwingLogger _ = return ()
in do
dflags <- getSessionDynFlags
setSessionDynFlags (dflags{
ghcLink = LinkInMemory,
hscTarget = HscInterpreted,
packageFlags = [ExposePackage "ghc"]
})
target <- guessTarget path Nothing
addTarget target
r <- loadWithLogger throwingLogger LoadAllTargets
case r of
Failed -> return $ Just "Generic module load error"
Succeeded -> return Nothing
 
`gcatch` \(e :: SourceError) -> let
errors e = concat $ map show (bagToList $ srcErrorMessages e)
in
return $ Just (errors e)
