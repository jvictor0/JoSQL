
import System.Process
import System.Directory

stupid mod fil = do
  putStrLn $ "sed -i 's/^import " ++ fil ++ "/import " ++ mod ++ "\\." ++ fil ++ "/g' Server/*.hs CodeGen/*.hs CodeGen/Metadata/*.hs Data/*.hs Utils/*.hs Encodings/*.hs"
  system $ "sed -i 's/^import " ++ fil ++ "/import " ++ mod ++ "\\." ++ fil ++ "/g' Server/*.hs CodeGen/*.hs CodeGen/Metadata/*.hs Data/*.hs Utils/*.hs Encodings/*.hs"

stupidModule mod fil = do  
  system $ "sed -i 's/^module /module CodeGen\\.Metadata\\./' " ++ mod ++ "/" ++ fil
  
stupidAll mod = do
  conts <- fmap (filter (\x -> ".hs" == (reverse $ take 3 $ reverse $ x))) $ getDirectoryContents mod
  print $ map (\x -> reverse $ drop 3 $ reverse x) conts
  mapM (stupid "CodeGen.Module") $ map (\x -> reverse $ drop 3 $ reverse x) conts
  
stupidModuleAll mod = do
  conts <- fmap (filter (\x -> ".hs" == (reverse $ take 3 $ reverse $ x))) $ getDirectoryContents mod
  mapM (stupidModule mod) $ conts
  
symlink mod fil = do
  system $ "rm " ++ mod ++ "/" ++ fil ++ ".o"
  system $ "rm " ++ mod ++ "/" ++ fil ++ ".hi"
  putStrLn $ "dist/build/" ++ mod ++ "/" ++ fil ++ ".o"
  b <- doesFileExist $ "dist/build/" ++ mod ++ "/" ++ fil ++ ".o"
  c <- doesFileExist $ "dist/build/" ++ mod ++ "/" ++ fil ++ ".hi"
  print $ (b,c)
--  system $ "ln dist/build/" ++ mod ++ "/" ++ fil ++ ".o " ++ mod ++ "/" ++ fil ++ ".o"
--  system $ "ln dist/build/" ++ mod ++ "/" ++ fil ++ ".hi " ++ mod ++ "/" ++ fil ++ ".hi"

symlinks = mapM (uncurry symlink) linkables
  
linkables = [("Utils","Include"),
             ("Utils","RuntimeUtils"),
             ("Utils","Utils"),
             ("CodeGen","Paramable"),
             ("Server","NutleyInstance"),
             ("Data","Types")]