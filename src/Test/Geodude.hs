import Network.Socket
import Network
import System.IO
import System.Directory
import System.Process
import Control.Monad
import Data.List
import Data.Char
import System.Environment
import System.Random


data QueryResult = QR String String String [String]

data Mode = Mode MType String

data MType = Test | Update | Diff | Error deriving (Eq)
numASCIIs = 3

main = do
  n <- randomIO :: IO Int
  (readFile $ "ASCIIDude" ++ (show $ (n`mod`numASCIIs) + 1)) >>= putStrLn
  args <- getArgs
  let (Mode md dotFile) = getMode args
  joSQLInputs <- generateInputs 0 "." dotFile
  forM_ joSQLInputs $ \x -> do
    putStrLn x
    when (md`elem`[Test,Update]) $ executeTest x
    when (md`elem`[Test,Diff]) $ checkTest x 
    when (md == Update) $ (updateTest x) >> passTest
  
  
getMode [dotFile] = Mode Test dotFile
getMode ["-u",dotFile] = Mode Update dotFile
getMode ["-update",dotFile] = Mode Update dotFile
getMode ["-d",dotFile] = Mode Diff dotFile
getMode ["-diff",dotFile] = Mode Diff dotFile
getMode _ = Mode Error ""                        
 
isDotFile "." = False
isDotFile ".." = False
isDotFile ('.':file) = True
isDotFile _ = False

isJoSQL f = (reverse $ take (length ".joSQL") $ reverse f) == ".joSQL"
dropJoSQL f = reverse $ drop (length ".joSQL") $ reverse f

maxDepth = 10

generateInputs _ _ "" = return []
generateInputs depth path dotFile = do
  bf <- doesFileExist $ path ++ "/" ++ dotFile
  bd <- doesDirectoryExist $ path ++ "/" ++ dotFile
  if (not bf && not bd) || (depth > maxDepth)
    then do
     putStrLn $ "WARNING: " ++ (path ++ "/" ++ dotFile) ++ " not found"
     return []
    else if bf && (isDotFile dotFile)
         then do
           cs <- fmap ((filter (not.(all isSpace))).lines) $ readFile dotFile
           fmap (nub.concat) $ mapM (\x -> generateInputs (depth + 1) path x) cs
         else if bf && (isJoSQL dotFile)
              then return [path ++ "/" ++ (dropJoSQL dotFile)]
              else if bd
                   then do 
                     cs <- fmap (filter (\x -> not $ x`elem`[".",".."])) $ getDirectoryContents $ path ++ "/" ++ dotFile
                     fmap (nub.concat) $ mapM (\x -> generateInputs (depth + 1) (path ++ "/" ++ dotFile) x) cs
                   else return []

                   
                   
putStrLnGreen str = putStrLn $ "\x1b[32m" ++ str ++ "\x1b[0m"
putStrLnRed str = putStrLn $ "\x1b[31m" ++ str ++ "\x1b[0m"

passTest = putStrLnGreen "    PASS"
failTest = putStrLnRed "    FAIL"

dropLine = tail.(dropWhile (/='\n'))

queryHead i = "######## ----QUERY " ++ (show i) ++ "---- ########"
resultHead i = "######## ----RESULT " ++ (show i) ++ "---- ########"

breakSharps lines = break ((=="########").(take 8)) lines

readResultsFile :: FilePath -> IO [QueryResult]
readResultsFile fn = fmap (rrf.lines) $ readFile $ fn
  where rrf (l1:rst) = let (q1,l2:rst0) = breakSharps rst
                       in let (r1,rst1) = breakSharps rst0
                          in (QR l1 (unlines q1) l2 r1):(rrf rst1)
        rrf [] = []

updateTest :: FilePath -> IO ()
updateTest fp = do
  b <- doesFileExist $  fp ++ ".joSQL"
  if not b
    then putStrLn $ "WARNING: file " ++ fp ++ ".joSQL not found"
    else do
    system $ "cp " ++ (fp ++ ".result") ++ " " ++  (fp ++ ".expected")
    return ()

checkTest :: FilePath -> IO ()
checkTest fp = do
  resb <- doesFileExist $ fp ++ ".result"
  expb <- doesFileExist $ fp ++ ".expected"
  if not $ resb && expb
    then putStrLn $ "WARNING: results or expected file " ++ fp ++ " not found"
    else do 
    res <- readResultsFile $ fp ++ ".result"
    exp <- readResultsFile $ fp ++ ".expected"
    if (length res) /= (length exp)
      then do
      putStrLn "  Different number of queries in result and expected"
      failTest
      else do
      comps <- forM (zip3 [1..] res exp) $ \(i,(QR _ qr _ rr),(QR _ qe _ rq)) -> do
        if qr /= qe
          then do
          putStrLn $ "  Query number " ++ (show i) ++ " mismatch"
          return False
          else if (sort $ tail rr) /= (sort $ tail rq)
               then do
                 putStrLn $ "  Query number " ++ (show i) ++ " wrong results"
                 return False
               else return True
      if and comps
        then passTest
        else failTest

executeTest :: FilePath -> IO ()
executeTest fp = do
  b <- doesFileExist $  fp ++ ".joSQL"
  if not b
    then putStrLn $ "WARNING: file " ++ fp ++ ".joSQL not found"
    else do
    input <- readFile $ fp ++ ".joSQL"
    han <- connectTo "localhost" $ PortNumber 4242 
    hPutStr han input
    results <- hGetContents han
    let resTups = zip3 [1..] (splitContents ';' input) (splitContents '\EOT' results)
        res = unlines $ concatMap (\(i,inp,outp) -> [queryHead i,dropWhile isSpace inp,resultHead i,dropWhile isSpace outp]) resTups
    writeFile (fp ++ ".result") res


readQuote ('\"':rst) = ("",'\"':rst)
readQuote ('\\':a:rst) = let (f,r) = readQuote rst in ('\\':a:f,r)
readQuote (a:rst) 
  | isPrint a = let (f,r) = readQuote rst in (a:f,r)
  | otherwise = ([],a:rst)
readQuote [] = ([],[])
            
splitContents ch f = init $ sc f
  where sc [] = [[]]
        sc ('"':rst) = let (q,m:rst0) = readQuote rst
                           (c:cs) = sc rst0
                       in ((('"':q) ++ [m] ++ c)):cs
        sc (c:cs) 
          | ch == c   = []:(sc cs)
          | otherwise = let (a:as) = sc cs in (c:a):as
               
