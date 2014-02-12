import Data.List
import Data.Char
import Control.Monad
import qualified Data.Map as Map

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
        sc ('\'':c:'\'':rst) = let (a:as) = sc rst in ('\'':c:'\'':a):as
        sc (c:cs) 
          | ch == c   = []:(sc cs)
          | otherwise = let (a:as) = sc cs in (c:a):as

sepBy _ [] = [[]]
sepBy fn (a:as)
  | fn a      = []:(sepBy fn as)
  | otherwise = let (res1:res) = sepBy fn as in ((a:res1):res)

sepBySkipQuotes _ [] = [[]]
sepBySkipQuotes fn (a:as)
  | fn a      = []:(sepBySkipQuotes fn as)
  | a == '"'  = case readQuote as of
    (q,b:rst) -> let (res1:res) = sepBySkipQuotes fn rst in ((a:q) ++ [b] ++ res1):res
    ([],[])     -> [a:as]
  | otherwise = let (res1:res) = sepBySkipQuotes fn as in ((a:res1):res)

tripFiles = "TripFiles.csv"

readCSV filepath = fmap ((map (sepBySkipQuotes (==','))).tail.lines) $ readFile filepath

trim = reverse . (dropWhile isSpace) . reverse . (dropWhile isSpace) . read

csvGraph filePath dataSelector fromFn tooFn = do
  csv <- readCSV filePath
  let edges = map (\l -> let dl = dataSelector l in (fromFn dl,tooFn dl,[dl])) csv
  return $ foldr (\(a,b,l) mp -> Map.insertWith (++) (a,b) l mp) Map.empty edges
  
data Place = Home | Work | School | OtherPlace deriving (Eq,Show)

readPlace i = i
{-
readPlace 1 = Home
readPlace 2 = Work
readPlace 3 = School
readPlace 4 = OtherPlace
-}

data Purpose = UsualWork 
             | OtherWork
             | ToSchool
             | DropOffOrPickUp
             | Shopping
             | Errands
             | HealthMedical
             | EatOut
             | Entertainment
             | VisitFriends
             | GoHome
             | ChangeMode
             | Other
             deriving (Eq,Show)

readPurpose i = i
{-
readPurpose i = case i of
  1 -> UsualWork
  2 -> OtherWork
  3 -> ToSchool
  4 -> DropOffOrPickUp
  5 -> Shopping
  6 -> Errands
  7 -> HealthMedical
  8 -> EatOut
  9 -> Entertainment
  10 -> VisitFriends
  11 -> GoHome
  96 -> ChangeMode
  97 -> Other
-}
data Date = Date String deriving (Eq,Show)
data Time = Time String deriving (Eq,Show)

readDate = id
readTime = id

data TransitEntry = TransitEntry
                    {
                      sampleNum :: Int,
                      personNum :: Int,
                      tripNum :: Int,
                      tripID :: Int,
                      startingPlace :: Int,
                      endingPlace :: Int,
                      purpose :: Int,
                      departTime :: String,
                      arriveTime :: String,
                      departDate :: String,
                      arriveDate :: String,
                      originCounty :: String,
                      destCounty :: String,
                      stringList :: [String]
                    }
                  deriving (Eq,Show)

selectTransitEntry sl@[sn,pn,tn,ti,sp,ep,p,dt,at,dd,ad,oc,od] = 
  TransitEntry
  {
    sampleNum = read sn,
    personNum = read pn,
    tripNum = read tn,
    tripID = round $ (read ti :: Double),
    startingPlace = readPlace $ read sp,
    endingPlace = readPlace $ read ep,
    purpose = readPurpose $ read p,
    departTime = readTime dt,
    arriveTime = readTime at,
    departDate = readDate dd,
    arriveDate = readDate ad,
    originCounty = trim oc,
    destCounty = trim od,
    stringList = [sn,pn,tn,show $ round $ (read ti :: Double),sp,ep,p,dt,at,dd,ad,show $ trim oc,show $ trim od]
  }
selectTransitEntry x = selectTransitEntry $ take 13 x


colNames = zip ["sampNum","perNum","tripNum","tripID",
                "OriginPlace","DestPlace","purpose",
                "DepartTime","ArriveTime","DepartDate","ArriveDate",
                "OriginCounty","DestCounty"]
           ["Int","Int","Int","Int",
            "Int","Int","Int",
             "String","String","String","String",
             "String","String"]
destNames = filter ((`elem`["Dest","Arri"]).(take 4).fst) colNames
originNames = filter ((`elem`["Orig","Depa"]).(take 4).fst) colNames
remainingNames = (colNames!!6):(take 4 colNames)
simplexNames a b = map (sn a b) $ map fst colNames
  where sn a b n
          | n `elem` (map fst destNames) = b ++ n
          | n `elem` (map fst originNames) = a ++ n
          | otherwise = n
        
tripsGraph = csvGraph tripFiles selectTransitEntry originCounty destCounty

cim i f l = concat $ intersperse i $ map f l

toName (a:as) = filter (not.isSpace) $ (toLower a):as

tripsSchema graph =
  let places = nub $ concatMap (\(a,b) -> [a,b]) $ Map.keys graph
      verts = remainingNames ++ (concatMap (\p -> map (\(n,t) -> ((toName p)++n,t)) $ destNames ++ originNames) places)
      simplices = map (\(a,b) -> simplexNames (toName a) (toName b)) $ Map.keys graph
  in ("create schema with vertices = \n{\n  " ++
      (cim ",\n  " (\(a,t) -> a ++ " :: " ++ t) verts) ++
      "\n}\nsimplices = \n{\n  " ++ 
      (cim ",\n  " (\vs -> "{" ++ (cim "," id vs) ++ "}") simplices) ++ "\n}\n",
      map (\vs -> "{" ++ (cim "," id vs) ++ "}") simplices,verts)
    
csvDir = "EdgeTables"
csvFileName a b = csvDir ++ "/" ++ (toName a) ++ (toName b) ++ "Edge.csv"
         
generateCSVs graph = do
  forM_ (Map.toList graph) $ \((a,b),edge) -> do
    putStrLn $ "generating " ++ (csvFileName a b)
    writeFile (csvFileName a b) $ cim "\n" (cim "," id) $ map stringList edge
    
generateJoSQL graph = do
  let (s,simps,vers) = tripsSchema graph
      schema = "let newYorkTransit = " ++ s ++ ";\n\n"
      inserts = flip map (zip (Map.keys graph) simps) 
                $ (\((a,b),s) -> "let e" ++ (toName a) ++ (toName b) ++ 
                                 " = instantiate newYorkTransit at " ++ s
                                 ++ " with load " ++ (show $ "../data/Transit/" ++ (csvFileName a b)) ++ ";\n\n")
      union = "let nycTransitSurvey = union " ++ (cim "," (\(a,b) -> "e" ++ (toName a) ++ (toName b)) $ Map.keys graph) ++ ";\n\n"
  writeFile "nyctransitsetup.joSQL" (schema ++ (concat inserts) ++ union)