module Server.Parser where

import Prelude 
import Server.ClientQueries
import Utils.Utils
import CodeGen.HaskellCode
import Data.Types

import Data.Maybe
import Data.List
import Control.Monad
import Data.Char

data NodeType = TopLevel | Paren | Bracket | Curly deriving (Eq)

data LexTree = Ident String 
             | Node NodeType [LexTree] 
             | Quote String
             | CharLit Char
             | LetToken
             | LoadToken
             | ClearToken
             | DataToken
             | ConnectToken
             | CacheToken
             | ServerToken
             | CreateToken
             | SelectToken
             | FilterToken
             | UnknownToken
             | UnionToken
             | SchemaToken
             | MapToken
             | ShriekToken
             | ImageToken
             | DirectToken
             | InverseToken
             | InstantiateToken
             | WithToken
             | AtToken
             | ByToken
             | FromToken
             | OnToken
             | ToToken
             | FreshToken
             | VerticesToken
             | SimplicesToken
             | ShowToken
             | NullToken
             | EqToken
             | CommaToken
             | UnderScoreToken
             | LambdaToken
             | ColonToken
             | ArrowToken               
             | KILLToken
             | QuitToken
             deriving (Eq)

instance Show LexTree where
  show (Ident s) = s
  show (Quote str)  = str
  show (CharLit ch) = show ch
  show LetToken = "let"
  show CreateToken = "create"
  show SelectToken = "select"
  show InstantiateToken = "instantiate"
  show ShowToken = "show"                     
  show MapToken = "map"
  show FilterToken = "filter"                     
  show SchemaToken = "schema"
  show ShriekToken = "shriek"
  show ImageToken = "image"
  show InverseToken = "inverse"
  show ConnectToken = "connect"
  show ToToken = "to"
  show FreshToken = "fresh"
  show DirectToken = "direct"
  show WithToken = "with"
  show AtToken = "at"
  show LoadToken = "load"
  show ByToken = "by"
  show FromToken = "from"               
  show VerticesToken = "vertices"
  show SimplicesToken = "simplices"
  show NullToken = "null" 
  show EqToken = "="
  show CommaToken = ","
  show UnderScoreToken = "_"
  show LambdaToken = "\\"
  show ArrowToken = "->"
  show ColonToken = "::"
  show UnionToken = "union"
  show KILLToken = "KILL"
  show ClearToken = "clear"
  show CacheToken = "cache"
  show QuitToken = "quit"
  show OnToken = "on"
  show ServerToken = "server"
  show UnknownToken = "UNKOWN"
  show (Node t lvs) = (leftBraceStr t) ++ (cim " " show lvs) ++ (rightBraceStr t)

isLeftBrace x = x`elem`"([{"
isRightBrace x = x`elem`")]}"
isBrace x = isLeftBrace x || isRightBrace x 

leftBraceLevel '(' = Paren
leftBraceLevel '[' = Bracket
leftBraceLevel '{' = Curly
rightBraceLevel ')' = Paren
rightBraceLevel ']' = Bracket
rightBraceLevel '}' = Curly

leftBraceStr TopLevel = ""
leftBraceStr Paren = "("
leftBraceStr Bracket = "["
leftBraceStr Curly = "{"
rightBraceStr TopLevel = ""
rightBraceStr Paren = ")"
rightBraceStr Bracket = "]"
rightBraceStr Curly = "}"

token "::" = ColonToken
token "," = CommaToken
token "_" = UnderScoreToken
token "\\" = LambdaToken
token "=" = EqToken
token "->" = ArrowToken
token "map" = MapToken
token "create" = CreateToken
token "select" = SelectToken
token "let" = LetToken
token "instantiate" = InstantiateToken
token "schema" = SchemaToken
token "with" = WithToken
token "at" = AtToken
token "from" = FromToken
token "shriek" = ShriekToken
token "image" = ImageToken
token "connect" = ConnectToken
token "to" = ToToken
token "on" = OnToken
token "direct" = DirectToken
token "inverse" = InverseToken
token "show" = ShowToken
token "union" = UnionToken
token "vertices" = VerticesToken
token "simplices" = SimplicesToken
token "load" = LoadToken
token "null" = NullToken
token "KILL" = KILLToken
token "server" = ServerToken
token "clear" = ClearToken
token "fresh" = FreshToken
token "cache" = CacheToken
token "data" = DataToken
token "quit" = QuitToken
token "filter" = FilterToken
token "by" = ByToken
token "~" = UnknownToken
token str = Ident str

fromTopLevel (Node TopLevel lexTree) = lexTree

identName (Ident n) = Just n
identName _ = Nothing

isInfix = flip elem "+-*=/<>:"
isSingletonTok a = (isBrace a) || (a`elem`"_\\,")
isTokenable '_' = True
isTokenable a = isAlphaNum a

lexEscChar c = 
  case c of
    'a'  -> '\a'
    'b'  -> '\b'
    'f'  -> '\f'
    'n'  -> '\n'
    'r'  -> '\r'
    't'  -> '\t'
    'v'  -> '\v'
    '\\' -> '\\'
    '\"' -> '\"'
    '\'' -> '\''
    a    -> a

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
               
        
preLex [] = []
preLex ('\'':a:'\'':rst) = ['\'',a,'\'']:(preLex rst)
preLex ('\'':'\\':a:'\'':rst) = ['\'',lexEscChar a,'\'']:(preLex rst)
preLex (a:as)
  | isSpace a = preLex as
  | isAlpha a = let (tk,rst) = break (not.isTokenable) as in (a:tk):(preLex rst)
  | a == '\"' = let (tk,rst) = readQuote as in 
  case rst of
    ('\"':rst1) -> ([a] ++ tk ++ ['\"']):(preLex rst1)
    _          -> ["~"]
  | isSingletonTok a = [a]:(preLex as)
  | isDigit a = let (tk,rst) = break (not.isDigit) as 
                in case rst of
                  ('.':b:rst0) 
                    | isDigit b -> let (tk0,rst1) = break (not.isDigit) rst0 in ((a:tk) ++ "." ++ (b:tk0)):(preLex rst1)
                  _             -> (a:tk):(preLex rst)
  | isInfix a = let (tk,rst) = break (not.isInfix) as in (a:tk):(preLex rst)
  | otherwise = ["~"]
                

lexTree str = fmap fst $ lex_ TopLevel [] $ preLex str

lex_ level res (('\"':q):rst) = lex_ level ((Quote ('\"':q)):res) rst
lex_ level res (['\'',q,'\'']:rst) = lex_ level ((CharLit q):res) rst
lex_ _ _ ("~":_) = Nothing
lex_ level res ([a]:as)
  | isLeftBrace a  = (lex_ (leftBraceLevel a) [] as) >>= (\(nd,rst) -> lex_ level (nd:res) rst)
  | isRightBrace a = (guard $ (rightBraceLevel a) == level) >> (Just (Node level (reverse res),as))
lex_ level res (a:as) = lex_ level ((token a):res) as
lex_ TopLevel res [] = Just (Node TopLevel $ reverse res,[])
lex_ _ _ [] = Nothing

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

parseTypeDec :: [LexTree] -> Maybe TypeDec
parseTypeDec ((Ident a):ColonToken:tp) = fmap (TypeDec a) $ parseType tp
parseTypeDec _ = Nothing

parseType :: [LexTree] -> Maybe HaskellType
parseType [Ident tp] = Just $ BaseType tp
parseType [Node Paren tps] = fmap TupType $ mapM parseType $ sepBy (==CommaToken) tps
parseType [Node Bracket tp] = fmap tc_List $ parseType tp
parseType _ = Nothing

parseSimplex :: [LexTree] -> Maybe [Name]
parseSimplex [Node Curly verts] = mapM identName $ concat $ sepBy (==CommaToken) verts
parseSimplex _ = Nothing

parseSimplices (Node Curly simps) = mapM parseSimplex $ sepBy (==CommaToken) simps 

createInstanceFunctions :: [[LexTree] -> Maybe CreateQuery]
createInstanceFunctions = [parseInstantiateSchema,parseFilter,parseFunctor,parseUnion]

findSuccessIn :: a -> [a -> Maybe b] -> Maybe b
findSuccessIn a fns = join $ find isJust $ map ($a) fns

parseLetName :: LexTree -> Maybe ClientQuery
parseLetName (Node TopLevel (LetToken:(Ident name):EqToken:create)) = do
  createQuery <- parseCreateQuery create
  return $ LetQuery name createQuery
parseLetName (Node TopLevel (LetToken:FreshToken:EqToken:create)) = do
  createQuery <- parseCreateQuery create
  return $ LetQuery "" createQuery
parseLetName _ = Nothing

parseCreateSchema :: [LexTree] -> Maybe CreateQuery
parseCreateSchema [CreateToken,SchemaToken,WithToken,
                   VerticesToken,EqToken,Node Curly verts,
                   SimplicesToken,EqToken,simps] = do
  vertdecs <- mapM parseTypeDec $ sepBy (==CommaToken) verts
  simpdecs <- parseSimplices simps
  return $ CreateSchema vertdecs simpdecs
parseCreateSchema _ = Nothing

parseSchemaQuery :: LexTree -> Maybe SchemaQuery
parseSchemaQuery (Ident a) =  Just $ NamedSchema a
parseSchemaQuery (Node Paren [SchemaToken,instanceQuery]) = fmap SchemaOf $ parseInstanceQuery instanceQuery
parseSchemaQuery _ = Nothing

parseInstanceQuery :: LexTree -> Maybe InstanceQuery
parseInstanceQuery (Ident a) =  Just $ NamedInstance a
parseInstanceQuery (Node Paren [createQuery]) = parseInstanceQuery createQuery 
parseInstanceQuery (Node Paren createQuery) = fmap CreateInstance $ findSuccessIn createQuery createInstanceFunctions
parseInstanceQuery _ = Nothing

parseMapQuery :: LexTree -> Maybe MapQuery
parseMapQuery (Ident a) = Just $ NamedMap a
parseMapQuery _ = Nothing

parseDatum :: [LexTree] -> Maybe (Maybe String)
parseDatum [Ident a] = Just $ Just a
parseDatum [NullToken] = Just Nothing
parseDatum [Quote q] = Just $ Just q
parseDatum [CharLit c] = Just $ Just $ show c
parseDatum [Ident "-",Ident a] = Just $ Just $ '-':a
parseDatum [Node nType dts] 
  | nType`elem`[Paren,Bracket] = do
    ls <- mapM parseDatum $ sepBy (==CommaToken) dts
    guard $ all isJust ls
    return $ Just $ (leftBraceStr nType) ++ (cim "," fromJust ls) ++ (rightBraceStr nType)
parseDatum  _         = Nothing


parseData :: [LexTree] -> Maybe DataQuery
parseData [LoadToken,Quote filename] = Just $ LoadCSV $ read filename
parseData selectQuery@(SelectToken:_) = fmap SelectData $ parseSelect selectQuery
parseData dats = do
  tups <- forM dats $ \x -> do
    case x of
      (Node Paren items) -> do
        forM (sepBy (==CommaToken) items) parseDatum 
      _                  -> Nothing
  return $ ExplicitTuples tups

parseInstantiateSchema :: [LexTree] -> Maybe CreateQuery
parseInstantiateSchema (InstantiateToken:schemaQuery:AtToken:simplex:WithToken:dataQuery) = do
  sq <- parseSchemaQuery schemaQuery
  simp <- parseSimplex [simplex]
  dat <- parseData dataQuery
  return $ InstantiateSchema sq simp dat
parseInstantiateSchema _ = Nothing

-- TODO: generalize this to all infix expressions, should require modifying something
parseExpression :: [LexTree] -> Maybe HaskellCode
parseExpression [Ident a] = Just $ Lit a
parseExpression [Node Paren expr] = do 
  exp <- mapM parseExpression $ sepBy (==CommaToken) expr
  case exp of
    [x] -> return x
    exps -> return $ Tpl exps
parseExpression [Node Bracket expr] = fmap Lst $ mapM parseExpression $ sepBy (==CommaToken) expr
parseExpression [Ident "-",x] = (Just $ App (Lit "-")) `ap` (fmap return $ parseExpression [x])
parseExpression (LambdaToken:rst) = do
  let (pat,exp) = break (==ArrowToken) rst
  guard $ not $ null exp
  pattern0 <- mapM (parsePattern.return) pat
  let pattern = case pattern0 of
        [p] -> p
        ps -> Mlp ps
  fmap (Lam pattern) $ parseExpression $ tail exp
parseExpression [Quote q] = Just $ SLit q
parseExpression [CharLit q] = Just $ CLit q
parseExpression [x] = Nothing
parseExpression [l_expr,Ident infixOp,r_expr] 
  | all isInfix infixOp = parseExpression [Ident $ "(" ++ infixOp ++ ")",l_expr,r_expr]
parseExpression (f:rst) = (Just App) `ap` (parseExpression [f]) `ap` (mapM (parseExpression.return) rst)
parseExpression [] = Nothing

parsePattern :: [LexTree] -> Maybe HaskellPattern
parsePattern [Ident a] = Just $ Ltp a
parsePattern [Quote q] = Just $ Ltp $ q
parsePattern [Node Paren expr] = do
  pat <- mapM parsePattern $ sepBy (==CommaToken) expr
  case pat of
    [x] -> return x
    xs  -> return $ Tup xs
parsePattern [Node Bracket expr] = fmap Lstp $ mapM parsePattern $ sepBy (==CommaToken) expr
parsePattern [UnderScoreToken] = Just USp
parsePattern ((Ident f):rst) = fmap (Fnp f) $ mapM (\x -> parsePattern [x]) rst
parsePattern _ = Nothing


parseFilter :: [LexTree] -> Maybe CreateQuery
parseFilter (FilterToken:instanceQuery:ByToken:expr) = do
  inst  <- parseInstanceQuery instanceQuery
  filtfun <- parseExpression expr
  return $ FilterQuery inst filtfun
parseFilter _ = Nothing

parseUnion :: [LexTree] -> Maybe CreateQuery
parseUnion (UnionToken:instanceQueries) = do
  instances <- mapM (liftToSingletonList parseInstanceQuery) $ sepBy (==CommaToken) instanceQueries
  return $ UnionQuery instances
parseUnion _ = Nothing

parseFunctor :: [LexTree] -> Maybe CreateQuery                
parseFunctor [ShriekToken,mapQuery,WithToken,instanceQuery] = createFunctorQuery ShriekFunctor mapQuery instanceQuery
parseFunctor [DirectToken,ImageToken,mapQuery,WithToken,instanceQuery] = createFunctorQuery DirectImageFunctor mapQuery instanceQuery
parseFunctor [InverseToken,ImageToken,mapQuery,WithToken,instanceQuery] = createFunctorQuery InverseImageFunctor mapQuery instanceQuery
parseFunctor _ = Nothing
  
parseOnConnect :: [LexTree] -> Maybe CreateQuery
parseOnConnect (OnToken:connectQuery:createQuery) = do
  conn <- parseConnectQuery connectQuery
  return $ OnConnection conn $ show $ Node TopLevel createQuery
parseOnConnect _ = Nothing

parseConnectQuery :: LexTree -> Maybe ConnectQuery
parseConnectQuery (Ident a) = Just $ NamedConnect a
parseConnectQuery (Node Paren c) = do
  (ConnectQuery a b) <- parseConnect c
  return $ AddressedConnect a b

createFunctorQuery fType mapQuery instanceQuery = do
  inst  <- parseInstanceQuery instanceQuery
  mp    <- parseMapQuery mapQuery
  return $ FunctorQuery fType mp inst
    
parseMapDef :: [LexTree] -> Maybe (Name,Name,HaskellCode)
parseMapDef [Ident a,ArrowToken,Ident b] = parseMapDef [Ident a,ArrowToken,Ident b,ByToken,Ident b]
parseMapDef ((Ident a):ArrowToken:(Ident b):ByToken:expr) = fmap (\f -> (a,b,Lam (Ltp b) f)) $ parseExpression expr
parseMapDef _ = Nothing
                                                            
parseCreateMap :: [LexTree] -> Maybe CreateQuery
parseCreateMap [CreateToken,MapToken,srcQuery,ArrowToken,trgQuery,WithToken,Node Curly defs] = do
  src <- parseSchemaQuery srcQuery
  trg <- parseSchemaQuery trgQuery
  mp <- mapM parseMapDef $ sepBy (==CommaToken) defs
  return $ CreateMap src trg mp
parseCreateMap _ = Nothing

parseConnect [ConnectToken,ToToken,Quote addrAndPort] = case break (==':')  addrAndPort of
  ('\"':addr,':':port) 
    | all isDigit $ init port -> Just $ ConnectQuery addr $ read $ init port
  _ -> Nothing
parseConnect _ = Nothing

parseSelect :: [LexTree] -> Maybe ClientQuery
parseSelect [SelectToken,simplices,FromToken,instanceQuery] = do
  simps <- parseSimplices simplices
  inst  <- parseInstanceQuery instanceQuery
  return $ SelectQuery simps inst
parseSelect _ = Nothing

parseName [Ident a] = Just $ NamedObject a
parseName _ = Nothing


parseCreateQuery create = findSuccessIn create $ createInstanceFunctions ++ 
                          [parseName,parseCreateSchema,parseCreateMap,parseConnect,
                           parseOnConnect,
                           liftToSingletonList $ (fmap SchemaQuery).parseSchemaQuery]

parseShow (Node TopLevel (ShowToken:create)) = do
  createQuery <- parseCreateQuery create
  return $ ShowQuery createQuery

parseShow _ = Nothing

parseSpecial (Node TopLevel [ClearToken,CacheToken]) = Just ClearCache
parseSpecial (Node TopLevel [ClearToken,DataToken]) = Just ClearData
--parseSpecial (Node TopLevel [KILLToken,ServerToken]) = Just KILLServer
parseSpecial (Node TopLevel [QuitToken]) = Just Quit
parseSpecial _ = Nothing

parse :: String -> Maybe ClientQuery
parse str = do
  lx <- lexTree str
  findSuccessIn lx [parseLetName,parseShow,parseSelect . fromTopLevel,parseSpecial]
  
