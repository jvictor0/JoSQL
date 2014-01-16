module Parser where

import Prelude hiding (lex)

import ClientQueries
import Utils
import HaskellCode
import Types

import Data.Maybe
import Data.List
import Control.Monad
import Data.Char

data NodeType = TopLevel | Quote | Paren | Bracket | Curly deriving (Eq)

data LexTree = Ident String 
             | LetToken
             | ClearToken
             | DataToken
             | CacheToken
             | ServerToken
             | CreateToken
             | SelectToken
             | FilterToken
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
             | VerticesToken
             | SimplicesToken
             | ShowToken
             | NullToken
             | EqToken
             | CommaToken
             | ColonToken
             | ArrowToken               
             | Node NodeType [LexTree] 
             | KILLToken
             | QuitToken
             deriving (Eq)

instance Show LexTree where
  show (Ident s) = s
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
  show DirectToken = "direct"
  show WithToken = "with"
  show AtToken = "at"
  show ByToken = "by"
  show FromToken = "from"               
  show VerticesToken = "vertices"
  show SimplicesToken = "simplices"
  show NullToken = "null" 
  show EqToken = "="
  show CommaToken = ","
  show ArrowToken = "->"
  show ColonToken = "::"
  show KILLToken = "KILL"
  show ClearToken = "clear"
  show CacheToken = "cache"
  show QuitToken = "quit"
  show ServerToken = "server"
  show (Node t lvs) = (leftBraceStr t) ++ (cim " " show lvs) ++ (rightBraceStr t)

isLeftBrace x = x`elem`"\"([{"
isRightBrace x = x`elem`"\")]}"
isBrace x = isLeftBrace x || isRightBrace x 

leftBraceLevel '\"' = Quote
leftBraceLevel '(' = Paren
leftBraceLevel '[' = Bracket
leftBraceLevel '{' = Curly
rightBraceLevel '\"' = Quote
rightBraceLevel ')' = Paren
rightBraceLevel ']' = Bracket
rightBraceLevel '}' = Curly

leftBraceStr TopLevel = ""
leftBraceStr Quote = "\""
leftBraceStr Paren = "("
leftBraceStr Bracket = "["
leftBraceStr Curly = "{"
rightBraceStr TopLevel = ""
rightBraceStr Quote = "\""
rightBraceStr Paren = ")"
rightBraceStr Bracket = "]"
rightBraceStr Curly = "}"

token "::" = ColonToken
token "," = CommaToken
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
token "direct" = DirectToken
token "inverse" = InverseToken
token "show" = ShowToken
token "vertices" = VerticesToken
token "simplices" = SimplicesToken
token "null" = NullToken
token "KILL" = KILLToken
token "server" = ServerToken
token "clear" = ClearToken
token "cache" = CacheToken
token "data" = DataToken
token "quit" = QuitToken
token "filter" = FilterToken
token "by" = ByToken
token str = Ident str

identName (Ident n) = Just n
identName _ = Nothing

isInfix = flip elem "+-*=/<>!,:"

preLex str = concatMap (groupAdjBy (\x y -> sameGroup x y)) $ words str
  where sameGroup x y
          | isBrace x || isBrace y = False
          | x == '-'  && isDigit y = True
          | isInfix x              = isInfix y
          | isDigit x              = isDigit y
          | isAlpha x              = isAlpha y

lex str = fmap fst $ lex_ TopLevel [] $ preLex str

lex_ Quote res ("\"":as) = Just (Node Quote $ reverse res,as)
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

parseLetName :: LexTree -> Maybe ClientQuery
parseLetName (Node TopLevel (LetToken:(Ident name):EqToken:create)) = do
  createQuery <- join $ find isJust $ map ($create) [parseCreateSchema,parseInstantiateSchema,parseFilter,parseCreateMap,parseFunctor]
  return $ LetQuery name createQuery
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
parseSchemaQuery _ = Nothing

parseInstanceQuery :: LexTree -> Maybe InstanceQuery
parseInstanceQuery (Ident a) =  Just $ NamedInstance a
parseInstanceQuery _ = Nothing

parseMapQuery :: LexTree -> Maybe MapQuery
parseMapQuery (Ident a) = Just $ NamedMap a
parseMapQuery _ = Nothing

parseData :: [LexTree] -> Maybe DataQuery
parseData dats = do
  tups <- forM dats $ \x -> do
    case x of
      (Node Paren items) -> do
        forM (sepBy (==CommaToken) items) $ \i -> do
          guard $ length i == 1
          case head i of
            (Ident a) -> Just $ Just a
            NullToken -> Just Nothing
            _         -> Nothing
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
parseExpression [Node Paren expr] = parseExpression expr
parseExpression [Node Bracket expr] = fmap Lst $ mapM parseExpression $ sepBy (==CommaToken) expr
parseExpression [Ident "-",x] = (Just $ App (Lit "-")) `ap` (fmap return $ parseExpression [x])
parseExpression [l_expr,Ident infixOp,r_expr] 
  | all isInfix infixOp = parseExpression [Ident $ "(" ++ infixOp ++ ")",l_expr,r_expr]
parseExpression (f:rst) = (Just App) `ap` (parseExpression [f]) `ap` (mapM (parseExpression.return) rst)

parseFilter :: [LexTree] -> Maybe CreateQuery
parseFilter (FilterToken:instanceQuery:ByToken:expr) = do
  inst  <- parseInstanceQuery instanceQuery
  filtfun <- parseExpression expr
  return $ FilterQuery inst filtfun
parseFilter _ = Nothing

parseFunctor :: [LexTree] -> Maybe CreateQuery                
parseFunctor [ShriekToken,mapQuery,WithToken,instanceQuery] = createFunctorQuery ShriekFunctor mapQuery instanceQuery
parseFunctor [DirectToken,ImageToken,mapQuery,WithToken,instanceQuery] = createFunctorQuery DirectImageFunctor mapQuery instanceQuery
parseFunctor [InverseToken,ImageToken,mapQuery,WithToken,instanceQuery] = createFunctorQuery InverseImageFunctor mapQuery instanceQuery
parseFunctor _ = Nothing
  
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

parseSelect :: LexTree -> Maybe ClientQuery
parseSelect (Node TopLevel [SelectToken,simplices,FromToken,instanceQuery]) = do
  simps <- parseSimplices simplices
  inst  <- parseInstanceQuery instanceQuery
  return $ SelectQuery simps inst
parseSelect _ = Nothing

parseShow (Node TopLevel [ShowToken,Ident name]) = Just $ ShowQuery name
parseShow _ = Nothing

parseSpecial (Node TopLevel [ClearToken,CacheToken]) = Just ClearCache
parseSpecial (Node TopLevel [ClearToken,DataToken]) = Just ClearData
--parseSpecial (Node TopLevel [KILLToken,ServerToken]) = Just KILLServer
parseSpecial (Node TopLevel [QuitToken]) = Just Quit
parseSpecial _ = Nothing

parse :: String -> Maybe ClientQuery
parse str = do
  lx <- lex str
  join $ find isJust $ map ($lx) [parseLetName,parseShow,parseSelect,parseSpecial]
  
