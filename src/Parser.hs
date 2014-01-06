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
             | CreateToken
             | SchemaToken
             | InstantiateToken
             | WithToken
             | AtToken
             | VerticesToken
             | SimplicesToken
             | ShowToken
             | EqToken
             | CommaToken
             | ColonToken
             | Node NodeType [LexTree] 
             deriving (Eq)

instance Show LexTree where
  show (Ident s) = s
  show CreateToken = "create"
  show InstantiateToken = "instantiate"
  show ShowToken = "show"                     
  show SchemaToken = "schema"
  show WithToken = "with"
  show AtToken = "at"
  show VerticesToken = "vertices"
  show SimplicesToken = "simplices"
  show EqToken = "="
  show CommaToken = ","
  show ColonToken = ":"
  show (Node t lvs) = (leftBraceStr t) ++ (cim " " show lvs) ++ (rightBraceStr t)

isLeftBrace x = x`elem`"\"([{"
isRightBrace x = x`elem`"\")]}"
isBrace x = isLeftBrace x || isRightBrace x 
leftToRightBrace x = fromJust $ lookup x $ zip "\"([{" "\")]}"
rightToLeftBrace x = fromJust $ lookup x $ zip "\")]}" "\"([{"

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

token ":" = ColonToken
token "," = CommaToken
token "=" = EqToken
token "create" = CreateToken
token "instantiate" = InstantiateToken
token "schema" = SchemaToken
token "with" = WithToken
token "at" = AtToken
token "show" = ShowToken
token "vertices" = VerticesToken
token "simplices" = SimplicesToken
token str = Ident str

identName (Ident n) = Just n
identName _ = Nothing

isInfix a = (isPunctuation a) || (isSymbol a)

preLex str = concatMap (groupBy (\x y -> (not $ isInfix x) && (not $ isInfix y))) $ words str

lex str = lex_ TopLevel [] $ preLex str

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
parseType _ = Nothing

parseSimplex :: [LexTree] -> Maybe [Name]
parseSimplex [Node Curly verts] = mapM identName $ concat $ sepBy (==CommaToken) verts
parseSimplex _ = Nothing

parseCreateSchema :: LexTree -> Maybe ClientQuery
parseCreateSchema (Node TopLevel [CreateToken,SchemaToken,Ident name,WithToken,
                                  VerticesToken,EqToken,Node Curly verts,
                                  SimplicesToken,EqToken,Node Curly simps]) = do
  vertdecs <- mapM parseTypeDec $ sepBy (==CommaToken) verts
  simpdecs <- mapM parseSimplex $ sepBy (==CommaToken) simps
  return $ CreateSchema name vertdecs simpdecs
parseCreateSchema _ = Nothing

parseSchemaQuery :: LexTree -> Maybe SchemaQuery
parseSchemaQuery (Ident a) =  Just $ NamedSchema a
parseSchemaQuery _ = Nothing

parseData :: LexTree -> Maybe DataQuery
parseData (Node Bracket dats) = do
  tups <- forM dats $ \x -> do
    case x of
      (Node Paren items) -> forM (sepBy (==CommaToken) items) $ \i -> do
        guard $ length i == 1
        identName $ head i
      _                  -> Nothing
  return $ ExplicitTuples tups
parseData _ = Nothing

parseInstantiateSchema :: LexTree -> Maybe ClientQuery
parseInstantiateSchema (Node TopLevel [InstantiateToken, schemaQuery, AtToken, simplex, WithToken, dataQuery]) = do
  sq <- parseSchemaQuery schemaQuery
  simp <- parseSimplex [simplex]
  dat <- parseData dataQuery
  return $ InstantiateSchema sq simp dat

parseShow (Node TopLevel [ShowToken,Ident name]) = Just $ Show name
parseShow _ = Nothing

parse :: String -> Maybe ClientQuery
parse str = do
  (lx,_) <- lex str
  join $ find isJust $ map ($lx) [parseCreateSchema,parseShow,parseInstantiateSchema]