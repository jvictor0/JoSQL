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
             | CreateToken
             | SchemaToken
             | InstantiateToken
             | WithToken
             | AtToken
             | VerticesToken
             | SimplicesToken
             | ShowToken
             | NullToken
             | EqToken
             | CommaToken
             | ColonToken
             | Node NodeType [LexTree] 
             deriving (Eq)

instance Show LexTree where
  show (Ident s) = s
  show LetToken = "let"
  show CreateToken = "create"
  show InstantiateToken = "instantiate"
  show ShowToken = "show"                     
  show SchemaToken = "schema"
  show WithToken = "with"
  show AtToken = "at"
  show VerticesToken = "vertices"
  show SimplicesToken = "simplices"
  show NullToken = "null" 
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
token "let" = LetToken
token "instantiate" = InstantiateToken
token "schema" = SchemaToken
token "with" = WithToken
token "at" = AtToken
token "show" = ShowToken
token "vertices" = VerticesToken
token "simplices" = SimplicesToken
token "null" = NullToken
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

parseLetName :: LexTree -> Maybe ClientQuery
parseLetName (Node TopLevel (LetToken:(Ident name):EqToken:create)) = do
  createQuery <- join $ find isJust $ map ($create) [parseCreateSchema,parseInstantiateSchema]
  return $ LetQuery name createQuery
parseLetName _ = Nothing

parseCreateSchema :: [LexTree] -> Maybe CreateQuery
parseCreateSchema [CreateToken,SchemaToken,WithToken,
                   VerticesToken,EqToken,Node Curly verts,
                   SimplicesToken,EqToken,Node Curly simps] = do
  vertdecs <- mapM parseTypeDec $ sepBy (==CommaToken) verts
  simpdecs <- mapM parseSimplex $ sepBy (==CommaToken) simps
  return $ CreateSchema vertdecs simpdecs
parseCreateSchema _ = Nothing

parseSchemaQuery :: LexTree -> Maybe SchemaQuery
parseSchemaQuery (Ident a) =  Just $ NamedSchema a
parseSchemaQuery _ = Nothing

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

parseShow (Node TopLevel [ShowToken,Ident name]) = Just $ Show name
parseShow _ = Nothing


parse :: String -> Maybe ClientQuery
parse str = do
  (lx,_) <- lex str
  join $ find isJust $ map ($lx) [parseLetName,parseShow]
  
