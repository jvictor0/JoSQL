{-# LANGUAGE DeriveGeneric #-}
module HaskellCode where

import Utils
import Types

import Data.List
import Data.Char
import GHC.Generics
import Data.Serialize

-- Warning: not gaurenteed to create syntatically correct Haskell.
-- For instance, A case statement with a multi-pattern is not valid Haskell, nor is a where statement inside a function application.  

data HaskellType = BaseType Type | FunType [HaskellType] HaskellType | TupType [HaskellType] | ConsType Type [HaskellType]
                 deriving (Eq,Ord, Generic)
data HaskellFunction = Fun Name HaskellType HaskellCode 
                     deriving (Eq, Generic)
data HaskellCode = Lit String 
                 | Whr HaskellCode [(HaskellPattern,Either HaskellType HaskellCode)]
                 | If HaskellCode HaskellCode HaskellCode
                 | Lam HaskellPattern HaskellCode
                 | App HaskellCode [HaskellCode]
                 | Lst [HaskellCode]
                 | Tpl [HaskellCode]
                 | Do [(HaskellPattern,HaskellCode)]
                 | SLit String
                 | CLit Char
                 | LComp HaskellCode [(HaskellPattern,HaskellCode)] [HaskellCode]
                 deriving (Eq, Generic)
data HaskellPattern = Mlp [HaskellPattern] -- toplevel only
                    | Ltp String
                    | Tup [HaskellPattern]
                    | Lstp [HaskellPattern]
                    | Fnp Name [HaskellPattern] 
                    | FnpNP Name [HaskellPattern] 
                    | USp
                    deriving (Eq, Generic)
                   
instance Serialize HaskellType
instance Serialize HaskellFunction
instance Serialize HaskellCode
instance Serialize HaskellPattern
                             
tab = "  "
                   
showI :: (Show a) => Int -> a -> String
showI n x = concatMap (\ch -> if ch == '\n' then "\n" ++ (replicate (2*n) ' ') else [ch]) $ show x
      
instance Show HaskellType where
  show (BaseType t) = t
  show (FunType args result) = "(" ++ (concatMap (\a -> (show a) ++ " -> ") args) ++ (show result) ++ ")" 
  show (TupType ts) = "(" ++ (cim "," show ts) ++ ")"
  show (ConsType "[]" ts) = "[" ++ (cim "," show ts) ++ "]"
  show (ConsType c ts) = "(" ++ c ++ " " ++ (cim " " show ts) ++ ")"

instance Show HaskellFunction where
  show (Fun name t (Lam pat body)) = name ++ " :: " ++ (show t) ++ "\n" ++ 
                                     name ++ " " ++ (show pat) ++ " = \n  " ++ (showI 1 body) ++ "\n"
  show (Fun name t c) = name ++ " :: " ++ (show t) ++ "\n" ++
                        name ++ " =\n  " ++ (showI 1 c) ++ "\n"
                        
maxFunAppLen = 50

parensBalanced x = pb x 0
  where pb _ (-1) = False
        pb [] n = n == 0
        pb ('(':as) n = pb as (n+1)
        pb (')':as) n = pb as (n-1)
        pb (a:as) n = pb as n

paren x 
  | all (not.isSpace) x = x
  | (head x == '(') && (last x == ')') && (parensBalanced $ tail $ init $ x) = x
  | otherwise           = "(" ++ x ++ ")"
                        
instance Show HaskellCode where
  show (Lit s) = s
  show (SLit s) = show s
  show (CLit c) = show c
  show (Whr c whrlst) = (show c) ++ "\n" ++ tab ++ "where\n" ++ 
                        (concatMap (\(pat,tp) -> (case tp of
                                                     (Left t) -> tab ++ tab ++ (show pat) ++ " :: " ++ (show t) ++ "\n"
                                                     (Right cde) -> tab ++ tab ++ (show pat) ++ " = \n      " ++ (showI 3 cde) ++ "\n"))
                         whrlst)
  show (If con thn els) = "if    " ++ (showI 3 con) ++ "\nthen  " ++ (showI 3 thn) ++ "\nelse  " ++ (showI 3 els)
  show (Lam pat cde) = if length showbod < maxFunAppLen 
                       then "(\\" ++ (show pat) ++ " -> " ++ (show cde) ++ ")"
                       else "(\\" ++ (show pat) ++ " ->\n  " ++ (showI 1 cde) ++ ")"
    where showbod = show cde
  show (App fun args) = if ((sum $ map length $ showfun:showargs) < maxFunAppLen) && (not $ '\n'`elem`(concat $ showfun:showargs))
                        then (paren showfun) ++ " " ++ (cim " " (\arg -> (paren arg)) showargs)
                        else (paren showfun) ++ "\n" ++ (concatMap (\arg -> "  " ++ (paren $ showI 1 arg) ++ "\n") args)
    where showfun = show fun
          lenfun = length $ showfun
          showargs = map show args
  show (Lst hsks) = if ((sum $ map length $ showargs) < maxFunAppLen) && (not $ '\n'`elem`(concat $ showargs))
                    then "[" ++ (concat $ intersperse "," $ showargs) ++"]"
                    else "[\n" ++ (cim ",\n" (\arg -> "  " ++ (showI 1 arg)) hsks) ++"\n]"
    where showargs = map show hsks
  show (Tpl hsks) = if ((sum $ map length $ showargs) < maxFunAppLen) && (not $ '\n'`elem`(concat $ showargs))
                    then "(" ++ (concat $ intersperse "," $ showargs) ++")"
                    else "(\n" ++ (cim ",\n" (\arg -> "  " ++ (showI 1 arg)) hsks) ++"\n)"
    where showargs = map show hsks
  show (Do hasks) = "do\n" ++ 
                    (cim "\n" (\(p,d) -> if p == USp then "  " ++ (showI 1 d) else "  " ++ (show p) ++ " <- " ++ (showI 3 d)) hasks)

  show (LComp tp pats conds) =  "[\n  " ++ (showI 1 tp) ++ "\n  |\n" ++ 
                                (cim ",\n" (\(p,t) -> "  " ++ (show p) ++ " <- " ++ (showI 2 t)) pats) ++ 
                                (if null conds then "" else ",\n") ++ 
                                (cim ",\n" (\arg -> "  " ++ (showI 1 arg)) conds) ++
                                "\n]"

instance Show HaskellPattern where
  show (Mlp pats) = cim " " show pats
  show (Ltp l) = l
  show (Tup pats) = "(" ++ (cim "," show pats) ++ ")"
  show (Lstp pats) = "[" ++ (cim "," show pats) ++ "]"
  show (Fnp name pats) = "(" ++ name ++ " " ++ (cim " " show pats) ++ ")"
  show (FnpNP name pats) = name ++ " " ++ (cim " " show pats)
  show USp = "_"
  
--- TYPE CONVINIENCE FUNCTIONS ---
tc_1 t x = ConsType t [x]
tc_2 t x y = ConsType t [x,y]
tc_List x = ConsType "[]" [x]
tc_Maybe x = ConsType "Maybe" [x]
tc_Either x y = ConsType "Either" [x,y]
tc_IO x = ConsType "IO" [x]

t_ = BaseType
t_Int = t_ "Int"
t_Double = t_ "Double"
t_String = t_ "String"
t_ByteString = t_ "ByteString"
t_LazyByteString = t_ "LazyByteString"

t_NutleyInstance = t_ "NutleyInstance"


--- Functional Convinience Type
x +$+ y = App (Lit "($)") [x,y]
x +==+ y = App (Lit "(==)") [x,y]
x +.+ y = App (Lit "(.)") [x,y]
x +++ y = App (Lit "(+)") [x,y]
x +-+ y = App (Lit "(-)") [x,y]
x +*+ y = App (Lit "(*)") [x,y]
x +/+ y = App (Lit "(/)") [x,y]
x ++++ y = App (Lit "(++)") [x,y]
x +>>=+ y = App (Lit "(>>=)") [x,y]
x +=<<+ y = App (Lit "(=<<)") [x,y]


f $$ lst = App f lst

c_1 f x = (Lit f) $$ [x]
c_2 f x y = (Lit f) $$ [x,y]
c_3 f x y z = (Lit f) $$ [x,y,z]
c_4 f x y z w = (Lit f) $$ [x,y,z,w]

c_return = c_1 "return"

c_map = c_2 "map"
c_foldr = c_3 "foldr"
c_fmap = c_2 "fmap"
c_filter = c_2 "filter"
c_zip = c_2 "zip"
c_mapM = c_2 "mapM"
c_mapM_ = c_2 "mapM_"
c_mapMaybe = c_2 "mapMaybe"
c_concat = c_1 "concat"

do_1 f x = (USp,c_1 f x)

do_return = do_1 "return"