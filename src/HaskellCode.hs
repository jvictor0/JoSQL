module HaskellCode where

import Utils
import Types

import Data.List
import Data.Char

-- Warning: not gaurenteed to create syntatically correct Haskell.
-- For instance, A case statement with a multi-pattern is not valid Haskell, nor is a where statement inside a function application.  

data HaskellType = BaseType Type | FunType [HaskellType] HaskellType | TupType [HaskellType] | ConsType Type HaskellType
                 deriving (Eq)
data HaskellFunction = Fun Name HaskellType HaskellCode 
                     deriving (Eq)
data HaskellCode = Lit String 
                 | Pat HaskellPattern
                 | Whr HaskellCode [(HaskellPattern,HaskellCode)]
                 | If HaskellCode HaskellCode HaskellCode
                 | Lam HaskellPattern HaskellCode
                 | App HaskellCode [HaskellCode]
                 | Lst [HaskellCode]
                 | Tpl [HaskellCode]
                 | Do [(HaskellPattern,HaskellCode)]
                 deriving (Eq)
data HaskellPattern = Mlp [HaskellPattern] -- toplevel only
                    | Ltp String
                    | Tup [HaskellPattern]
                    | Fnp Name [HaskellPattern] -- used for where statements, not aloud inside lambdas
                    | USp
                    deriving (Eq)
                   
tab = "  "
                   
showI :: (Show a) => Int -> a -> String
showI n x = concatMap (\ch -> if ch == '\n' then "\n" ++ (replicate (2*n) ' ') else [ch]) $ show x
      
instance Show HaskellType where
  show (BaseType t) = t
  show (FunType args result) = "(" ++ (concatMap (\a -> (show a) ++ " -> ") args) ++ (show result) ++ ")" 
  show (TupType ts) = "(" ++ (cim "," show ts) ++ ")"
  show (ConsType "[]" t) = "[" ++ (show t) ++ "]"
  show (ConsType c t) = "(" ++ c ++ " " ++ (show t) ++ ")"

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
  show (Pat p) = show p
  show (Whr c whrlst) = (show c) ++ "\n" ++ tab ++ "where\n" ++ 
                        (concatMap (\(pat,cde) -> tab ++ tab ++ (show pat) ++ " = \n      " ++ (showI 3 cde) ++ "\n") whrlst)
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


instance Show HaskellPattern where
  show (Mlp pats) = cim " " show pats
  show (Ltp l) = l
  show (Tup pats) = "(" ++ (cim "," show pats) ++ ")"
  show (Fnp name pats) = name ++ " " ++ (cim " " show pats)
  show USp = "_"
  
--- TYPE CONVINIENCE FUNCTIONS ---
listTC = ConsType "[]"
tupTC = TupType.(map BaseType)
maybeTC = ConsType "Maybe"
ioTC = ConsType "IO"

--- Functional Convinience Type
x +$+ y = App (Lit "($)") [x,y]
x +.+ y = App (Lit "(.)") [x,y]
x +++ y = App (Lit "(+)") [x,y]
x +-+ y = App (Lit "(-)") [x,y]
x +*+ y = App (Lit "(*)") [x,y]
x +/+ y = App (Lit "(/)") [x,y]
x ++++ y = App (Lit "(++)") [x,y]


f $$ lst = App f lst

c_1 f x = (Lit f) $$ [x]
c_2 f x y = (Lit f) $$ [x,y]
c_3 f x y z = (Lit f) $$ [x,y,z]
c_4 f x y z w = (Lit f) $$ [x,y,z,w]

c_return = c_1 "return"

c_map = c_2 "map"
c_zip = c_2 "zip"
c_mapM_ = c_2 "mapM_"
c_mapMaybe = c_2 "mapMaybe"