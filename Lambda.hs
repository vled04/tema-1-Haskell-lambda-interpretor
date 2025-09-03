module Lambda where

import Data.List (nub, (\\), sort)

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars = nub . getString
  where
    getString :: Lambda -> [String]
    getString (Var x) = [x]
    getString (App e1 e2) = (getString e1) ++ (getString e2)
    getString (Abs x e) = x : getString e


-- 1.2.
freeVars :: Lambda -> [String]
freeVars = nub . getString
  where 
    getString :: Lambda -> [String]
    getString (Var x) = [x]
    getString (App e1 e2) = (getString e1) ++ (getString e2)
    getString (Abs x e) = filter (/=x) (getString e)

-- 1.3.

nextString :: String -> String
nextString string = reverse (inc (reverse string))
  where
    inc :: String -> String
    inc [] = "a"
    inc (e:str)
      | e == 'z'  = 'a' : (inc str)
      | otherwise = (succ e) : str

newVar :: [String] -> String
newVar list = search "a" (take (length list) (repeat 'z'))
  where
    search :: String -> String -> String
    search lft rght = 
      if lft `notElem` sort (list) then lft
      else search (nextString lft) rght
      


-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App e1 e2) = (isNormalForm e1) && (isNormalForm e2)
isNormalForm (Abs str lambda) = isNormalForm lambda

-- 1.5.

check :: String -> Lambda -> Bool
check x (Var y) = if(x == y) then True else False
check x (Abs str lambda) = if(str == x) then False else check x lambda
check x (App e1 e2) = (check x e1) || (check x e2)

substitute :: String -> Lambda -> Lambda ->Lambda
substitute (x) (Var e1) (e2)
  | e1 == x = e2
  | otherwise = (Var e1)
substitute (x) (Abs str lambda) (e2)
  | str /= x = Abs str (substitute x lambda e2)
  | otherwise = Abs str lambda
substitute (x) (App e11 e12) (e2) = App (substitute x e11 e2) (substitute x e12 e2)

reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = 
  if (check x e1) == False then e1
  else (substitute x e1 e2)
    



-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (App (Abs x lambda1) lambda2) = reduce x lambda1 lambda2
normalStep (App e1 e2) =
  let newE1 = normalStep e1 
  in if (newE1 /= e1) then App newE1 e2 else App e1 (normalStep e2)
normalStep (Abs x lambda) = Abs x (normalStep lambda)
normalStep (Var x) = Var x

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (App (Abs x lambda1) lambda2) = reduce x lambda1 lambda2
applicativeStep (App e1 e2) =
  let newE2 = applicativeStep e2 
  in if (newE2 /= e2) then App e1 newE2 else App (applicativeStep e1) e2
applicativeStep (Abs x lambda) = Abs x (applicativeStep lambda)
applicativeStep (Var x) = Var x

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify function lambda
  | isNormalForm lambda == True = [lambda]
  | otherwise = lambda : (simplify function (function lambda))

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
