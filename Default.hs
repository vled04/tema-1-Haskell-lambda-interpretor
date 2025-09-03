module Default where

import Lambda
import Binding

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vm = Var "m"
vn = Var "n"

-- Basic combinators
m = Abs "x" $ App vx vx
i = Abs "x" $ vx
k = Abs "x" $ Abs "y" $ vx
ki = Abs "x" $ Abs "y" $ vy
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y = Abs "f" $ App fix fix
  where fix = Abs "x" $ App vf (App vx vx)

-- 4.1. Boolean encodings
bTrue  = Abs "x" (Abs "y" (Var "x"))                           -- λx.λy.x
bFalse = Abs "x" (Abs "y" (Var "y"))                           -- λx.λy.y
bAnd   = Abs "p" (Abs "q" (App (App (Var "p") (Var "q")) (Var "p")))  -- λp.λq.(p q p)
bOr    = Abs "p" (Abs "q" (App (App (Var "p") (Var "p")) (Var "q")))  -- λp.λq.(p p q)
bNot   = Abs "p" (Abs "t" (Abs "e" (App (App (Var "p") (Var "e")) (Var "t"))))  -- λp.λt.λe.(p e t)
bXor   = Abs "p" (Abs "q" (App (App (Var "p") (App bNot (Var "q"))) (Var "q"))) -- λp.λq.(p (¬q) q)

-- 4.2. Pair encodings
pair   = Abs "a" (Abs "b" (Abs "z" (App (App (Var "z") (Var "a")) (Var "b")))) -- λa.λb.λz.(z a b)
first  = Abs "p" (App (Var "p") bTrue) -- λp.(p TRUE)
second = Abs "p" (App (Var "p") bFalse) -- λp.(p FALSE)
                                          


-- 4.3. Natural number encodings
n0 = undefined
n1 = undefined
n2 = undefined
nSucc = undefined
nPred = undefined
nAdd = undefined
nSub = undefined
nMult = undefined

-- Default Context
defaultContext :: Context
defaultContext = 
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    ,("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]
