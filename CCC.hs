import Control.Applicative hiding ((<|>))
import Data.List
import Text.Parsec

-- Code from http://www.cs.nott.ac.uk/~gmh/ccc.pdf

-- 5.1 Syntax

data Expr = Val Int | Add Expr Expr | Var Int | Abs Expr | App Expr Expr
 deriving (Eq, Show)

-- 5.2 Semantics

data Value = Num Int | Clo Expr Env
 deriving (Eq, Show)
type Env = [Value]

eval             :: Expr -> Env -> Value
eval (Val n) e    = Num n
eval (Add x y) e  = case eval x e of
                      Num n -> case eval y e of
		                 Num m -> Num (n + m)
eval (Var i) e    = e !! i
eval (Abs x) e    = Clo x e
eval (App x y) e = case eval x e of
                     Clo x' e' -> eval x' (eval y e : e')

-- 5.3 Specification

type Conf = (Stack, Env')
type Stack = [Elem]

data Value' = Num' Int | Clo' Code Env'
 deriving (Eq, Show)
type Env' = [Value']

data Code
 = HALT
 | PUSH Int Code
 | ADD Code
 | LOOKUP Int Code
 | ABS Code Code
 | RET
 | APP Code
 deriving (Eq, Show)

comp             :: Expr -> Code
comp x            = comp' x HALT
comp'            :: Expr -> Code -> Code
comp' (Val n) c   = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))
comp' (Var i) c   = LOOKUP i c
comp' (Abs x) c   = ABS (comp' x RET) c
comp' (App x y) c = comp' x (comp' y (APP c))

data Elem = VAL Value' | CLO Code Env'
 deriving (Eq, Show)

exec                    :: Code -> Conf -> Conf
exec HALT (s, e)         = (s, e)
exec (PUSH n c) (s, e)   = exec c (VAL (Num' n) : s, e)
exec (ADD c) (VAL (Num' m) : VAL (Num' n) : s, e)
                         = exec c (VAL (Num' (n + m)) : s, e)
exec (LOOKUP i c) (s, e) = exec c (VAL (e !! i) : s, e)
exec (ABS c' c) (s, e)   = exec c (VAL (Clo' c' e) : s, e)
exec RET (VAL v : CLO c e : s, _)
                         = exec c (VAL v : s, e)
exec (APP c) (VAL v : VAL (Clo' c' e') : s, e)
                         = exec c' (CLO c e : s, v : e')

conv          :: Value -> Value'
conv (Num n)   = Num' n
conv (Clo x e) = Clo' (comp' x RET) (map conv e)

-- binary lambda terms

runParseTerm = runParser (parseTerm <* eof) () ""

parseTerm :: Parsec String () Expr
parseTerm = try parseVariable <|> try parseLambda <|> parseApplication

parseVariable = do
    n <- many1 (char '1')
    char '0'
    return (Var ((length n) - 1))

parseLambda = do
    string "00"
    t <- parseTerm
    return (Abs t)

parseApplication = do
    string "01"
    p <- parseTerm
    q <- parseTerm
    return (App p q)