data Expr
 = Val Int
 | Add Expr Expr
 deriving (Eq, Show)

type Stack = [Int]

data Code = HALT | PUSH Int Code | ADD Code
 deriving (Eq, Show)

eval          :: Expr -> Int
eval (Val i)   = i
eval (Add x y) = eval x + eval y

exec                    :: Code -> Stack -> Stack
exec HALT s              = s
exec (PUSH n c) s        = exec c (n : s)
exec (ADD c) (n : m : s) = exec c (n + m : s)

comp             :: Expr -> Code
comp e            = comp' e HALT

comp'            :: Expr -> Code -> Code
comp' (Val n) c   = PUSH n c
comp' (Add x y) c = comp' x (comp' y (ADD c))

--

t1 = Val 17
t2 = Add (Val 1) (Val 1)
t3 = Add (Add (Add (Add (Val 1) (Val 2)) (Val 3)) (Val 4)) (Val 5)
t4 = Add (Val 1) (Add (Val 2) (Add (Val 3) (Add (Val 4) (Val 5))))
t5 = Add (Add (Val 5) (Val (-7))) (Add (Val (-5)) (Val 7))
