{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Monad.State

import qualified CCC as C
import CCC(Expr(..))

type CodePtr = Int

data INST l
 = HALT
 | PUSH Int
 | ADD
 | LOOKUP Int
 | ABS l
 | RET
 | APP

 | LABEL l
 deriving (Eq, Show)

newtype Codegen l a = Codegen { unCodegen :: State (Int,Int,[INST l]) a }
 deriving Monad

emit i = do (l,idx,m) <- Codegen get
       	    Codegen (put (l,idx+1,i:m))
	    return ()

freshLabel = do (l,idx,m) <- Codegen get
       	        Codegen (put (l+1,idx,m))
	        return l

runCodegen m = reverse . (\(_,_,a)->a) . snd $ runState (unCodegen m) (0,0,[])

-----------

flat C.HALT = emit HALT
flat (C.PUSH i c) = do emit (PUSH i) ; flat c
flat (C.ADD c) = do emit ADD ; flat c
flat (C.LOOKUP i c) = do emit (LOOKUP i) ; flat c
flat (C.ABS k c) = do
 l <- freshLabel
 emit (ABS l)
 flat c
 emit (LABEL l)
 flat k
flat C.RET = do emit RET
flat (C.APP c) = do emit APP ; flat c

---

assemble _ [] = []
assemble i (LABEL l:cs) = (l,i) : assemble i cs
assemble i (_:cs) = assemble (i+1) cs

assemble' cod = map go . filter notLabel $ cod
 where labels = assemble 0 cod
       notLabel (LABEL _) = False
       notLabel _ = True
       go (ABS l) = ABS (fromJust . lookup l $ labels)
       go i = i
       fromJust (Just j) = j

{-

*Main> let t = App (App (Abs (App (Var 0) (Var 0))) (Abs (Abs (Abs (App (App (App (Var 0) (Abs (Abs (Abs (Abs (App (Var 2) (Abs (App (App (Var 4) (App (Var 2) (Abs (App (App (Var 1) (App (Var 2) (Abs (Abs (App (Var 2) (Abs (App (App (Var 0) (Var 1)) (Var 2)))))))) (App (Var 3) (Abs (App (Var 3) (Abs (App (App (Var 2) (Var 0)) (App (Var 1) (Var 0))))))))))) (App (App (Var 0) (App (Var 1) (Abs (App (Var 0) (Var 1))))) (Abs (App (App (Var 3) (Abs (App (Var 3) (Abs (App (Var 1) (App (Var 0) (Var 3))))))) (Var 4)))))))))))) (App (Var 2) (Var 2))) (Var 1)))))) (Abs (App (Var 0) (App (Abs (App (Var 0) (Var 0))) (Abs (App (Var 0) (Var 0))))))
*Main> runCodegen . flat . C.comp $ t
[ABS 0,ABS 1,APP,ABS 2,APP,HALT,LABEL 2,LOOKUP 0,ABS 3,ABS 4,APP,APP,RET,LABEL 4,LOOKUP 0,LOOKUP 0,APP,RET,LABEL 3,LOOKUP 0,LOOKUP 0,APP,RET,LABEL 1,ABS 5,RET,LABEL 5,ABS 6,RET,LABEL 6,LOOKUP 0,ABS 7,APP,LOOKUP 2,LOOKUP 2,APP,APP,LOOKUP 1,APP,RET,LABEL 7,ABS 8,RET,LABEL 8,ABS 9,RET,LABEL 9,ABS 10,RET,LABEL 10,LOOKUP 2,ABS 11,APP,RET,LABEL 11,LOOKUP 4,LOOKUP 2,ABS 12,APP,APP,LOOKUP 0,LOOKUP 1,ABS 13,APP,APP,ABS 14,APP,APP,RET,LABEL 14,LOOKUP 3,ABS 15,APP,LOOKUP 4,APP,RET,LABEL 15,LOOKUP 3,ABS 16,APP,RET,LABEL 16,LOOKUP 1,LOOKUP 0,LOOKUP 3,APP,APP,RET,LABEL 13,LOOKUP 0,LOOKUP 1,APP,RET,LABEL 12,LOOKUP 1,LOOKUP 2,ABS 17,APP,APP,LOOKUP 3,ABS 18,APP,APP,RET,LABEL 18,LOOKUP 3,ABS 19,APP,RET,LABEL 19,LOOKUP 2,LOOKUP 0,APP,LOOKUP 1,LOOKUP 0,APP,APP,RET,LABEL 17,ABS 20,RET,LABEL 20,LOOKUP 2,ABS 21,APP,RET,LABEL 21,LOOKUP 0,LOOKUP 1,APP,LOOKUP 2,APP,RET,LABEL 0,LOOKUP 0,LOOKUP 0,APP,RET]
*Main> assemble' . runCodegen . flat . C.comp $ t
[ABS 133,ABS 23,APP,ABS 6,APP,HALT,LOOKUP 0,ABS 18,ABS 13,APP,APP,RET,LOOKUP 0,LOOKUP 0,APP,RET,LOOKUP 0,LOOKUP 0,APP,RET,ABS 26,RET,ABS 29,RET,LOOKUP 0,ABS 40,APP,LOOKUP 2,LOOKUP 2,APP,APP,LOOKUP 1,APP,RET,ABS 43,RET,ABS 46,RET,ABS 49,RET,LOOKUP 2,ABS 54,APP,RET,LOOKUP 4,LOOKUP 2,ABS 93,APP,APP,LOOKUP 0,LOOKUP 1,ABS 88,APP,APP,ABS 69,APP,APP,RET,LOOKUP 3,ABS 76,APP,LOOKUP 4,APP,RET,LOOKUP 3,ABS 81,APP,RET,LOOKUP 1,LOOKUP 0,LOOKUP 3,APP,APP,RET,LOOKUP 0,LOOKUP 1,APP,RET,LOOKUP 1,LOOKUP 2,ABS 118,APP,APP,LOOKUP 3,ABS 104,APP,APP,RET,LOOKUP 3,ABS 109,APP,RET,LOOKUP 2,LOOKUP 0,APP,LOOKUP 1,LOOKUP 0,APP,APP,RET,ABS 121,RET,LOOKUP 2,ABS 126,APP,RET,LOOKUP 0,LOOKUP 1,APP,LOOKUP 2,APP,RET,LOOKUP 0,LOOKUP 0,APP,RET]

-}