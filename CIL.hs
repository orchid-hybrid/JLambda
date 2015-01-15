module CIL (
 ) where

import Data.Int

import qualified HuttonsRazor as H

data CIL
 = LDC_I4 Int32 -- push number onto the stack
 | ADD          -- add the top two numbers ( x y -- x+y )
 | RET          -- return the top of the stack

instance Show CIL where
 show (LDC_I4 i) = unwords ["ldc.i4", show i]
 show ADD        = unwords ["add"]
 show RET        = unwords ["ret"]

--

toCIL H.HALT = [RET]
toCIL (H.ADD s) = ADD : toCIL s
toCIL (H.PUSH i s) = LDC_I4 (fromIntegral i) : toCIL s

compile = unlines . map show . toCIL
