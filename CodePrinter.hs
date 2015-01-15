import HuttonsRazor

import Data.List

data Javable = JInt Int | JNew String [Javable]

java (JInt i) = show i
java (JNew cls vals) = "new " ++ cls ++ "(" ++ intercalate "," (map java vals) ++ ")"

toJava HALT = JNew "HaltNode" []
toJava (PUSH i c) = JNew "PushNode" [JInt i, toJava c]
toJava (ADD c) = JNew "AddNode" [toJava c]

cccJava HALT = JNew "HaltNode" []
cccJava (PUSH i c) = JNew "PushNode" [JInt i, cccJava c]
cccJava (ADD c) = JNew "AddNode" [cccJava c]
cccJava (LOOKUP i c) = JNew "LookupNode" [JInt i, cccJava c]
cccJava (ABS c c') = JNew "AbsNode" [cccJava c, cccJava c']
cccJava RET = JNew "RetNode" []
cccJava (APP c) = JNew "AppNode" [cccJava c]

{-

*Main Data.List> putStrLn . java . toJava . comp $ t1
new PushNode(17,new HaltNode())
*Main Data.List> putStrLn . java . toJava . comp $ t2
new PushNode(1,new PushNode(1,new AddNode(new HaltNode())))
*Main Data.List> putStrLn . java . toJava . comp $ t3
new PushNode(1,new PushNode(2,new AddNode(new PushNode(3,new AddNode(new PushNode(4,new AddNode(new PushNode(5,new AddNode(new HaltNode())))))))))
*Main Data.List> putStrLn . java . toJava . comp $ t4
new PushNode(1,new PushNode(2,new PushNode(3,new PushNode(4,new PushNode(5,new AddNode(new AddNode(new AddNode(new AddNode(new HaltNode())))))))))
*Main Data.List> putStrLn . java . toJava . comp $ t5
new PushNode(5,new PushNode(-7,new AddNode(new PushNode(-5,new PushNode(7,new AddNode(new AddNode(new HaltNode())))))))

-}
