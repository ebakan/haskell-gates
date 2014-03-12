import Data.Bits
import Text.Printf

and2 = (&&)
and3 = (fmap.fmap) and2 and2
and4 = (fmap.fmap.fmap) and2 and3

nand2 = (fmap.fmap) not and2
nand3 = (fmap.fmap.fmap) not and3
nand4 = (fmap.fmap.fmap.fmap) not and4

or2 = (||)
or3 = (fmap.fmap) or2 or2
or4 = (fmap.fmap.fmap) or2 or3

nor2 = (fmap.fmap) not or2
nor3 = (fmap.fmap.fmap) not or3
nor4 = (fmap.fmap.fmap.fmap) not or4

xor2 True = not
xor2 False = id
xnor2 = (fmap.fmap) not xor2

numToArr len num = [testBit (num :: Int) x | x <- [0..(len-1)]]

apply2 fn [] = []
apply2 fn (x1:x2:xs) = fn x1 x2 : apply2 fn xs

apply3 fn [] = []
apply3 fn (x1:x2:x3:xs) = fn x1 x2 x3 : apply3 fn xs

apply4 fn [] = []
apply4 fn (x1:x2:x3:x4:xs) = fn x1 x2 x3 x4 : apply4 fn xs

and2' = apply2 and2
and3' = apply3 and3
and4' = apply4 and4

nand2' = apply2 nand2
nand3' = apply3 nand3
nand4' = apply4 nand4

or2' = apply2 or2
or3' = apply3 or3
or4' = apply4 or4

nor2' = apply2 nor2
nor3' = apply3 nor3
nor4' = apply4 nor4

xor2' = apply2 xor2
xnor2' = apply2 xnor2

main = do
  let nor32 = head . nor2' . or4' . or4'
  let nor32' = head . nor2' . nand4' . nor4'
  let n = 0
  let bits = 32
  let arr = numToArr bits n
  printf "Positive Logic nor32 %d = %s\n" n $ (show . nor32) arr
  printf "Negative Logic nor32 %d = %s\n" n $ (show . nor32') arr
