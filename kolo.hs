import Data.Monoid
import Control.Monad.Writer
import Data.List

-- -- zad1
pack :: Eq a => [a] -> [[a]]
pack[] = []
pack(x:xs) = (x : takeWhile(== x) xs) : (pack(dropWhile(== x) xs))

remover list = concat (head pack list : map tail(tail pack list))

zad2

avg l = sum l / genericLength l
msplit myList k = splitAt (((length myList) + 1) `div` k) myList
meansAndVars list k =  map.map avg msplit list k

-- Zad 3


ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))


h :: (Show a, Integral a, Integral b) => a -> Writer (Sum b, String) b
h m n = if m == 0 then do tell (Sum 1, show "1") n + 1
                  else if n == 0 then do tell (Sum 1, show "2") ack(m n)
                  else do tell (Sum 1, show "3") ack(m n)
