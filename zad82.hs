import Data.Monoid
import Control.Monad.Writer

f :: Integral a => a -> a
f a = if a `mod` 2 == 0 then a `div` 2 else 3 * a + 1

h :: (Show a, Integral a, Integral b) => a -> Writer (Sum b, String) b
h a = if a == 1 then return 1 else do tell (Sum 1, show a ++ ", ")
                                      h (f a)
