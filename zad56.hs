alterSeries xs = foldl (+) 0 (zipWith (*) (cycle [1,-1]) xs)
