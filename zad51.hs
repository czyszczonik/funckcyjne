rleEncode [] = []
rleEncode (x:xs) = (length (x : takeWhile (== x) xs),x) : rleEncode(dropWhile(== x) xs)

rleDec [] = []
rleDec (x:xs) = replicate (fst x) (snd x) ++ rleDec xs
