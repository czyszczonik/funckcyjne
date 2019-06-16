nub :: (Eq a)=>[a]->[a]

nub [] = []
nub [x] = [x]
nub (x:xs) = x:nub[a | a<-xs, a/=x]
