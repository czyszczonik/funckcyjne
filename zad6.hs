symNew n k = if n == k || k == 0
    then 1
    else symNew (n-1) (k-1) + symNew (n-1) k
