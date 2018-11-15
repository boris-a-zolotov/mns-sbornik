alln 0 = [[]]
alln n = [1 : u | u <- a] ++ [2: u | u <- a] ++ [3: u | u <- a]
  where a = alln (n-1)

shift n 0 = n
shift (n1:ns) k = shift (ns++[n1]) (k-1)

iselder n = and [shift n k < n | k <- [1..length(n)-1]]

main = do
  print $ length [u | u<-alln 5, iselder u]