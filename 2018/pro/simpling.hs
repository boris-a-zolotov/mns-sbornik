isPrime x = ([t | t <- [2..x-1], x `mod` t == 0] == [])

isSimpling x = ([t | t <- [2..x], gcd x t == 1, not (isPrime t)] == [])

main = do
  sequence $ map print [x | x <- [2..2000], isSimpling x]