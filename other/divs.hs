is_prime x = length [t | t<-[2..x `div` 2+1], x `mod` t == 0] == 0
primes = 2 : [x | x<-[2..],is_prime x]

plog a x | x `mod` a == 0 = 1 + plog a (x `div` a)
plog a x = 0

divisors 1 pri = []
divisors x (p:pri) | x `mod` p == 0 = c : divisors (x `div` (p^c)) pri
        where c = plog p x
divisors x (_:pri) = 0 : divisors x pri

fact n = product [1..n]
nearest n = [t | t <- [n+1..], fact n `mod` t /= 0, sum (divisors t primes) > 1] !! 0

main = do
   print $ take 20 primes
   print $ divisors 5 primes

   print $ [(k,nearest k,divisors (nearest k) primes)| k <- [1..10]]