import Data.Array
import Data.List

points = [(i,j) | i <- [1..5], j <- [1..5]]

all9s _ 0 = [[]]
all9s (l1:ls) n = [l1:x | x <- all9s ls (n-1)] ++ all9s ls n
all9s [] n = []

neighbours pts = accumArray (+) 0 ((1,1),(5,5)) nb where
    raw_nb = concat [[(a+1,b),(a-1,b),(a,b+1),(a,b-1)] | (a,b) <- pts]
    nb = [((r,s),1) | (r,s) <- raw_nb, r > 0, r <= 5, s > 0, s <= 5, not (elem (r,s) pts)] ++
         [((r,s),9) | (r,s) <- pts]

check pa = and [pa ! (i,j) == 1 || pa ! (i,j) == 9 | (i,j) <- points]

visualize pa = intercalate "\n" [concat [if pa ! (i,j) < 9 then show $ pa ! (i,j) else "*" | j <- [1..5]] | i <- [1..5]]

main = do
    putStrLn $ concat [visualize pa ++ "\n\n" | pa <- map neighbours (all9s points 9), check pa]