main :: IO ()

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, c)] ++ (hanoi (n-1) b a c)

naiveSplit :: Integer -> (Integer, Integer)
naiveSplit n =
  do
    let a = n `div` 2
    (a, n -a)

-- cf.: sigcse-vol23-no-3-Sept-1991.pdf
cleverSplit :: Integer -> (Integer, Integer)
cleverSplit n =
  do
    let b = floor ( (sqrt (fromIntegral(1 + 8*n)) -1) / 2 )
    (n-b, b)

superHanoi :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
superHanoi 0 _ _ _ _ = []
superHanoi n a b c d =
  do
    let split = cleverSplit
    let (n1,n2) = split n
    (superHanoi n1 a b d c) ++ (hanoi n2 a b d) ++ (superHanoi n1 c a b d)

           


main =
  do
    let n = 16
    print $ length (hanoi n "a" "b" "c")
    print $ length (superHanoi n "a" "b" "c" "d")
    

