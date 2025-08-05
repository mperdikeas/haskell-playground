main :: IO ()

absol :: Int -> Int

absol n | n >= 0 = n
      | otherwise = -n


rev :: [a] -> [a]
rev [] = []
rev (x:xs) = rev xs ++ [x]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n<0 = []
           | n==0 = []
           | otherwise = (n `mod` 10) : toDigitsRev(n `div` 10)


toDigits :: Integer -> [Integer]
toDigits n = rev (toDigitsRev n)

doubleEveryOther :: [Integer] -> [Integer]

doubleEveryOther ([a]) = [a]
doubleEveryOther ([a, b]) = [2*a, b]
doubleEveryOther (a:b:xs) = 2*a : b: doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs)
  | x<10 = x+sumDigits xs
  | otherwise = (sumDigits (toDigits x)) + sumDigits (xs)

isValid :: Integer -> Bool
isValid n = (sumDigits (doubleEveryOther (toDigits n)) `mod` 10)==0

           


main =
  do
--    print $ toDigits 100 
--    print $ toDigits 1234
--    print $ doubleEveryOther (toDigits 1234)
--    print $ sumDigits (doubleEveryOther (toDigits 1234))
--    print $ 4012888888881881
--    print $ toDigits 4012888888881881
--    print $ doubleEveryOther( toDigits 4012888888881881)
--    print $ sumDigits (doubleEveryOther (toDigits 4012888888881881))
--    print $ sumDigits (doubleEveryOther (toDigits 4012888888881882))
    print $ isValid 4012888888881881
    print $ isValid 4012888888881882
