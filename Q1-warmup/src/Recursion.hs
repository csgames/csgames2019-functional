module Recursion where

recFibonacci :: Int -> Int
recFibonacci 0 = 0
recFibonacci 1 = 1
recFibonacci n = recFibonacci (n-1) + recFibonacci (n-2)

tailRecFibonacci :: Int -> Int
tailRecFibonacci n = -- 0 -- TODO Write this function
  recur n 0 1
  where
    recur 0 a _ = a
    recur n a b = recur (n - 1) b (a + b)

recFizzBuzz :: Int -> [String]
recFizzBuzz 0 = []
recFizzBuzz n =
  recFizzBuzz (n - 1)
  ++
  [ case n of it | (3*5) `divides` it -> "FizzBuzz"
                 | 3 `divides` it -> "Fizz"
                 | 5 `divides` it -> "Buzz"
                 | otherwise -> show it
  ] where a `divides` b = b `mod` a == 0

tailRecFizzBuzz :: Int -> [String]
tailRecFizzBuzz n = -- [] -- TODO Write this function
  fizzBuzz n []
  where
    fizzBuzz 0 acc = acc
    fizzBuzz i acc =
      fizzBuzz (i-1) (line i:acc)
    line i = case i of it | (3*5) `divides` it -> "FizzBuzz"
                          | 3 `divides` it -> "Fizz"
                          | 5 `divides` it -> "Buzz"
                          | otherwise -> show it
    a `divides` b = b `mod` a == 0

-- O(n) exponentiation helper for small exponents
simpleExp :: Integer -> Integer -> Integer
simpleExp base expon = product $ replicate (fromIntegral expon) base

recExp :: Integer -> Integer -> Integer -> Integer
recExp _ 0 _ = 1
recExp base 1 _ = base
recExp base expon 1 = base `simpleExp` expon
recExp base expon step =
  let
    (d, m) = expon `divMod` step
    recursivePart = recExp (base `simpleExp` step) d step
    remainderPart = base `simpleExp` m
  in
     recursivePart * remainderPart

tailRecExp :: Integer -> Integer -> Integer -> Integer
tailRecExp base expon step = -- 0 -- TODO write this function
  tailRecExp' base expon step 1
  where
    tailRecExp' _ 0 _ acc = acc
    tailRecExp' base expon 1 acc = base `simpleExp` expon * acc
    tailRecExp' base expon step acc =
      let
        (d, m) = expon `divMod` step
        remainderPart = base `simpleExp` m
      in
        tailRecExp' (base `simpleExp` step) d step (acc * remainderPart)
