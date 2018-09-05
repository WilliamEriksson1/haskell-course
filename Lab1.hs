import Test.QuickCheck
import Data.Foldable

{- Lab 1
  Authors:
  Lab Group: Group 18
-}

--------------------------------------
power :: Integer -> Int -> Integer
-- Second argument type was amended to type 'Int', simply because
-- the base function 'take' for lists, is restricted to the 'Int' type.
-- not our fault. we had to do this when we wanted to compare them in
-- comparepower1. don't hate us.
power n k
    | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k - 1)

-- A ---------------------------------
-- stepsPower n k gives the number of steps that
-- power n k takes to compute

stepsPower :: Integer -> Integer -> Integer
stepsPower n k = k + 1


-- B ---------------------------------
-- power1

eles :: Integer -> Int -> [Integer]
eles n k = take k (repeat n)

power1 :: Integer -> Int -> Integer
power1 n k = product (eles n k)


-- C ---------------------------------
-- power2

isEven :: Int -> Bool
isEven n | mod n 2 == 0 = True
         | mod n 2 /= 0 = False

power2 :: Integer -> Int -> Integer
power2 n k
    | k < 0 = error "power: negative argument"
power2 n 0 = 1
power2 n k
    | isEven k = power2 (n * n) (div k 2)
    | not (isEven k) = n * power2 n (k - 1)

-- D ---------------------------------
{-

<Describe your test cases here>

0^0 should always be 1 you know cool. 1
2^2 because it's very simple and very good. 4
2^64 to make sure that we are not overflowing 18446744073709551616
3^5 because fuck it, uneven number and stuff like that. 243

-}

-- comparePower1
comparePower1 :: Integer -> Int -> Bool
comparePower1 n k = if power n k == power1 n k then True else False

-- we chose to do if sats becase we learned on our own yes.

-- comparePower2
comparePower2 :: Integer -> Int -> Bool
comparePower2 n k | power n k == power2 n k = True
                  | power n k /= power2 n k = False

-- Test functions:

testPower1 :: Bool
testPower1 = (comparePower1 0 0) && (comparePower1 2 2) &&
             (comparePower1 2 64) && (comparePower1 3 5)

testPower2 :: Bool
testPower2 = (comparePower2 0 0) && (comparePower2 2 2) &&
             (comparePower2 2 64) && (comparePower2 3 5)



----------------- TABLE

spaces = 9

triplePower :: Integer -> Int -> String
triplePower n k = show [show k, show (power n k),
                   show (power1 n k), show (power2 n k)]

myList :: [Integer]
myList = [1]

genTable n k = take k [ (j) | i <- [0..],
                              let l = triplePower n i,
                              let j = l]

table n k = mapM_ putStrLn (genTable n (k + 1))
