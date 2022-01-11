module Main where

import Data.Char
import HaskellSay (haskellSay)

add :: Integer -> Integer -> Integer   --function declaration 
add x y =  x + y                       --function definition 

fact :: Int -> Int 
fact 0 = 1 
fact n = n * fact ( n - 1 ) 

fact2 :: Integer -> Integer 
fact2 n | n == 0 = 1 
        | n /= 0 = n * fact2 (n-1) 

roots :: (Float, Float, Float) -> (Float, Float)  
roots (a,b,c) = (x1, x2) where 
   x1 = e + sqrt d / (2 * a) 
   x2 = e - sqrt d / (2 * a) 
   d = b * b - 4 * a * c  
   e = - b / (2 * a)  

fType :: Double -> Double -> Double
fType x y = x*x + y*y

myA :: Char -> Char
myA x = 'A'

data Area = Circle Float Float Float
surface :: Area -> Float
surface (Circle _ _ r) = pi * r ^ 2

eveno :: Int -> Bool 
noto  :: Bool -> String 

eveno x = if x `rem` 2 == 0 
   then True 
else False 
noto x = if x == True 
   then "This is an even Number" 
else "This is an ODD number" 

addBrackets :: String -> String 
addBrackets s = "[" ++ s ++ "]"
result = map addBrackets ["One", "Two", "three"]

factorial :: Int -> Int
factorial n = if n < 2 then 1 else n * factorial (n-1)

convertUpperCase :: String -> String
convertUpperCase = map toUpper

readInts :: String -> [Int]
readInts s = let ws = words s in map read ws


main :: IO ()
main = do
    -- haskellSay "Hello, Haskell! You're using a function from another package!"
    putStrLn "The addition of the two numbers is:"  
    print (add 2 5)    --calling a function 
    putStrLn "The factorial of 5 is:" 
    print (fact 5)
    print (fact2 6)
    putStrLn "The roots of our Polynomial equation are:" 
    print (roots(1,-8,6))
    let var1 = 2
    let var2 = 3
    putStrLn "var1 plus var2 is:"
    print(var1 + var2)
    print([1..10])
    putStrLn "fType function:"
    print(fType 5.52 2.45)
    let x = False
    if x == True
        then putStrLn "X is True"
    else putStrLn "X is False"
    print(myA 'b')
    print([1..10])
    print(show [1..10])
    print(2 :: Int)
    print(2 :: Double)
    print(surface $ Circle 10 20 14)
    let a = [1..10]
    print(head a)
    print(tail a)
    print(last a)
    print(init a)
    print(null a)
    print(reverse a)
    print(length a)
    print(take 5 a)
    print(drop 5 a)
    print(maximum a)
    print(minimum a)
    print(sum a)
    print(product a)
    putStrLn "Check in list .. Is 99 is in list?"
    print(elem 99 a)
    print ((noto.eveno)(16))
    print result
    print $ factorial 5
    let
        x = 5
        y = 6
    print (x + y)
    print (words "aa bb cc")
    print (convertUpperCase  "aavvcc")
    print (readInts "11 22 33")