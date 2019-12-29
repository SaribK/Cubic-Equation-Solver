{- Assignment 1
 - Name: Sarib Kashif
 - Date: 2019 September 28
 -}
module Assign_1 where

macid :: String
macid = "Kashis2"

{- -----------------------------------------------------------------
 - cubicQ
 - -----------------------------------------------------------------
 - Description: 
 - takes in 3 inputs and then utilizes the inputs to calculate Q
 -}
cubicQ :: Double -> Double -> Double -> Double
cubicQ a b c = (3 * a * c - b^2) / (9 * a^2) 

{- -----------------------------------------------------------------
 - cubicR
 - -----------------------------------------------------------------
 - Description: 
 - takes 4 inputs and uses a formula to find R
 -}
cubicR :: Double -> Double -> Double -> Double -> Double
cubicR a b c d = (9 * a * b * c - 27 * (a^2) * d - 2 * b^3) / (54 * a^3)

{- -----------------------------------------------------------------
 - cubicDisc
 - -----------------------------------------------------------------
 - Description: 
 - finds the discriminant by taking two inputs, q and r
 -}
cubicDisc :: Double -> Double -> Double
cubicDisc q r = q^3 + r^2

{- -----------------------------------------------------------------
 - cubicS
 - -----------------------------------------------------------------
 - Description: 
 - takes two inputs, q and r, and finds S
 - two if statements because before the cube root, if the value is negative, first the number has to be turned into positive, then cube rooted
 - so the if statement just checks if the number is negative before being cube rooted
 - cube root is done using the ** operator
 - if statements are done using guarded expressions
 -}
cubicS :: Double -> Double -> Double
cubicS q r 
    | ((r + ((q^3 + r^2)**(1 / 2))) < 0) = -(-(r + ((q^3 + r^2)**(1 / 2))))**(1 / 3)
    | otherwise = (r + ((q^3 + r^2)**(1 / 2)))**(1 / 3)

{- -----------------------------------------------------------------
 - cubicT
 - -----------------------------------------------------------------
 - Description: 
 - very similar function to cubicS, check comments for that
 -}
cubicT :: Double -> Double -> Double
cubicT q r 
    | ((r - ((q^3 + r^2)**(1 / 2))) < 0) = -(-(r - ((q^3 + r^2)**(1 / 2))))**(1 / 3)
    | otherwise = (r - ((q^3 + r^2)**(1 / 2)))**(1 / 3)
{- -----------------------------------------------------------------
 - cubicRealSolutions
 - -----------------------------------------------------------------
 - Description: 
 - use if statements, |, to determine different outputs depending on the discriminant
 - determine all values needed to calculate x1, x2, x3
 - "where" is used to define all the values using the functions created earlier
 -}
cubicRealSolutions :: Double -> Double -> Double -> Double -> [Double]
cubicRealSolutions a b c d
    | (disc < 0) = []
    | (disc == 0) = [x1, x2, x3]
    | (disc > 0) = [x1]
    where 
        q = cubicQ a b c
        r = cubicR a b c d
        disc = cubicDisc q r
        s = cubicS q r
        t = cubicT q r
        x1 = s + t - (b / (3 * a))
        x2 = (-(s + t) / 2) - (b / (3 * a))
        x3 = (-(s + t) / 2) - (b / (3 * a))
{- -----------------------------------------------------------------
 - Test Cases 
 - cubicRealSolutions 2 1 3 6 = [-1.2404464287820862]
 - cubicRealSolutions 1 3 2 (-10) = [1.308907319765098]
 - cubicRealSolutions 1 1 (-1) (-1) = [1.0,-1.0,-1.0]
 - cubicRealSolutions 2 (-3) (-17) 30 = [] 
 -}