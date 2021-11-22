{-|
Module      : 1JC3-Assign1.Assign_1.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 1 - McMaster CS 1JC3 2021
-}
module Assign_1 where

import Prelude hiding (sin,cos,tan)

-----------------------------------------------------------------------------------------------------------
-- INSTRUCTIONS              README!!!
-----------------------------------------------------------------------------------------------------------
-- 1) DO NOT DELETE/ALTER ANY CODE ABOVE THESE INSTRUCTIONS
-- 2) DO NOT REMOVE / ALTER TYPE DECLERATIONS (I.E THE LINE WITH THE :: ABOUT THE FUNCTION DECLERATION)
--    IF YOU ARE UNABLE TO COMPLETE A FUNCTION, LEAVE IT'S ORIGINAL IMPLEMENTATION (I.E. THROW AN ERROR)
-- 3) MAKE SURE THE PROJECT COMPILES (I.E. RUN STACK BUILD AND MAKE SURE THERE ARE NO ERRORS) BEFORE
--    SUBMITTING, FAILURE TO DO SO WILL RESULT IN A MARK OF 0
-- 4) REPLACE macid = "TODO" WITH YOUR ACTUAL MACID (EX. IF YOUR MACID IS jim THEN macid = "jim")
-----------------------------------------------------------------------------------------------------------
macid :: String
macid = "singhn77"



{- -----------------------------------------------------------------
 - factorial
 - -----------------------------------------------------------------
 - Description:
 -    Computes the factorial of any Integer n
 - -----------------------------------------------------------------
 - |   Input     |                                                 |
 - |      n      | Integer input                                   |
 - -----------------------------------------------------------------
 - |   Output    |                                                 |
 - |      n <= 1 | 1                                               |
 - |      n >  1 | n * (n-1) ...  * 1   while (n-k) > 0            |
 - -----------------------------------------------------------------
 -}
factorial :: Integer -> Integer
factorial n = if n > 0
              then n * factorial (n-1)
              else 1

{- ---------------------------------------------------------------------
 - sinTaylor
 - ---------------------------------------------------------------------
 - Description:
 -   Computes the 4thTaylor polynomial approximation of sin(x) at a
 - ---------------------------------------------------------------------
 - |    Input    | 
 - |      a      | Double input 
 - |      cos_a  | Double input 
 - |      sin_a  | Double input 
 - |      x      | Double input 
 - ---------------------------------------------------------------------
 - |    Output   |
 -
 -    (x-a)^n / ( factorial n ) , where n = 0 , 1 , 2 , 3 , 4
 -
 - ---------------------------------------------------------------------
 -}
sinTaylor :: Double -> Double -> Double -> Double -> Double
sinTaylor a cos_a sin_a x = 
  sin_a*fn 0 + cos_a*fn 1 - sin_a*fn 2 - cos_a*fn 3 + sin_a*fn 4
  where fn n = (x-a)^n / fromIntegral ( factorial n )

{- -----------------------------------------------------------------
 - fmod
 - -----------------------------------------------------------------
 - Description:
 -   Computes the remainder of a division
 - -----------------------------------------------------------------
 - |    Input    |                                                 |
 - |      x      | Double input                                    |
 - |      y      | Double input                                    |
 - -----------------------------------------------------------------
 - |    Output   |                         
 - |
 - |    z = floor ( x / y )  , where z = remainder of (x/y)
 - |
 - -----------------------------------------------------------------
 -}

fmod :: Double -> Double -> Double
fmod x y =
  let
    -- z is the largest integer s.t. z*y <= x
    -- HINT use floating point division, then round down
    z = fromIntegral(floor(x/y))
  in x - z*y

{- -----------------------------------------------------------------------------------------------
 - sinApprox
 - -----------------------------------------------------------------------------------------------
 - Description:
 -   Computes an approximation of sin(x) using sinTaylor
 - -----------------------------------------------------------------------------------------------
 - |    Input    | 
 - |      x      | Double input 
 - -----------------------------------------------------------------------------------------------
 - |       Output             |
 - |  sinApprox 0 1 0 x       | x --> fmod x (2*pi) , where mod (x,2*pi) between [0,pi/4)
 - |  sinApprox pi/2 0 1 x    | x --> fmod x (2*pi) , where mod (x,2*pi) between [pi/4,3*pi/4)
 - |  sinApprox pi -1 0 x     | x --> fmod x (2*pi) , where mod (x,2*pi) between [3*pi/4,5*pi/4)
 - |  sinApprox 3*pi/2 0 -1 x | x --> fmod x (2*pi) , where mod (x,2*pi) between [5*pi/4,7*pi/4)
 - |  sinApprox 2*pi 1 0 x    | x --> fmod x (2*pi) , where mod (x,2*pi) between [7*pi/4 , 2*pi)
 - |  otherwise sinx=0        
 - ------------------------------------------------------------------------------------------------
 -}

sinApprox :: Double -> Double
sinApprox x 

  | f >=0 && f < pi/4  = sinTaylor 0 1 0 (f)
  | f>= pi/4 && f < 3*pi/4   = sinTaylor (pi/2) 0 1 (f)
  | f>=3*pi/4 && f < 5*pi/4   = sinTaylor (pi) (-1) 0 (f)
  | f>=5*pi/4 && f < 7*pi/4   = sinTaylor (3*pi/2) 0 (-1) (f)
  | f>=7*pi/4 && f < 2*pi  = sinTaylor (2*pi) 1 0 (f)
  | otherwise = 0
  
  where f= fmod x (2*pi)
  

{- ---------------------------------------------------------------------------------
 - cosApprox
 - ---------------------------------------------------------------------------------
 - Description:
 -   Uses sinApprox to compute the approximate value of cos(x)
 - ---------------------------------------------------------------------------------
 - |    Input    | 
 - |      x      | Double input 
 - ---------------------------------------------------------------------------------
 - |    Output   |
 - |    cos(x) = -sinApprox ( x-pi/2 ) 
 - ---------------------------------------------------------------------------------
 -}
cosApprox :: Double -> Double
cosApprox x = - sinApprox ( x - pi/2 )

{- ---------------------------------------------------------------------------------
 - tanApprox
 - ---------------------------------------------------------------------------------
 - Description:
 -   Uses sinApprox and cosApprox to compute the approximate value of tan(x)
 - ---------------------------------------------------------------------------------
 - |    Input    | 
 - |      x      | Double input 
 - ---------------------------------------------------------------------------------
 - |    Output   |
 - |    tan(x) = sinApprox (x) / cosApprox(x)
 - ---------------------------------------------------------------------------------
 -}
tanApprox :: Double -> Double
tanApprox x = sinApprox x / cosApprox x
