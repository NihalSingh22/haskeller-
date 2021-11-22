{-|
Module      : 1JC3-Assign2.Assign_2.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 2 - McMaster CS 1JC3 2021
-}
module Assign_2 where

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

-- Name: Nihal Singh
-- Date: 25/10/2021

macid :: String
macid = "singhn77"

type Vector3D = (Double,Double,Double)

{- ------------------------------------------------------------------------
 - getX
 - ------------------------------------------------------------------------
 - Description:
 -   Computes the function and returns the x - coordinate.
 - ------------------------------------------------------------------------
 - |   Input  -> A vector set ( x , y , z )
 - ------------------------------------------------------------------------
 - |   Output -> Returns the x - coordinate
 - ------------------------------------------------------------------------
 -}


getX :: Vector3D -> Double
getX (x,y,z) = x


{- ------------------------------------------------------------------------
 - getY
 - ------------------------------------------------------------------------
 - Description:
 -   Computes the function and returns the y - coordinate.
 - ------------------------------------------------------------------------
 - |   Input  -> A vector set ( x , y , z )
 - ------------------------------------------------------------------------
 - |   Output -> Returns the y - coordinate
 - ------------------------------------------------------------------------
 -}


getY :: Vector3D -> Double
getY ( x , y , z ) = y


{- ------------------------------------------------------------------------
 - getZ
 - ------------------------------------------------------------------------
 - Description:
 -   Computes the function and returns the z - coordinate.
 - ------------------------------------------------------------------------
 - |   Input  -> A vector set ( x , y , z )
 - ------------------------------------------------------------------------
 - |   Output -> Returns the z - coordinate
 - ------------------------------------------------------------------------
 -}


getZ :: Vector3D -> Double
getZ ( x , y , z ) = z


{- ------------------------------------------------------------------------
 - scalarMult 
 - ------------------------------------------------------------------------
 - Description:
 -   Computes the scalar multiplication of the inputs.
 - ------------------------------------------------------------------------
 - |   Input  -> A double type value 's' and vector ( x , y , z )
 - ------------------------------------------------------------------------
 - |   Output -> vector of the product of s with the each of 
 - |             x , y and z coordinate          
 - ------------------------------------------------------------------------
 -}


scalarMult :: Double -> Vector3D -> Vector3D
scalarMult s (x,y,z) = ( s*x , s*y , s*z )


{- ------------------------------------------------------------------------
 - add
 - ------------------------------------------------------------------------
 - Description:
 -   Computes the 3D vector addition of the coordinates.
 - ------------------------------------------------------------------------
 - |   Input  -> 2 set of vectors (x1,y1,z1) and (x2,y2,z3)
 - ------------------------------------------------------------------------
 - |   Output -> A vector with the sum of each of the corresponding  
 - |             x , y and z vector coordinates
 - ------------------------------------------------------------------------
 -}


add :: Vector3D -> Vector3D -> Vector3D
add ( x1 , y1 , z1 ) ( x2 , y2 , z2 )  =  ( x1+x2 , y2+y1 , z2+z1 )


{- ------------------------------------------------------------------------
 - innerProduct
 - ------------------------------------------------------------------------
 - Description:
 -   Computes the inner product operation for a 3D vector space.
 - ------------------------------------------------------------------------
 - |   Input  -> 2 set of vectors (x1,y1,z1) and (x2,y2,z3)
 - ------------------------------------------------------------------------
 - |   Output -> A precise floating point sum of the product between 
 - |             each of the corresponding vector coordinates 
 - ------------------------------------------------------------------------
 -}


innerProduct :: Vector3D -> Vector3D -> Double
innerProduct ( x1 , y1 , z1 ) ( x2 , y2 , z2 ) = x2*x1 + y2*y1 + z2*z1


{- ------------------------------------------------------------------------
 - distance
 - ------------------------------------------------------------------------
 - Description:
 -   Computes the Vector distance between two elements i.e d(p,q).
 - ------------------------------------------------------------------------
 - |   Input  -> 2 set of vectors (x1,y1,z1) and (x2,y2,z3)
 - ------------------------------------------------------------------------
 - |   Output -> A precise floating point distance between the two elements 
 - ------------------------------------------------------------------------
 -}


distance :: Vector3D -> Vector3D -> Double
distance ( x1 , y1 , z1 ) ( x2 , y2 , z2 ) =  sqrt ( (x2-x1) **2 + (y2-y1) **2 + (z2-z1) **2 )


{- ------------------------------------------------------------------------
 - maxDistance
 - ------------------------------------------------------------------------
 - Description:
 -   Computes a list of elements with the largest distance from (0,0,0)
     Returns (0,0,0) if an empty list is given. 
 - ------------------------------------------------------------------------
 - |   Input  -> A list of 3D Vectors (Vector 3D)
 - ------------------------------------------------------------------------
 - |   Output -> A vector with the largest distance calculated through the 
 - |             distance function
 - ------------------------------------------------------------------------
 -}

maxDistance :: [Vector3D] -> Vector3D
maxDistance [] = (0,0,0)
maxDistance (v:vs) 
  | distance (0,0,0) v > distance (0,0,0) (maxDistance vs) = v
  | otherwise = maxDistance vs




{-------------              TEST CASES              -------------}




{- 

- --------------------------------------------------------
|                    Function: getX                      |
- --------------------------------------------------------
Test Case Number: 1                                      |
Input: (4,7,9)                                           |
Expected Output: 4.0                                     |
Actual Output: 4.0                                       |
- --------------------------------------------------------
Test Case Number: 2                                      |
Input: (4.4,6.1,9.6)                                     |
Expected Output: 4.4                                     | 
Actual Output: 4.4                                       |
- --------------------------------------------------------
Test Case Number: 3                                      |
Input: (18.943,32.321,8.555)                             |
Expected Output: 18.943                                  | 
Actual Output: 18.943                                    |
- --------------------------------------------------------

-}




{- 

- --------------------------------------------------------
|                    Function: getY                      |
- --------------------------------------------------------
Test Case Number: 1                                      |
Input: (7,3,1)                                           |
Expected Output: 3.0                                     |
Actual Output: 3.0                                       |
- --------------------------------------------------------
Test Case Number: 2                                      |
Input: (6.4,9.9,2.1)                                     |
Expected Output: 9.9                                     | 
Actual Output: 9.9                                       |
- --------------------------------------------------------
Test Case Number: 3                                      |
Input: (5.67 , 8.21 , 4.01)                              |
Expected Output: 8.21                                    | 
Actual Output: 8.21                                      |
- --------------------------------------------------------

-}




{- 

- --------------------------------------------------------
|                     Function: getZ                     |
- --------------------------------------------------------
Test Case Number: 1                                      |
Input: (0,5,2)                                           |
Expected Output: 2.0                                     |
Actual Output: 2.0                                       |
- --------------------------------------------------------
Test Case Number: 2                                      |
Input: (0.5,9.1,0.9)                                     |
Expected Output: 0.9                                     | 
Actual Output: 0.9                                       |
- --------------------------------------------------------
Test Case Number: 3                                      |
Input: (3.111,4.099,0.456)                               |
Expected Output: 0.456                                   | 
Actual Output: 0.456                                     |
- --------------------------------------------------------

-}




{- 

- --------------------------------------------------------
|                 Function: scalarMult                   |
- --------------------------------------------------------
Test Case Number: 1                                      |
Input: 3 (5,9,11)                                        |
Expected Output: (15.0,27.0,33.0)                        |
Actual Output: (15.0,27.0,33.0)                          |
- --------------------------------------------------------
Test Case Number: 2                                      |
Input: 9 (4.1 , 6.8 , 3.3)                               |
Expected Output: (36.9,61.199999999999996,29.7)          | 
Actual Output: 0.9                                       |
- --------------------------------------------------------
Test Case Number: 3                                      |
Input: 5 (6.66 , 9.12 , 4.09)                            |
Expected Output: (33.3,45.599999999999994,20.45)         | 
Actual Output: (33.3,45.599999999999994,20.45)           |
- --------------------------------------------------------

-}




{- 

- --------------------------------------------------------
|                    Function: add                       |
- --------------------------------------------------------
Test Case Number: 1                                      |
Input: (4,1,9) (11,19,21)                                |
Expected Output: (15.0,20.0,30.0)                        |
Actual Output: (15.0,20.0,30.0)                          |
- --------------------------------------------------------
Test Case Number: 2                                      |
Input: (21.91231,31.12313,0.92984) (1.1 , 6.2 , 9.5)     |
Expected Output: (23.012310000000003,37.32313,10.42984)  | 
Actual Output: (23.012310000000003,37.32313,10.42984)    |
- --------------------------------------------------------
Test Case Number: 3                                      |
Input: (4.4 , 6.11 , 9) (61.921 , 4 , 12.888)            |
Expected Output: (66.321,10.11,21.887999999999998)       | 
Actual Output: (66.321,10.11,21.887999999999998)         |
- --------------------------------------------------------

-}




{- 

- --------------------------------------------------------
|                  Function: innerProduct                |
- --------------------------------------------------------
Test Case Number: 1                                      |
Input: (6.1,4.9,3.3) (8.5,2.7,4.6)                       |
Expected Output: 80.25999999999999                       |
Actual Output: 80.25999999999999                         |
- --------------------------------------------------------
Test Case Number: 2                                      |
Input: (9.342,0.99,7.188) (4.44,1.34,8.61)               |
Expected Output: 104.69376                               | 
Actual Output: 104.69376                                 |
- --------------------------------------------------------
Test Case Number: 3                                      |
Input: (2.11,8.1,0.321) (9.8213,11.485,41.2)             |
Expected Output: 126.976643                              | 
Actual Output: 126.976643                                |
- --------------------------------------------------------

-}




{- 

- --------------------------------------------------------
|                  Function: distance                    |
- --------------------------------------------------------
Test Case Number: 1                                      |
Input: (1.1,5.9,4) (3.1,2.9,9)                           |
Expected Output: 6.164414002968976                       |
Actual Output: 6.164414002968976                         |
- --------------------------------------------------------
Test Case Number: 2                                      |
Input: (8.921,3.92,2.23)(1.3,4.12,4.666)                 |
Expected Output: 8.003357857799438                       | 
Actual Output: 8.003357857799438                         |
- --------------------------------------------------------
Test Case Number: 3                                      |
Input: (4.431,9,2.41) (21,5.33,6.92)                     |
Expected Output: 17.55963442102369                       | 
Actual Output: 17.55963442102369                         |
- --------------------------------------------------------

-}




{- 

- ---------------------------------------------------------------------------------------------
|                  Function: maxDistance                                                      |
- ---------------------------------------------------------------------------------------------
Test Case Number: 1                                                                           |
Input: [(4.234,6.12,91) , (12.3,76.543,8.02) , (4.321,5.432,9.876)]                           |
Expected Output: (4.234,6.12,91.0)                                                            |
Actual Output: (4.234,6.12,91.0)                                                              |
- ---------------------------------------------------------------------------------------------
Test Case Number: 2                                                                           |
Input: [(9.9,0.37472,1.99) , (0.11,4.912,3.077)]                                              |
Expected Output: (9.9,0.37472,1.99)                                                           | 
Actual Output: (9.9,0.37472,1.99)                                                             |
- ---------------------------------------------------------------------------------------------
Test Case Number: 3                                                                           | 
Input: [(0,0,0)]                                                                              |
Expected Output: (0.0,0.0,0.0)                                                                | 
Actual Output: (0.0,0.0,0.0)                                                                  |
- ---------------------------------------------------------------------------------------------

-}