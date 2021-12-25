{-|
Module      : 1JC3-Assign4.Assign_4.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 4 - McMaster CS 1JC3 2021
-}
module Assign_4 where

import Test.QuickCheck

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
-- Date: 6-12-2021 (d-m-y)
-- Assignment 4

macid :: String
macid = "singhn77"

{- --------------------------------------------------------------------
 - Datatype: MathExpr
 - --------------------------------------------------------------------
 - Description: An Abstract Syntax Tree (AST) for encoding mathematical
 -              expressions
 - Example: The expression
 -                (abs (2*X + 1)) ^ 3
 -          can be encoded as
 -                Power 3 (Func1 Abs
 -                              (Func2 Add (Func2 Mult (Coef 2) X)
 -                                         (Coef 1)))
 - --------------------------------------------------------------------
 -}
data MathExpr a =
    X
  | Coef a
  | Func1 UnaryOp (MathExpr a)
  | Func2 BinOp (MathExpr a) (MathExpr a)
  deriving (Eq,Show,Read)

data BinOp = Add | Mult
  deriving (Show,Eq,Read)

data UnaryOp = Cos | Sin | Abs | Power Int
  deriving (Show,Eq,Read)

{- -----------------------------------------------------------------
 - eval
 - -----------------------------------------------------------------
 - Description:
 -    eval function takes e as an expression of type (MathExpr a) 
      and v as a floating point number, the function return the 
      evaluation of the value e at v

 -}

eval :: (Floating a, Eq a) => MathExpr a -> a -> a
eval X v = v 
eval (Coef a) v = a
eval (Func1 Cos x) v = cos (eval x v)
eval (Func1 Sin x) v = sin (eval x v)
eval (Func1 Abs x) v = abs (eval x v)
eval (Func1 (Power x) n ) v = (eval n v) ^^ x
eval (Func2 Add x y) v = (eval x v) + (eval y v)
eval (Func2 Mult x y) v = (eval x v) * (eval y v)

{- -----------------------------------------------------------------
 - instance Num a => Num (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    The function defines methods in terms of MathExpr , (+) , (*),
      negate , abs and fromInteger are turned into instaces of 
      Num to MathExpr data type.

 -}

instance Num a => Num (MathExpr a) where
  x + y         = Func2 Add x y
  x * y         = Func2 Mult x y
  negate x      = Func2 Mult (-1) x
  abs x         = Func1 Abs x
  fromInteger i = Coef (fromInteger i)
  signum _      = error "signum is left un-implemented"

{- -----------------------------------------------------------------
 - instance Fractional a => Fractional (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    The function includes implementations of the following methods
      recip and fromRational. Turns instaces of Fractional to 
      MathExpr data type.

 -}

instance Fractional a => Fractional (MathExpr a) where
  recip e        = Func1 (Power (-1)) e
  fromRational e = Coef (fromRational e)

{- -----------------------------------------------------------------
 - instance Floating a => Floating (MathExpr a)
 - -----------------------------------------------------------------
 - Description:
 -    The function includes implementations of the following methods
      pi , sin and cos. Turns instaces of Floating to MathExpr data 
      type.

 -}
instance Floating a => Floating (MathExpr a) where
  pi       = Coef (pi)
  sin x    = Func1 Sin x
  cos x    = Func1 Cos x
  log     = error "log is left un-implemented"
  asin _  = error "asin is left un-implemented"
  acos _  = error "acos is left un-implemented"
  atan _  = error "atan is left un-implemented"
  sinh _  = error "sinh is left un-implemented"
  cosh _  = error "cosh is left un-implemented"
  tanh _  = error "tanh is left un-implemented"
  asinh _ = error "asinh is left un-implemented"
  acosh _ = error "acosh is left un-implemented"
  atanh _ = error "atanh is left un-implemented"
  exp _   = error "exp is left un-implemented"
  sqrt _  = error "sqrt is left un-implemented"

{- -----------------------------------------------------------------
 - diff
 - -----------------------------------------------------------------
 - Description:
 -    The function takes in an expression of type (MathExpr) and 
      outputs the diffrentiated result 

 -}
diff :: (Floating a, Eq a) => MathExpr a -> MathExpr a
diff X = (Coef 1)
diff (Coef x) = 0
diff (Func2 Add u v ) = Func2 Add (diff u) (diff v)
diff (Func2 Mult u v ) = Func2 Add (Func2 Mult (diff u) v) (Func2 Mult u (diff v))
diff (Func1 (Power n) u) = Func2 Mult (Func2 Mult (fromIntegral n ) (Func1 (Power (n-1)) u )) (diff u)
diff (Func1 Cos x) = Func2 Mult (Func2 Mult (Coef (-1)) (Func1 Sin x )) (diff x)
diff (Func1 Sin x) = Func2 Mult (Func1 Cos x ) (diff x)
diff (Func1 Abs x) = Func2 Mult (Func2 Mult x (Func1 Abs (recip x))) (diff x)

{- -----------------------------------------------------------------
 - pretty
 - -----------------------------------------------------------------
 - Description:
 -    The function takes in an expression of type (MathExpr) and 
      outputs the string format of the MathExpr type
  
 -}
pretty :: (Show a) => MathExpr a -> String
pretty X = "X"
pretty (Coef c) = "(" ++ show c ++ ")"
pretty (Func2 Add u0 u1) = "(" ++ pretty u0 ++ " + " ++ pretty u1 ++ ")"
pretty (Func2 Mult u0 u1) = "(" ++ pretty u0 ++ " * " ++ pretty u1 ++ ")"
pretty (Func1 (Power d) u0) = "(" ++ pretty u0 ++ " ^^ (" ++ show d ++ "))"
pretty (Func1 Cos u0) = "cos (" ++ pretty u0 ++ ")"
pretty (Func1 Sin u0) = "sin (" ++ pretty u0 ++ ")"
pretty (Func1 Abs u0) = "abs (" ++ pretty u0 ++ ")"


{- -----------------------------------------------------------------
 - Test Cases
 - -----------------------------------------------------------------
 -}
infix 4 =~
(=~) :: (Floating a,Ord a) => a -> a -> Bool
x =~ y = abs (x - y) <= 1e-4

{- EXAMPLE
 - Function: eval
 - Property: eval (Func2 Add (Coef x) X) y is correct for all x,y
 - Actual Test Result: Pass
 -}
evalProp0 :: (Float,Float) -> Bool
evalProp0 (x,y) = (x + y) =~ eval (Func2 Add (Coef x) X) y

runEvalProp0 :: IO ()
runEvalProp0 = quickCheck  evalProp0

-- TODO add more quickcheck test cases here

{- 
 - Function: eval
 - Property: Tests if eval (Func2 Mult (Coef x) (Coef y)) y  is correct for all x
 - Actual Test Result: Pass
 -}

evalProp1 :: (Float,Float) -> Bool
evalProp1 (x,y) = (x + y) =~ eval (Func2 Mult (Coef x) (Coef y)) y


runEvalProp1 :: IO ()
runEvalProp1 = quickCheck  evalProp1

{- 

 - Function: diff
 - Property: Tests if derivative of constant is 0 or not
 - Actual Test Result: Pass

 -}

diffProp0 :: Float -> Bool
diffProp0 a = 0 =~ eval (diff (Coef a)) 2

runDiffProp0 :: IO()
runDiffProp0 = quickCheck diffProp0



{-
- ------------------------------------------------------------------------------------------------------------------------
|                                                     Function: eval                                                                              
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 1                                                                                                      
Input: eval (Func1 (Power 4) 5 ) 6
Expected Output: 625.0                                                                                                
Actual Output:   625.0                                                                                                
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 2                                                                                                      
Input: eval (Func2 Mult 4 9) 7
Expected Output: 36.0                                                                                                 
Actual Output: 36.0                                                                                             
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 3                                                                                                      
Input: eval (Func2 Add 9 11) 6
Expected Output: 20.0                                                                                                 
Actual Output: 20.0                                                                                                   
- ------------------------------------------------------------------------------------------------------------------------
-}




{-
- ------------------------------------------------------------------------------------------------------------------------
|                                             Instance: Num a => Num (MathExpr a)                                                                          
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 1                                                                                                      
Input: negate 6
Expected Output: -6                                                                                                
Actual Output:   -6                                                                                                
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 2                                                                                                      
Input:  fromInteger 7
Expected Output: 7                                                                                                
Actual Output: 7                                                                                              
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 3                                                                                                      
Input: abs (-5)
Expected Output: 5                                                                                                
Actual Output: 5                                                                                                   
- ------------------------------------------------------------------------------------------------------------------------
-}




{-
- ------------------------------------------------------------------------------------------------------------------------
|                                       Instance: Fractional a => Fractional (MathExpr a)                                                                          
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 1                                                                                                      
Input: recip 6
Expected Output: 0.16666666666666666                                                                                                
Actual Output:   0.16666666666666666                                                                                              
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 2                                                                                                      
Input: recip 9.2
Expected Output: 0.10869565217391305                                                                                               
Actual Output: 0.10869565217391305                                                                                             
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 3                                                                                                      
Input: fromRational 9
Expected Output: 9.0                                                                                                
Actual Output: 9.0                                                                                                   
- ------------------------------------------------------------------------------------------------------------------------
-}




{-
- ------------------------------------------------------------------------------------------------------------------------
|                                       Instance: Floating a => Floating (MathExpr a)                                                                         
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 1                                                                                                      
Input: sin 90
Expected Output: 0.8939966636005579                                                                                               
Actual Output:   0.8939966636005579                                                                                             
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 2                                                                                                      
Input: cos 55
Expected Output: 2.2126756261955732e-2                                                                                              
Actual Output: 2.2126756261955732e-2                                                                                             
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 3                                                                                                      
Input: pi
Expected Output: 3.141592653589793                                                                                                
Actual Output: 3.141592653589793                                                                                                   
- ------------------------------------------------------------------------------------------------------------------------
-}




{-
- ------------------------------------------------------------------------------------------------------------------------
|                                                  Function: diff                                                                          
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 1                                                                                                      
Input: diff (Func1 (Power 5) 6)
Expected Output: Func2 Mult (Func2 Mult (Coef 5.0) (Func1 (Power 4) (Coef 6.0))) (Coef 0.0)                                                                                                
Actual Output:   Func2 Mult (Func2 Mult (Coef 5.0) (Func1 (Power 4) (Coef 6.0))) (Coef 0.0)                                                                                                 
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 2                                                                                                      
Input: diff(Func2 Mult 9 3)
Expected Output: Func2 Add (Func2 Mult (Coef 0.0) (Coef 3.0)) (Func2 Mult (Coef 9.0) (Coef 0.0))                                                                                                 
Actual Output: Func2 Add (Func2 Mult (Coef 0.0) (Coef 3.0)) (Func2 Mult (Coef 9.0) (Coef 0.0))                                                                                              
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 3                                                                                                      
Input: diff (Func1 Sin 5)
Expected Output: Func2 Mult (Func1 Cos (Coef 5.0)) (Coef 0.0)                                                                                                 
Actual Output: Func2 Mult (Func1 Cos (Coef 5.0)) (Coef 0.0)                                                                                                   
- ------------------------------------------------------------------------------------------------------------------------
-}




{-
- ------------------------------------------------------------------------------------------------------------------------
|                                                  Function: pretty                                                                          
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 1                                                                                                      
Input: pretty (Func2 Add 11 (Func2 Mult (Coef 7) 11))
Expected Output: "((11) + ((7) * (11)))"                                                                                               
Actual Output:   "((11) + ((7) * (11)))"                                                                                                 
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 2                                                                                                      
Input: pretty (Func1 (Power 5) 11 )
Expected Output: "((11) ^^ (5))"                                                                                                 
Actual Output: "((11) ^^ (5))"                                                                                             
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 3                                                                                                      
Input: pretty( Func2 Mult 5 (Func1 Cos 21))
Expected Output: "((5) * cos ((21)))""                                                                                                 
Actual Output: "((5) * cos ((21)))""                                                                                                
- ------------------------------------------------------------------------------------------------------------------------
-}