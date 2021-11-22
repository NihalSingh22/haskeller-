{-|
Module      : 1JC3-Assign3.Assign_3.hs
Copyright   :  (c) Curtis D'Alves 2021
License     :  GPL (see the LICENSE file)
Maintainer  :  none
Stability   :  experimental
Portability :  portable

Description:
  Assignment 3 - McMaster CS 1JC3 2021
-}
module Assign_3 where

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
-- Date: 2021-11-12 (y/m/d)
-- Assignment 3

macid :: String
macid = "singhn77"

{- -----------------------------------------------------------------
 - Datatypes
 - -----------------------------------------------------------------
 -}
data Graph a = Graph { getNodes :: [Node a]
                     , getEdges :: [Edge] }
  deriving (Show,Eq)

type Edge = (NodeID,NodeID)
type NodeID = Int

data Node a = Node { getNodeID  :: NodeID,
                     getNodeVal :: a }
  deriving (Show,Eq,Ord)


{- -----------------------------------------------------------------
 - Example Graph
 - -----------------------------------------------------------------
 -              -----        ----------
 -              | A |------->| C |    |
 -              -----        ----- <---
 -                |           |
 -                |     ------|
 -                v     |
 -              ----- <-|
 -              | B |
 -              -----
 -}
nodeA,nodeB,nodeC :: Node Char
nodeA = Node 0 'A'
nodeB = Node 1 'B'
nodeC = Node 2 'C'

exGraph :: Graph Char
exGraph =
  let
    nodes = [nodeA,nodeB,nodeC]
    edges = [(0,1),(0,2),(2,2),(2,1)]
  in Graph nodes edges

{- -----------------------------------------------------------------
 - maxNodeID
 - -----------------------------------------------------------------
 - Description:
 -    Function that returns the largest NodeID in a Graph 
 - -----------------------------------------------------------------
 - |   Input  -> A user defined Graph 
 - -----------------------------------------------------------------
 - |   Output -> Returns the largest NodeID in the graph
 - -----------------------------------------------------------------

 -}
maxNodeID :: Graph a -> Maybe NodeID
maxNodeID (Graph [] edges) = Nothing
maxNodeID (Graph a edges) = Just (maximum [getNodeID b | b <- a])


{- -----------------------------------------------------------------
 - insertNode
 - -----------------------------------------------------------------
 - Description:
 -    Function that inserts a new Node value into a Graph. The new 
      NodeID should be the previously largest NodeID plus one.
 - -----------------------------------------------------------------
 - |   Input  -> A node value 'v'
 - |          -> A user defined Graph data type
 - -----------------------------------------------------------------
 - |   Output -> Returns graph with node value of the input 'v' and
   |             nodeID which is the maxNodeID plus 1
 - -----------------------------------------------------------------

 -}
insertNode :: a -> Graph a -> Graph a
insertNode v (Graph [] edges) = Graph [Node 0 v] edges
insertNode v (Graph a edges) = 
  let
    firstNode  = maximum [getNodeID y | y <- a] + 1
    secondNode = a ++ [Node firstNode v]
  
  in
    Graph secondNode edges


{- -----------------------------------------------------------------
 - removeNode
 - -----------------------------------------------------------------
 - Description:
 -    Function that removes any Node with the given NodeID
      including occurrences of NodeID in edges from the given Graph
 - -----------------------------------------------------------------
 - |   Input  -> A node id to be removed
 - |          -> A user defined Graph data type
 - -----------------------------------------------------------------
 - |   Output -> Returns graph with the removal of the indicated 
 - |             NodeID
 - -----------------------------------------------------------------
 -}


removeNode :: NodeID -> Graph a -> Graph a
removeNode nID (Graph a edges) = 
  let 
    getnode = [x | x <- a , getNodeID x /= nID]
    getEdge = [y | y<- edges , fst y /= nID && snd y /=nID]
  in Graph getnode getEdge 

{- -----------------------------------------------------------------
 - lookupNode
 - -----------------------------------------------------------------
 - Description:
 -    Function that returns the Node corresponding to the given 
      NodeID in the given Graph. If no Node with NodeID exists,
      the function returns Nothing
 - -----------------------------------------------------------------
 - |   Input  -> A node id to be removed
 - |          -> A user defined Graph data type
 - -----------------------------------------------------------------
 - |   Output -> Returns graph with the corresponding given NodeID
 - |             in the group
 - -----------------------------------------------------------------
 -}
 
lookupNode :: NodeID -> Graph a -> Maybe (Node a)
lookupNode nID (Graph [] x) = Nothing
lookupNode nID (Graph (y:ys) x) 
  | nID == getNodeID y = Just y
  | otherwise = lookupNode nID (Graph ys x)


{- -----------------------------------------------------------------
 - insertEdge
 - -----------------------------------------------------------------
 - Description:
 -   Function  that inserts an edge from the Node with the 
     given NodeID in the first part of the tuple to the Node with 
     the given NodeID in the second part of the tuple. 
     If the edge already exists, it should NOT introduce a duplicate. 
     If the nodes corresponding to either of the given NodeIDs do 
     not already exist, the function should return Nothing
 - -----------------------------------------------------------------
 - |   Input  -> An edge (n1,n2)
 - |          -> A user defined Graph data type
 - -----------------------------------------------------------------
 - |   Output -> Returns graph with the new edge added
 - -----------------------------------------------------------------
 
 -}
insertEdge :: Eq a => (NodeID,NodeID) -> Graph a -> Maybe (Graph a)
insertEdge _       (Graph [] _)  = Nothing
insertEdge (n1,n2) (Graph ns es)
  | not containsBothNodes = Nothing
  | containsEdge          = Just (Graph ns es)
  | otherwise             = Just (Graph ns (es ++ [(n1,n2)]))
  where
    containsBothNodes :: Bool 
    containsBothNodes = (lookupNode n1 (Graph ns es) /= Nothing) && (lookupNode n2 (Graph ns es) /= Nothing )
    containsEdge :: Bool
    containsEdge = (n1,n2) `elem` es
 

{-
- ------------------------------------------------------------------------------------------------------------------------
|                                                Function: maxNodeID                                                                                   
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 1                                                                                                      
Input: Graph [(Node 0 1) , (Node 1 11) , (Node 2 38)] [(0,1) , (0,2) , (2,1)]                                            
Expected Output: Just 2                                                                                                  
Actual Output:   Just 2                                                                                                  
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 2                                                                                                      
Input: Graph [(Node 56 45) , (Node 179 46) , (Node 784 48)] [(56,179) , (56,784) , (784,179)]                            
Expected Output: Just 179                                                                                                 
Actual Output: Just 179                                                                                                  
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 3                                                                                                      
Input: Graph [(Node 20 'A') , (Node 21 'B') , (Node 22 'C')] [(20,21) , (20,22) , (21,20)]                               
Expected Output: Just 21                                                                                                  
Actual Output: Just 21                                                                                                   
- ------------------------------------------------------------------------------------------------------------------------
-}


{-
- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
|                                                 Function: insertNode                                                   
- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Test Case Number: 1                                                                                                      
Input: insertNode 69 (Graph [] [])                                                                                       
Expected Output: Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 69}], getEdges = []}                               
Actual Output:   Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 69}], getEdges = []}                                                                                                 
- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Test Case Number: 2                                                                                                      
Input: insertNode 409 (Graph [(Node 0 53) , (Node 1 87)] [(0,1)])                           
Expected Output: Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 53},Node {getNodeID = 1, getNodeVal = 87},Node {getNodeID = 2, getNodeVal = 409}], getEdges = [(0,1)]}                                                                                                
Actual Output: Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 53},Node {getNodeID = 1, getNodeVal = 87},Node {getNodeID = 2, getNodeVal = 409}], getEdges = [(0,1)]}                                                                                                 
- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Test Case Number: 3                                                                                                      
Input: insertNode 55 (Graph [(Node 7 943) , (Node 8 421)] [(7,8)])                              
Expected Output: Graph {getNodes = [Node {getNodeID = 7, getNodeVal = 943},Node {getNodeID = 8, getNodeVal = 421},Node {getNodeID = 9, getNodeVal = 55}], getEdges = [(7,8)]}                                                                                                 
Actual Output: Graph {getNodes = [Node {getNodeID = 7, getNodeVal = 943},Node {getNodeID = 8, getNodeVal = 421},Node {getNodeID = 9, getNodeVal = 55}], getEdges = [(7,8)]}                                                                                                   
- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-}


{-
- ---------------------------------------------------------------------------------------------------------------------------------------------
|                                                 Function: removeNode                                                   
- ---------------------------------------------------------------------------------------------------------------------------------------------
Test Case Number: 1                                                                                                      
Input: removeNode 1 (Graph [(Node 0 22) , (Node 1 66)] [(0,1)])                                                                                       
Expected Output: Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 22}], getEdges = []}                              
Actual Output:   Graph {getNodes = [Node {getNodeID = 0, getNodeVal = 69}], getEdges = []}                                                                                                 
- ---------------------------------------------------------------------------------------------------------------------------------------------
Test Case Number: 2                                                                                                      
Input: removeNode 21 (Graph [(Node 33 89) , (Node 34 111)] [(33,34)])                           
Expected Output: Graph {getNodes = [Node {getNodeID = 33, getNodeVal = 89},Node {getNodeID = 34, getNodeVal = 111}], getEdges = [(33,34)]}
Actual Output: Graph {getNodes = [Node {getNodeID = 33, getNodeVal = 89},Node {getNodeID = 34, getNodeVal = 111}], getEdges = [(33,34)]}
- ---------------------------------------------------------------------------------------------------------------------------------------------
Test Case Number: 3                                                                                                      
Input: removeNode 44 (Graph [(Node 90 'C') , (Node 91 'D')] [(90,91)])                             
Expected Output: Graph {getNodes = [Node {getNodeID = 90, getNodeVal = 'C'},Node {getNodeID = 91, getNodeVal = 'D'}], getEdges = [(90,91)]}
Actual Output: Graph {getNodes = [Node {getNodeID = 90, getNodeVal = 'C'},Node {getNodeID = 91, getNodeVal = 'D'}], getEdges = [(90,91)]}
- ---------------------------------------------------------------------------------------------------------------------------------------------
-}



{-
- ------------------------------------------------------------------------------------------------------------------------
|                                                 Function: lookupNode                                                  
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 1                                                                                                      
Input: lookupNode 1 (Graph [(Node 3 'G') ,(Node 4 'H') , (Node 5 'I')] [(3,4) , (3,5) , (5,4)])                                                                                      
Expected Output: Nothing                              
Actual Output:   Nothing                                                                                                
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 2                                                                                                      
Input: lookupNode 66 (Graph [(Node 66 'G') ,(Node 67 'H') , (Node 68 'I')] [(66,67) , (66,68) , (68,67)])                           
Expected Output: Just (Node {getNodeID = 66, getNodeVal = 'G'})
Actual Output: Just (Node {getNodeID = 66, getNodeVal = 'G'})
- ------------------------------------------------------------------------------------------------------------------------
Test Case Number: 3                                                                                                      
Input: lookupNode 81 (Graph [(Node 81 4) ,(Node 82 8) , (Node 83 9)] [(81,82) , (81,83) , (83,82)])                           
Expected Output: Just (Node {getNodeID = 81, getNodeVal = 4})
Actual Output: Just (Node {getNodeID = 81, getNodeVal = 4})
- ------------------------------------------------------------------------------------------------------------------------
-}


{-
- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
|                                                 Function: insertEdge                                                 
- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Test Case Number: 1                                                                                                      
Input: insertEdge (3,5) (Graph [] [] )                                                                                       
Expected Output: Nothing                              
Actual Output:   Nothing                                                                                                
- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Test Case Number: 2                                                                                                      
Input: insertEdge (5,6) (Graph [(Node 5 13) , (Node 6 99)] [])                     
Expected Output: Just (Graph {getNodes = [Node {getNodeID = 5, getNodeVal = 13},Node {getNodeID = 6, getNodeVal = 99}], getEdges = [(5,6)]})
Actual Output: Just (Graph {getNodes = [Node {getNodeID = 5, getNodeVal = 13},Node {getNodeID = 6, getNodeVal = 99}], getEdges = [(5,6)]})
- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
Test Case Number: 3                                                                                                                                                                                  
Input: insertEdge (5,6) (Graph [(Node 4 98) , (Node 5 71) , (Node 6 4)] [(4,5) , (0,6) ,(6,5)])                                                                                                      
Expected Output: Just (Graph {getNodes = [Node {getNodeID = 4, getNodeVal = 98},Node {getNodeID = 5, getNodeVal = 71},Node {getNodeID = 6, getNodeVal = 4}], getEdges = [(4,5),(0,6),(6,5),(5,6)]})  
Actual Output: Just (Graph {getNodes = [Node {getNodeID = 4, getNodeVal = 98},Node {getNodeID = 5, getNodeVal = 71},Node {getNodeID = 6, getNodeVal = 4}], getEdges = [(4,5),(0,6),(6,5),(5,6)]})    
- ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-}