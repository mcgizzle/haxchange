module Test where

import Control.Monad.State
import Data.List

data Tree a = Empty | Leaf (Node a) | Node a (Tree a) (Tree a)

lowestCommon :: a -> a -> Tree a -> a
lowestCommon x y t = minimum $ intersect listA listB 
        where listA = execState (findAncestors x t) [] 
              listB = execState (findAncestors y t) []

findAncestors :: a -> Tree a -> State [a] Bool
findAncestors _ Empty = return False
findAncestors x (Leaf x) = return True
findAncestors _ (Leaf _) = return False
findAncestors x (Node x (Tree _) (Tree _)) = return True
findAncestors x (Node y (Tree a) (Tree b)) = do 
        left <- findAncestors x a 
        right <- findAncestors x b
        if left || right then addAncestor y
                           else return False
      
addAncestor :: a -> State [a] Bool
addAncestor a = modify' (++ a) >> return True

