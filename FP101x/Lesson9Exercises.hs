{-# LANGUAGE NPlusKPatterns #-}

import Data.List
import Data.Char
import Unsafe.Coerce

data Tree = Leaf Int
          | Node Tree Int Tree

tree = Node (Node (Leaf 1) 3 (Leaf 4))
            5
            (Node (Leaf 6) 7 (Leaf 9))

occurs :: Int -> Tree -> Bool
occurs m (Leaf n)     = m==n
occurs m (Node l n r) = m==n
                      || occurs m l
                      || occurs m r

occursB :: Int -> Tree -> Bool
occursB m (Leaf n)     = m==n
occursB m (Node l n r) | m==n = True
                       | m <n = occursB m l
                       | m >n = occursB m r

data Nat = Zero
         | Succ Nat
         deriving Show

z = Zero
one = Succ Zero
two = Succ (Succ Zero)
three = Succ (Succ (Succ Zero))

{---
 - Exercise 0
 ---}
natToIntegerA :: Nat -> Int
natToIntegerA Zero = 0
natToIntegerA (Succ n) = natToIntegerA n + 1

natToIntegerB :: Nat -> Int
natToIntegerB (Succ n) = natToIntegerB n + 1
natToIntegerB Zero = 0

{-
natToIntegerC :: Nat -> Int
-}
natToIntegerD :: Nat -> Int
natToIntegerD (Succ n) = 1 + natToIntegerD n
natToIntegerD Zero = 0

natToIntegerE :: Nat -> Int
natToIntegerE Zero = 1
natToIntegerE (Succ n) = (1 + natToIntegerD n) - 1

natToIntegerF :: Nat -> Int
natToIntegerF = head . m
  where m Zero = [0]
        m (Succ n) = [sum [x | x <- (1 : m n)]]

natToIntegerG :: Nat -> Int
natToIntegerG = \ n -> genericLength [c | c <- show n, c == 'S']
{-
natToIntegerH :: Nat -> Int
-}


{---
 - Exercise 1
 ---}
integerToNatA :: Int -> Nat
integerToNatA 0 = Zero
integerToNatA (n+1) = Succ (integerToNatA n)

integerToNatB :: Int -> Nat
integerToNatB 0 = Succ Zero
integerToNatB n = (Succ (integerToNatB n))

{-
integerToNatC :: Int -> Nat
integerToNatC n = product [(unsafeCoerce c) :: Integer | c <- show n]
-}

integerToNatE :: Int -> Nat
integerToNatE (n+1) = Succ (integerToNatE n)
integerToNatE 0 = Zero

integerToNatF :: Int -> Nat
integerToNatF (n+1) = let m = integerToNatF n in Succ m
integerToNatF 0 = Zero

{-
integerToNatG :: Int -> Nat
integerToNatG = head . m
  where {
        ; m 0 = [0]
        ; m (n + 1) = [sum [x | x <- (1 : m n)]]
        }
-}

integerToNatH :: Int -> Nat
integerToNatH = \ n -> genericLength [c | c <- show n, isDigit c]
