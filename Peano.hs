{-# LANGUAGE TypeFamilies, DataKinds, TypeOperators #-}

module Peano -- ( Peano(..)
                  -- )
       where

data Peano
    = Zero
    | Succ Peano
    deriving (Show, Eq, Ord)

type One = Succ Zero
type Two = Succ One
type Three = Succ Two
type Four = Succ Three
type Five = Succ Four
type Six = Succ Five

type family (a :: Peano) :+ (b :: Peano) :: Peano
type instance Zero :+ b = b
type instance (Succ a) :+ b = Succ (a :+ b)

type family Guard (b :: Bool) (a :: Peano) :: Peano
type instance Guard True a = a

type family (a :: Peano) :<=: (b :: Peano) :: Bool
type instance Zero :<=: b = True
type instance (Succ a) :<=: Zero = False
type instance (Succ a) :<=: (Succ b) = a :<=: b
