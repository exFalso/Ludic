{-# LANGUAGE DataKinds, GADTs, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, TypeFamilies, TypeOperators #-}

-- | A simple peano-indexed vector type, some instances and functions
module VecN ( VecN(..)
            , vhead
            , vtail
            , vappend
              
            , module Peano) where

import Peano
import Data.Foldable
import Data.Monoid
import Control.Applicative

infixr 6 :>

data VecN p a where
    VecZero :: VecN Zero a
    (:>) :: a -> VecN p a -> VecN (Succ p) a

instance (Eq a) => Eq (VecN Zero a) where
    _ == _ = True
instance (Eq a, Eq (VecN p a)) => Eq (VecN (Succ p) a) where
    (a :> as) == (b :> bs) = a == b && as == bs

instance (Ord a) => Ord (VecN Zero a) where
    _ <= _ = True
instance (Ord a, Ord (VecN p a)) => Ord (VecN (Succ p) a) where
    (a :> as) <= (b :> bs) = a <= b && as <= bs    

instance (Show a) => Show (VecN Zero a) where
    show _ = "VecZero"
instance (Show a, Show (VecN p a)) => Show (VecN (Succ p) a) where
    show (a :> as) = show a ++ " :> " ++ show as

instance Functor (VecN Zero) where
    fmap _ _ = VecZero
instance (Functor (VecN p)) => Functor (VecN (Succ p)) where
    fmap f (a :> as) = f a :> fmap f as

instance Foldable (VecN Zero) where
    foldMap _ VecZero = mempty
instance (Foldable (VecN p)) => Foldable (VecN (Succ p)) where
    foldMap f (a :> as) = f a `mappend` foldMap f as

instance Applicative (VecN Zero) where
    pure _ = VecZero
    _ <*> _ = VecZero
instance (Applicative (VecN p)) => Applicative (VecN (Succ p)) where
    pure a = a :> pure a
    (f :> fs) <*> (a :> as) = f a :> (fs <*> as)

vtail :: VecN (Succ p) a -> VecN p a
vtail (_ :> as) = as

vhead :: VecN (Succ p) a -> a
vhead (a :> _) = a

vappend :: VecN p1 a -> VecN p2 a -> VecN (p1 :+ p2) a
vappend VecZero bs = bs
vappend (a :> as) bs = a :> vappend as bs
