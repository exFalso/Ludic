{-# LANGUAGE TypeOperators #-}
module Vec (Vec, VecN1, module VecN) where

import VecN

type Vec = VecN Two
type VecN1 p = VecN (Guard (One :<=: p) p)
