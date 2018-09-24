{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Math.PAdic.Q where

import GHC.TypeNats
import Data.Proxy

import Math.PAdic.Z (Z(..))
import qualified Math.PAdic.Z as Z


-- Represent a p-adic number in the form p^n*z where z is a p-adic integer.
data Q (p :: Nat) = Q Int (Z p)


instance KnownNat p => Num (Q p) where
  (+) = add
  (*) = multiply
  negate = negate'
  abs = abs'
  signum = undefined
  fromInteger = fromInteger'


-- Temporary version.
instance KnownNat p => Show (Q p) where
  show (Q n z) = "Q " ++ show n ++ " " ++ show z


valuation :: forall p. KnownNat p => Q p -> Int
valuation (Q n z) = n + Z.valuation z


-- p-adic absolute value returning a p-adic result.
abs' :: forall p. KnownNat p => Q p -> Q p
abs' q = Q (-(valuation q)) 1


-- The more usual p-adic absolute value expressed over the reals (well, floats).
-- This looks like a candidate for a typeclass.
norm :: forall p a. (KnownNat p, Floating a) => Q p -> a
norm q = let p = fromIntegral $ natVal (Proxy @p)
             (Q v _) = abs q
         in p ** (fromIntegral v)


fromInteger' :: forall p. KnownNat p => Integer -> Q p
fromInteger' n = Q 0 (fromInteger n)


negate' :: forall p. KnownNat p => Q p -> Q p
negate' (Q n z) = Q n (negate z)


add :: forall p. KnownNat p => Q p -> Q p -> Q p
add (Q n1 (Z ds1)) (Q n2 (Z ds2)) =
  let n = minimum [n1, n2]
      z1 = Z $ replicate (n1 - n) 0 ++ ds1
      z2 = Z $ replicate (n2 - n) 0 ++ ds2
  in Q n (z1 + z2)


multiply :: forall p. KnownNat p => Q p -> Q p -> Q p
multiply (Q n1 z1) (Q n2 z2) = Q (n1 + n2) (z1 * z2)
