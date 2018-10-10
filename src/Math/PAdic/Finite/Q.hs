{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Math.PAdic.Finite.Q where

import GHC.Natural
import GHC.TypeNats
import Data.Proxy

import Math.PAdic.Q (Q(..))
import qualified Math.PAdic.Q as Q
import Math.PAdic.Z (Z(..))


-- Finite precision p-adics in the same vein as Finite.Z.

data Qn (p :: Nat) (n :: Nat) = Qn (Q p)


instance (KnownNat p, KnownNat n) => Show (Qn p n) where
  show (Qn (Q e (Z ds))) = let n = fromIntegral $ natVal (Proxy @n)
                           in "Qn " ++ show e ++ " " ++ show (take n ds)


instance (KnownNat p, KnownNat n) => Num (Qn p n) where
  (Qn q1) + (Qn q2) = Qn $ q1 + q2
  (Qn q1) * (Qn q2) = Qn $ q1 * q2
  negate (Qn q) = Qn $ negate q
  abs (Qn q) = Qn $ abs q
  signum = undefined            -- How is this defined?
  fromInteger = Qn . fromInteger


instance (KnownNat p, KnownNat n) => Eq (Qn p n) where
  (Qn (Q e1 (Z ds1))) == (Qn (Q e2 (Z ds2))) =
    let n = fromIntegral $ natVal (Proxy @n)
    in e1 == e2 && take n ds1 == take n ds2


instance (KnownNat p, KnownNat n) => Fractional (Qn p n) where
  recip (Qn q) = Qn $ recip q
  (Qn q1) / (Qn q2) = Qn $ q1 / q2
  fromRational = Qn . fromRational
