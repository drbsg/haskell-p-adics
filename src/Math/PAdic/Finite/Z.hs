{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Math.PAdic.Finite.Z where

import GHC.Natural
import GHC.TypeNats
import Data.Proxy

import Math.PAdic.Z (Z(..))
import qualified Math.PAdic.Z as Z

-- The full blown p-adic integers are not as useful as they could be because of
-- non-termination of important operations such as equality, signum and showing
-- them. Here we add another type parameter which specifies the desired
-- precision of the result.


data Zn (p :: Nat) (n :: Nat) = Zn (Z p)


instance (KnownNat p, KnownNat n) => Show (Zn p n) where
  show (Zn (Z ds)) = let n = fromIntegral $ natVal (Proxy @n)
                     in "Zn " ++ show (take n ds)


instance (KnownNat p, KnownNat n) => Num (Zn p n) where
  (Zn z1) + (Zn z2) = Zn $ z1 + z2
  (Zn z1) * (Zn z2) = Zn $ z1 * z2
  negate (Zn z) = Zn $ negate z
  abs = undefined               -- Still does not fit Num's signature.
  signum (Zn z) = let n = fromIntegral $ natVal (Proxy @n)
                  in Zn $ Z.signum' n z
  fromInteger = Zn . fromInteger


instance (KnownNat p, KnownNat n) => Eq (Zn p n) where
  (Zn (Z ds1)) == (Zn (Z ds2)) = let n = fromIntegral $ natVal (Proxy @n)
                                 in take n ds1 == take n ds2
