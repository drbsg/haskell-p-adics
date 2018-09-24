{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Math.PAdic.Z where

import GHC.TypeNats
import Data.List (inits, unfoldr)
import Data.Proxy


data Z (p :: Nat) = Z [Integer]


-- Temporary version.
instance KnownNat p => Show (Z p) where
  show (Z ds) = "Z " ++ show (take 10 ds)


instance KnownNat p => Num (Z p) where
  (+) = add
  (*) = multiply
  negate = negate'
  abs = undefined               -- Cannot be defined for Z p with the usual signature.
  signum = undefined
  fromInteger = fromInteger'


valuation :: forall p. KnownNat p => Z p -> Int
valuation (Z ds) = length $ takeWhile (==0) ds


-- This looks like a candidate for a typeclass.
norm :: forall p a. (KnownNat p, Floating a) => Z p -> a
norm z = let p = fromIntegral $ natVal (Proxy @p)
             v = fromIntegral $ valuation z
         in p ** (-v)


add :: forall p. KnownNat p => Z p -> Z p -> Z p
add (Z digits1) (Z digits2) = Z $ go digits1 digits2 0
  where go (x:xs) (y:ys) carry =
          let p = fromIntegral $ natVal (Proxy @p)
              sum = x + y + carry
              (carry', sum') = sum `divMod` p
          in [sum'] ++ go xs ys carry'


multiply :: forall p. KnownNat p => Z p -> Z p -> Z p
multiply (Z digits1) (Z digits2) = Z $ go inits1 inits2 0
  where inits1 = tail $ inits digits1
        inits2 = fmap reverse . tail $ inits digits2
        go (x:xs) (y:ys) carry =
          let p = fromIntegral $ natVal (Proxy @p)
              s = carry + (sum $ zipWith (*) x y)
              (carry', sum') = s `divMod` p
          in [sum'] ++ go xs ys carry'


negate' :: forall p. KnownNat p => Z p -> Z p
negate' (Z digits) = Z $ go digits 0
  where go (x:xs) carry =
          let p = fromIntegral $ natVal (Proxy @p)
              d = - x - carry
              (d', carry') = if d < 0 then (p + d, 1) else (d, 0)
          in [d'] ++ go xs carry'


fromInteger' :: forall p. KnownNat p => Integer -> Z p
fromInteger' n = Z $ unfoldr f n
  where f a = let p = fromIntegral $ natVal (Proxy @p)
                  (q, r) = a `divMod` p
              in Just (r, q)
