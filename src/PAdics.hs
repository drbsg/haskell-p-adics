{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module PAdics where

import GHC.TypeNats

import Data.ExtendedReal
import Data.List (unfoldr, intercalate)
import Data.Proxy

type Valuation = Extended Int

-- Represent a p-adic number as a p-adic integer and a power of p.
data Q (p :: Nat) = Q { offset :: Valuation, digits :: [Integer] }


valuation :: forall p. KnownNat p => Q p -> Valuation
valuation (Q NegInf _) = undefined
valuation (Q PosInf _) = PosInf
valuation (Q v ds) = v + (Finite . length $ takeWhile (==0) ds)


fromInteger' :: forall p. KnownNat p => Integer -> Q p
fromInteger' n = Q v ds
  where p = fromIntegral $ natVal (Proxy @p) :: Integer
        go n' = let (q, r) = n' `divMod` p in Just (r, q)
        ds = unfoldr go n
        v = if n == 0 then PosInf else Finite 0


expansion :: forall p. KnownNat p => Q p -> [(Int, Integer)]
expansion (Q v ds) = case v of
  NegInf -> undefined
  PosInf -> zip [0..] (repeat 0)
  Finite v' -> let prefix = zip [0..(v'-1)] (repeat 0)
                   suffix = zip [v'..] ds
               in prefix ++ suffix


add :: forall p. KnownNat p => Q p -> Q p -> Q p
add (Q NegInf _) _ = undefined
add _ (Q NegInf _) = undefined
add (Q PosInf _) q2 = q2
add q1 (Q PosInf _) = q1
add (Q (Finite v1) ds1) (Q (Finite v2) ds2) =
  let v = minimum [v1, v2]
      ds1' = replicate (v1 - v) 0 ++ ds1
      ds2' = replicate (v2 - v) 0 ++ ds2
      ds = go ds1' ds2' 0
      v' = Finite v
  in Q v' ds
  where go (x:xs) (y:ys) carry =
          let p = fromIntegral $ natVal (Proxy @p)
              sum = x + y + carry
              (carry', sum') = sum `divMod` p
          in [sum'] ++ go xs ys carry'


negate' :: forall p. KnownNat p => Q p -> Q p
negate' (Q NegInf _) = undefined
negate' q1@(Q PosInf _) = q1
negate' (Q (Finite v) ds) = Q (Finite v) (go ds 0)
  where go (x:xs) carry =
          let p = fromIntegral $ natVal (Proxy @p)
              d = - x - carry
              (d', carry') = if d < 0 then (p + d, 1) else (d, 0)
          in [d'] ++ go xs carry'


-- Show the p-adic number up to the given order in p.
-- NOTE: Not implementing Show at this time.
showUpto :: forall p. KnownNat p => Int -> Q p -> String
showUpto n q = intercalate " + " $ fmap (\(e, d) -> show d ++ "p^" ++ show e) $ takeWhile (\(e, _) -> e <= n) $ expansion q


instance KnownNat p => Num (Q p) where
  (+) = add
  (*) = undefined
  negate = negate'
  abs = undefined
  signum = undefined
  fromInteger = fromInteger'
