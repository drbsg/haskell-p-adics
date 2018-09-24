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
-- All leading zero digits are given implicitly by the valuation.
data Q (p :: Nat) = Q { valuation :: Valuation, digits :: [Integer] }


fromInteger' :: forall p. KnownNat p => Integer -> Q p
fromInteger' n = Q v ds'
  where p = fromIntegral $ natVal (Proxy @p) :: Integer
        go n' = let (q, r) = n' `divMod` p in Just (r, q)
        ds = unfoldr go n
        v = if n == 0 then PosInf else Finite $ length $ takeWhile (==0) ds
        ds' = dropWhile (==0) ds


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
      v' = Finite (length $ takeWhile (==0) ds) + Finite v
      ds' = dropWhile (==0) ds
  in Q v' ds'
  where go (x:xs) (y:ys) carry =
          let p = fromIntegral $ natVal (Proxy @p)
              sum = x + y + carry
              (carry', sum') = sum `divMod` p
          in [sum'] ++ go xs ys carry'


-- Show the p-adic number up to the given order in p.
-- NOTE: Not implementing Show at this time.
showUpto :: forall p. KnownNat p => Int -> Q p -> String
showUpto n q = intercalate " + " $ fmap (\(e, d) -> show d ++ "p^" ++ show e) $ takeWhile (\(e, _) -> e <= n) $ expansion q


instance KnownNat p => Num (Q p) where
  (+) = add
  (*) = undefined
  negate = undefined
  abs = undefined
  signum = undefined
  fromInteger = fromInteger'
