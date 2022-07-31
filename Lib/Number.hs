{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Lib.Number where

import Prelude
import GHC.Real

data Number'
    = Integer Integer
    | Float Float

isInteger :: Number' -> Bool
isInteger (Integer _) = True
isInteger _ = False

round' :: Number' -> Number'
round' (Float f) = Integer $ round f
round' a = a

instance Eq Number' where
    (==) (Integer i1) (Integer i2) =            i1 == i2
    (==) (Integer i ) (Float   f ) = fromInteger i == f
    (==) (Float   f1) (Float   f2) =            f1 == f2
    (==)          n1           n2  =            n2 == n1

instance Ord Number' where
    compare (Integer i1) (Integer i2) = compare i1 i2
    compare (Integer i ) (Float   f ) = compare (fromInteger i) f
    compare (Float   f1) (Float   f2) = compare f1 f2
    compare n1 n2 = compare n2 n1

instance Show Number' where
    show (Integer  i) = show i
    show (Float    f) = show f

instance Num Number' where
    (+) (Integer i1) (Integer i2) = Integer (i1 + i2)
    (+) (Integer i ) (Float   f ) = Float   (fromInteger i + f)
    (+) (Float   f1) (Float   f2) = Float   (f1 + f2)
    (+) n1 n2 = n2 + n1

    (*) (Integer i1) (Integer i2) = Integer (i1 * i2)
    (*) (Integer i ) (Float   f ) = Float   (fromInteger i * f)
    (*) (Float   f1) (Float   f2) = Float   (f1 * f2)
    (*) n1 n2 = n2 * n1

    abs (Integer i) = Integer (abs i)
    abs (Float   f) = Float   (abs f)

    signum x
      | abs x == 0 = 0
      | abs x == x = 1
      | otherwise = -1

    fromInteger x = Integer x

    negate (Integer i) = Integer (-i)
    negate (Float   f) = Float   (-f)

instance Fractional Number' where
    fromRational x = Float (fromRational x)

    recip (Integer i) = Float (recip (fromInteger i))
    recip (Float   f) = Float (recip f)

instance Floating Number' where
    pi = Float pi
    exp = toFloating exp
    log = toFloating log
    sin = toFloating sin
    cos = toFloating cos
    asin = toFloating asin
    acos = toFloating acos
    atan = toFloating atan
    sinh = toFloating sinh
    cosh = toFloating cosh
    asinh = toFloating asinh
    acosh = toFloating acosh
    atanh = toFloating atanh

instance Real Number' where
  toRational (Integer i) = toRational i
  toRational (Float   f) = toRational f

instance Enum Number' where
  toEnum i = Integer (toInteger i)
  
  fromEnum (Integer i) = fromInteger i
  fromEnum (Float   f) = round f

instance Integral Number' where
  quotRem (Integer i1) (Integer i2) = (Integer (quot i1 i2), Integer (rem i1 i2))
  
  quotRem (Integer i) (Float f)
    | fromInteger i < f = (Integer 0, Integer i)
    |otherwise = (1 + fst (quotRem (Float (fromInteger i - f)) (Float f)), snd (quotRem (Float (fromInteger i - f)) (Float f)))
  
  quotRem (Float f) (Integer i)
    | f < fromInteger i = (Integer 0, Float f)
    |otherwise = (1 + fst (quotRem (Float (f - fromInteger i)) (Integer i)), snd (quotRem (Float (f - fromInteger i)) (Integer i)))
  
  quotRem (Float f1) (Float f2)
    | f1 < f2 = (Integer 0, Float f1)
    |otherwise = (1 + fst (quotRem (Float (f1 - f2)) (Float f2)), snd (quotRem (Float (f1 - f2)) (Float f2)))

  toInteger (Integer i) = i
  toInteger (Float   f) = round f

toFloating :: (Float -> Float) -> Number' -> Number'
toFloating f (Integer i ) = Float $ f $ fromInteger i
toFloating f (Float   fl) = Float $ f fl
