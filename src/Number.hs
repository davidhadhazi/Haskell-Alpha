module Number (Number' (..), round') where

import GHC.Real
import CReal

data Number'
  = Integer Integer
  | Creal   CReal
  | Frac    (Integer, Integer)

instance Show Number' where
  show (Integer           i) = show i
  show (Creal             c) = if round c == c then init (init (show c)) else show c 
  show (Frac     (num, dom)) = show (numerator (num % dom)) ++ (if 1 == denominator (num % dom) then "" else "/" ++ show (denominator (num % dom)) ++ "  " ++ show (Creal ((fromIntegral num) / (fromIntegral dom))))

round' :: Number' -> Number'
round' (Creal c) = if round c == c then Integer (round c) else (Creal c)
-- round' (Integer i) = Integer i
-- round' (Frac (n,d)) = Frac (n,d)
round' e = e

instance Eq Number' where
  (==) (Integer   i1) (Integer   i2) = i1 == i2
  (==) (Creal     f1) (Creal     f2) = f1 == f2
  (==) (Frac (n1,d1)) (Frac (n2,d2)) = (numerator (n1 % d1) == numerator (n2 % d2)) && denominator (n1 % d1) == denominator (n2 % d2)
  (==)            _             _  = False

instance Ord Number' where
  compare (Integer i1) (Integer i2) = compare i1 i2
  compare (Integer i ) (Creal   f ) = compare (fromInteger i) f
  compare (Integer i ) (Frac (n,d)) = compare (i * d) n
  compare (Creal   c ) (Integer i ) = compare c $ fromInteger i
  compare (Creal   c1) (Creal   c2) = compare c1 c2
  compare (Creal   c ) (Frac (n,d)) = compare c $ fromInteger n / fromInteger d
  compare (Frac (n,d)) (Integer i ) = compare n (i * d)
  compare (Frac (n,d)) (Creal   c ) = compare (fromInteger n / fromInteger d) c
  compare (Frac (n1,d1)) (Frac (n2,d2)) = compare (n1 * d2) (n2 * d1)

instance Num Number' where
  (+) (Integer   i1) (Integer   i2) = Integer (i1 + i2)
  (+) (Integer   i ) (Creal     f ) = Creal   (fromInteger i + f)
  (+) (Integer   i ) (Frac   (n,d)) = Frac    (i * d + n, d)
  (+) (Creal     f1) (Creal     f2) = Creal   (f1 + f2)
  (+) (Creal     f ) (Frac   (n,d)) = Creal f + Integer n / Integer d
  (+) (Frac (n1,d1)) (Frac (n2,d2)) = Frac (numerator ((n1 * d2 + n2 * d1) % (d1 * d2)), denominator ((n1 * d2 + n2 * d1) % (d1 * d2)))
  (+) n1 n2 = n2 + n1

  (*) (Integer i1) (Integer i2) = Integer (i1 * i2)
  (*) (Integer i ) (Creal   f ) = Creal   (fromInteger i * f)
  (*) (Integer i ) (Frac (n,d)) = if (mod i d == 0)
    then Integer (div (i * n) d)   
    else Frac (numerator ((i * n)% d), denominator ((i * n) % d))
  (*) (Creal   f ) (Frac (n,d)) = Creal f * Integer n / Integer d
  (*) (Creal   f1) (Creal   f2) = Creal   (f1 * f2)
  (*) (Frac (n1,d1)) (Frac (n2,d2)) = Frac (n1*n2, d1*d2)
  (*) n1 n2 = n2 * n1

  abs (Integer i) = Integer (abs i)
  abs (Creal   f) = Creal   (abs f)
  abs (Frac (n,d)) = Frac (abs n, abs d)

  signum x
    | abs x == 0 = 0
    | abs x == x = 1
    | otherwise = -1

  fromInteger x = Integer x

  negate (Integer i) = Integer (-i)
  negate (Creal   f) = Creal   (-f)
  negate (Frac (n,d)) = Frac (-n,d)

instance Fractional Number' where
  fromRational x = Creal (fromRational x)

  (/) (Integer i1) (Integer i2) = Frac (numerator (i1 % i2), denominator (i1 % i2))
  (/) (Integer i) (Creal  f) = Creal $ fromInteger i / f
  (/) (Integer i) (Frac (n,d)) = Frac (i * d, n)
  (/) (Creal c) (Integer i) = Creal $ c / fromInteger i
  (/) (Creal c1) (Creal c2) = Creal $ c1 / c2
  (/) (Creal c) (Frac (n,d)) = if fromIntegral (round c * d) == c * fromInteger d then Frac (round c * d, n) else Creal $ c / fromInteger n * fromInteger d
  (/) (Frac (n,d)) (Integer i) = Frac (numerator (n % (d * i)), denominator (n % (d * i)))
  (/) (Frac (n,d)) (Creal f) = Creal $ fromInteger n / (fromInteger d * f)
  (/) (Frac (n1,d1)) (Frac (n2,d2)) = Frac (numerator (n1 * d2 % (n2 * d1)), denominator (n1 * d2 % (n2 * d1)))

instance Floating Number' where
  pi = Creal pi
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

toFloating :: (CReal -> CReal) -> Number' -> Number'
toFloating f (Integer i ) = Creal $ f $ fromInteger i
toFloating f (Creal   c ) = Creal $ f c
toFloating f (Frac (n,d)) = Creal $ f $ fromInteger n / fromInteger d

instance Real Number' where
  toRational (Integer i) = toRational i
  toRational (Creal   f) = toRational f
  toRational (Frac (n,d)) = toRational $ Integer n / Integer d

instance Enum Number' where
  toEnum i = Integer (toInteger i)

  fromEnum (Integer i) = fromInteger i
  fromEnum (Creal   f) = round f
  fromEnum (Frac (n,d)) = fromEnum $ Integer n / Integer d

instance Integral Number' where
  quotRem (Integer i1) (Integer i2) = (Integer (quot i1 i2), Integer (rem i1 i2))

  quotRem (Integer i) (Creal c) = (Integer (floor (quot (fromInteger i) c)), Creal (rem (fromInteger i) c))

  quotRem (Integer i) (Frac f) = quotRem (Frac (i, 1)) (Frac f)

  quotRem (Frac (n1,d1)) (Frac (n2,d2)) = if Frac (n1,d1) > Frac (n2,d2) then (1 + quot (Frac (n1, d1) - Frac (n2,d2)) (Frac (n2,d2)), rem (Frac (n1, d1) - Frac (n2,d2)) (Frac (n2,d2))) else (Integer 0, Frac (n1,d1))

  quotRem (Frac f) (Integer i) = quotRem (Frac f) (Frac (i, 1))

  quotRem (Frac (n,d)) (Creal c) = quotRem (Creal (fromInteger n / fromInteger d)) (Creal c)

  quotRem (Creal c) (Integer i) = quotRem (Creal c) $ fromInteger i

  quotRem (Creal c) (Frac (n,d)) = quotRem (Creal c) (Creal (fromInteger n / fromInteger d))

  quotRem (Creal c1) (Creal c2) = (Integer (round (quot c1 c2)), Creal (rem c1 c2))

  toInteger (Integer i) = i
  toInteger (Creal   f) = round f
  toInteger (Frac (n,d)) = toInteger $ Integer n / Integer d

instance RealFrac Number' where
  properFraction (Integer i) = properFraction $ fromInteger i
  properFraction (Creal c) = (floor c, Creal $ c - floor c)
  properFraction (Frac (n,d)) = properFraction (fromRational (fromInteger n / fromInteger d))