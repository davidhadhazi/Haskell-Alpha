module Lib.CReal where

import Data.Ratio

import Numeric(readFloat, readSigned)

newtype CReal = CR (Int -> Integer)

instance Eq  CReal where
  x == y = s' (digitsToBits digits) == 0 where (CR s') = x-y



instance Ord CReal where
  x <= y = s' (digitsToBits digits) <= 0 where (CR s') = x-y

  x <  y = s' (digitsToBits digits) <  0 where (CR s') = x-y

  x >= y = s' (digitsToBits digits) >= 0 where (CR s') = x-y

  x >  y = s' (digitsToBits digits) >  0 where (CR s') = x-y

  max (CR x') (CR y') = CR (\p -> max (x' p) (y' p))

  min (CR x') (CR y') = CR (\p -> min (x' p) (y' p))

instance Num CReal where
  (CR x') + (CR y') = CR (\p -> roundUk ((x' (p+2) + y' (p+2)) % 4))

  (CR x') * (CR y') = CR (\p -> roundUk ((x' (p+sy)*y' (p+sx)) % 2^(p+sx+sy)))
    where x0 = abs (x' 0)+2; y0 = abs (y' 0)+2
          sx = sizeinbase x0 2+3; sy = sizeinbase y0 2+3

  negate (CR x')    = CR (negate . x')

  abs x             = max x (negate x)

  signum (CR x')    = fromInteger (signum (x' (digitsToBits digits)))

  fromInteger n     = CR (\p -> n*2^p)



instance Fractional CReal where
  recip (CR x') = CR (\p -> let s = head [n | n <- [0..], 3 <= abs (x' n)]
    in roundUk (2^(2*p+2*s+2) % x' (p+2*s+2)))

  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

div2n :: CReal -> Int -> CReal
div2n (CR x') n = CR (\p -> if p >= n then x' (p-n) else roundUk (x' p % 2^n))

mul2n :: CReal -> Int -> CReal
mul2n (CR x') n = CR (\p -> x' (p+n))

instance Floating CReal where
  pi = 16 * atan (fromRational (1 % 5))
    - 4 * atan (fromRational (1 % 239))

  sqrt x  = CR (\p -> floorsqrt (x' (2*p))) where (CR x') = x

  log x
    | t < 0 = error "log of negative number\n"
    | t < 4 = - log (recip x)
    | t < 8 = logDr x
    | otherwise =

  {- 7 < t -} logDr (div2n x n) + fromIntegral n * log2
    where
      (CR x') = x
      t = x' 2
      n = sizeinbase t 2 - 3

  exp x
    | n < 0 = div2n (expDr s) (fromInteger (-n))
    | n > 0 = mul2n (expDr s) (fromInteger n)
    | otherwise = expDr s
    where
      (CR u') = x / log2
      n = u' 0
      s = x - fromInteger n * log2

  sin x
    | n == 0 = sinDr y
    | n == 1 = sqrt1By2 * (cosDr y + sinDr y)
    | n == 2 = cosDr y
    | n == 3 = sqrt1By2 * (cosDr y - sinDr y)
    | n == 4 = - sinDr y
    | n == 5 = - sqrt1By2 * (cosDr y + sinDr y)
    | n == 6 = - cosDr y
    | otherwise = - sqrt1By2 * (cosDr y - sinDr y)
    where
      (CR z') = x / piBy4
      s = roundUk (z' 2 % 4)
      n = s `mod` 8
      y = x - piBy4 * fromInteger s

  cos x
    | n == 0 = cosDr y
    | n == 1 = sqrt1By2 * (cosDr y - sinDr y)
    | n == 2 = - sinDr y
    | n == 3 = - sqrt1By2 * (cosDr y + sinDr y)
    | n == 4 = - cosDr y
    | n == 5 = - sqrt1By2 * (cosDr y - sinDr y)
    | n == 6 = sinDr y
    | otherwise =

  {- n == 7 -} sqrt1By2 * (cosDr y + sinDr y)
    where
      (CR z') = x / piBy4
      s = roundUk (z' 2 % 4)
      n = s `mod` 8
      y = x - piBy4 * fromInteger s

  atan x
    | t <  -5 = atanDr (negate (recip x)) - piBy2
    | t == -4 = -piBy4 - atanDr (xp1/xm1)
    | t <   4 = atanDr x
    | t ==  4 = piBy4 + atanDr (xm1/xp1)
    | otherwise =

  {- t >   4 -} piBy2 - atanDr (recip x)
    where
      (CR x') = x
      t = x' 2
      xp1 = x + 1
      xm1 = x - 1

  asin x
    | x0 >  0 = pi / 2 - atan (s/x)
    | x0 == 0 = atan (x/s)
    | otherwise =

  {- x0 <  0 -} - atan (s/x) - pi / 2
    where
      (CR x') = x
      x0 = x' 0
      s = sqrt (1 - x * x)

  acos x  = pi / 2 - asin x

  sinh x  = (y - recip y) / 2 where y = exp x

  cosh x  = (y + recip y) / 2 where y = exp x

  tanh x  = (y - y') / (y + y') where y = exp x; y' = recip y

  asinh x = log (x + sqrt (x*x + 1))

  acosh x = log (x + sqrt (x*x - 1))

  atanh x = log ((1 + x) / (1 - x)) / 2

accSeq :: (Rational -> Integer -> Rational) -> [Rational]
accSeq f = scanl f (1 % 1) [1..]


expDr :: CReal -> CReal
expDr = powerSeries (accSeq (\a n -> a*(1 % n))) id

logDr :: CReal -> CReal
logDr x = y * logDrx y where y = (x - 1) / x

logDrx :: CReal -> CReal
logDrx = powerSeries [1 % n | n <- [1..]] (+1)

sinDr :: CReal -> CReal
sinDr x = x*powerSeries (accSeq (\a n -> -a*(1 % (2*n*(2*n+1))))) id (x*x)

cosDr :: CReal -> CReal
cosDr x = powerSeries (accSeq (\a n -> -a*(1 % (2*n*(2*n-1))))) id (x*x)

atanDr :: CReal -> CReal
atanDr x = (x/y) * atanDrx ((x*x)/y) where y = x*x+1

atanDrx :: CReal -> CReal
atanDrx = powerSeries (accSeq (\a n -> a*((2*n) % (2*n+1)))) (+1)

powerSeries :: [Rational] -> (Int -> Int) -> CReal -> CReal
powerSeries ps terms (CR x')
  = CR (\p -> let t = terms p; l2t = 2*sizeinbase (toInteger t+1) 2+6; p' = p + l2t
                  xr = x' p'; xn = 2^p'; g yn = roundUk ((yn*xr) % (2^p'))
                  in roundUk (accumulate (iterate g xn) (take t ps) % (2^l2t)))
    where accumulate _      []     = 0
          accumulate []     _      = error "CReal.power_series.accumulate"
          accumulate (x:xs) (c:cs) = let t = roundUk (c*(x % 1)) in
                                     if t == 0 then 0 else t + accumulate xs cs

piBy2 :: CReal
piBy2 = div2n pi 1

piBy4 :: CReal
piBy4 = div2n pi 2

log2 :: CReal
log2 = div2n (logDrx (recip 2)) 1

sqrt1By2 :: CReal
sqrt1By2 = sqrt (recip 2)

instance Enum CReal where
  toEnum i         = fromIntegral i

  fromEnum _       = error "Cannot fromEnum CReal"

  enumFrom         = iterate (+ 1)

  enumFromTo n e   = takeWhile (<= e) $ iterate (+ 1)n

  enumFromThen n m = iterate (+(m-n)) n

  enumFromThenTo n m e = if m >= n then takeWhile (<= e) $ iterate (+(m-n)) n
    else takeWhile (>= e) $ iterate (+(m-n)) n

instance Real CReal where
  toRational _ = error "CReal.toRational"

instance RealFrac CReal where
  properFraction x@(CR x') = (fromInteger n, x - fromInteger n) where n = x' 0

instance RealFloat CReal where
  floatRadix _ = error "CReal.floatRadix"

  floatDigits _ = error "CReal.floatDigits"

  floatRange _ = error "CReal.floatRange"

  decodeFloat _ = error "CReal.decodeFloat"

  encodeFloat _ _ = error "CReal.encodeFloat"

  exponent _ = 0

  significand x = x

  isNaN _ = False

  isInfinite _ = False

  isDenormalized _ = False

  isNegativeZero _ = False

  isIEEE _ = False

showCReal :: Int                -- ^ The number of decimals
          -> CReal              -- ^ The real number
          -> String             -- ^ The resulting string

showCReal d (CR x')
  = (if s then "-" else "") ++ zs ++ (if d /= 0 then '.':fs' else "")
    where b  = digitsToBits d
          n  = x' b
          ds = show (roundUk ((n*10^d) % 2^b))
          (s,ds') = let sgn = head ds == '-' in (sgn, if sgn then tail ds else ds)
          ds'' = replicate (max (d+1-length ds') 0) '0' ++ ds'
          (zs,fs) = splitAt (length ds'' -d) ds''
          fs' = case reverse $ dropWhile (== '0') $ reverse fs of
                "" -> "0"
                xs -> xs



digitsToBits :: Int -> Int
digitsToBits d = ceiling (fromIntegral d * (logBase 2.0 10.0 :: Double)) + 4

digits :: Int
digits = 40

instance Read CReal where
  readsPrec _p = readSigned readFloat

instance Show CReal where
  showsPrec p x = let xs = showCReal digits x in
                  if head xs == '-' then showParen (p > 6) (showString xs)
                                    else showString xs

sizeinbase :: Integer -> Int -> Int
sizeinbase i b = f (abs i)
  where f n = if n <= 1 then 1 else 1 + f (n `div` toInteger b)

floorsqrt :: Integer -> Integer
floorsqrt x = until satisfy improve x
              where improve y = floor ((y*y+x) % (2*y))
                    satisfy y = y*y <= x && x <= (y+1)*(y+1)

roundUk :: Rational -> Integer
roundUk x = floor (x+1 % 2)

instance Integral CReal where
  quotRem a b = if a > b then (1 + quot (a-b) b, rem (a-b) b) else (0, a)
  toInteger = round
