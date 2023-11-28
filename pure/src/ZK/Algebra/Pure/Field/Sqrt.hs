
-- | Square roots in finite fields

{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleContexts #-}
module ZK.Algebra.Pure.Field.Sqrt where

--------------------------------------------------------------------------------

import Data.Bits
import Data.Maybe
import Data.Proxy

import System.Random
import System.IO.Unsafe

import ZK.Algebra.Pure.Field.Class
import ZK.Algebra.Pure.Jacobi

import ZK.Algebra.Pure.Misc

--------------------------------------------------------------------------------
-- based on: 
-- 
-- Adj, Rodriguez-Henriquez: Square root computation over even extension fields
--
-- <https://eips.ethereum.org/assets/eip-3068/2012-685_Square_Root_Even_Ext.pdf>
--

isQuadraticResidue :: forall f. Field f => f -> QR
isQuadraticResidue a = 
  case dim of
    1 -> jacobiSymbol (unsafeProjectToPrimeSubfield a) p
    m -> jacobiSymbol (unsafeProjectToPrimeSubfield b) p where b = seqC_naive m a 1

  where
    dim = dimension      (Proxy @f)
    p   = characteristic (Proxy @f)
    q   = fieldSize      (Proxy @f)

    -- | computes @a^(1+s+s^2+....+s^k)@ where @s = p^u@, with @1 <= u < m@
    seqC_naive :: forall f. Field f => Int -> f -> Int -> f
    seqC_naive k a u = pow a $ sum [ s^i | i<-[0..k-1] ] where
      p = characteristic (Proxy @f)
      s = p ^ u

{-
    -- | This is from the above article. TODO: use fast Frobenius instead
    -- of naive powers... that's the point
    --
    -- Computes @a^(1+s+s^2+....+s^{k-1})@ where @s = p^u@, with @1 <= u < m@
    --
    seqC_article :: forall f. Field f => Int -> f -> Int -> f
    seqC_article 0 a _ = 1
    seqC_article 1 a _ = a
    seqC_article k a u = worker (k-1) a where
      p = characteristic (Proxy @f)
      s = p ^ u

      worker 0 a = a
      worker k a =
        case (k.&.1) of
          1 -> let c = worker  n    a in          pow c (1 + s^(n+1)) 
          _ -> let c = worker (n-1) a in a * pow (pow c (1 + s^ n   )) s
        where
          n = shiftR k 1
-}

{-# NOINLINE randomNonresidue #-}
randomNonresidue :: (Random f, Field f) => Proxy f -> f
randomNonresidue pxy = unsafePerformIO (randomNonresidueIO pxy)

{-# NOINLINE randomNonresidueIO #-}
randomNonresidueIO :: (Random f, Field f) => Proxy f -> IO f
randomNonresidueIO pxy = worker where
  worker = do
    x <- randomIO 
    case isQuadraticResidue x of
      NonResidue -> return x
      otherwise  -> worker 

--------------------------------------------------------------------------------
-- * generic finite fields

-- | Square root in a generic finite field
--
-- Based on <https://eips.ethereum.org/assets/eip-3068/2012-685_Square_Root_Even_Ext.pdf>
--
genericFieldSqrt :: forall f. (Random f, Field f) => f -> Maybe f
genericFieldSqrt x 
  | isZero x    = Just 0       -- 0 apparently causes an infinite loop...
  | otherwise   = mbsqrt 
  where

    q = fieldSize (Proxy @f)
    mbsqrt = case dimension (Proxy @f) of

      -- prime field
      1  -> primeTonelliAlgorithm (powerOfTwoDecomp (q-1)) x

      -- prime power field
      m  -> case m .&. 1 > 0 of

        -- m is odd
        True  -> case mod4 q of
          3 -> shanksAlgorithm x
          1 -> case mod8 q of
            5 -> atkinAlgorithm x
            1 -> case mod16 q of
              9 -> kongAlgorithm x
              1 -> genericTonelliShanksAlgorithm (powerOfTwoDecomp (q-1)) x

        -- m is even
        False -> genericTonelliShanksAlgorithm (powerOfTwoDecomp (q-1)) x

        {-
        -- m is even
        False -> case q .&. 3 of
          3 -> error "algorithm_9"
          1 -> error "algorithm_10"
        -}

--------------------------------------------------------------------------------
-- * prime fields

-- | Tonelli–Shanks algorithm for square root modulo p (a prime)
--
-- The @s@ and @q@ parameters must be such that @p - 1 = 2^s * q@ with @q@ being odd
--
primeTonelliAlgorithm :: forall f. Field f => PowerOfTwoDecomp -> f -> Maybe f
primeTonelliAlgorithm (POTD s q) x 
  | isEven q        = error "primeTonelliAlgorithm: q should be odd"
  | p-1 /= q * 2^s  = error "primeTonelliAlgorithm: we expect 2^s * q = p - 1"
  | otherwise       = mb_root
  where
 
    -- we don't want to use the `asInteger` method from PrimeField, 
    -- because we want to call this function from `genericFieldSqrt` above...
    -- hence, so we cannot expect a PrimeField instance
    Just x_int = projectToPrimeSubfield x

    mb_root = case jacobiSymbol x_int p of
      Zero       -> Just 0
      NonResidue -> Nothing
      Residue    -> go m0 c0 t0 r0
     
    p = fieldSize (Proxy @f)
    z = fromInteger (randomNonresidueModPrime p) :: f

    m0 = s
    c0 = pow z q
    t0 = pow x q
    r0 = pow x (div2 (q+1))

    -- finds smallest i such that t^(2^i) == 1
    find_i :: f -> Int
    find_i t = worker 0 t where
      worker i t = if t == 1 then i else worker (i+1) (t*t)

    go :: Int -> f -> f -> f -> Maybe f
    go m c 0 r = Just 0 
    go m c 1 r = Just r
    go m c t r = let i  = find_i t
                     b  = pow c (2^(m-i-1))
                     b2 = b*b
                 in  if i >= m
                       then Nothing
                       else go i b2 (t*b2) (r*b)

-- | given a prime p, we return a quadratic nonresidue 
-- (half of all possible values is a nonresidue, so we just sample randomly
-- until we find one)
{-# NOINLINE randomNonresidueModPrime #-}
randomNonresidueModPrime :: Integer -> Integer   
randomNonresidueModPrime p = unsafePerformIO (randomNonresidueModPrimeIO p)

{-# NOINLINE randomNonresidueModPrimeIO #-}
randomNonresidueModPrimeIO :: Integer -> IO Integer
randomNonresidueModPrimeIO p = loop where
  loop = do
    k <- randomRIO (1,p-1)
    if jacobiSymbol k p == NonResidue
      then return k
      else loop

--------------------------------------------------------------------------------
-- * prime power fields, odd dimension

-- | Shanks' algorithm for square root over @Fq@ when @q = 3 (mod 4)@ and $q = p^m$ for @m@ odd
shanksAlgorithm :: forall f. Field f => f -> Maybe f
shanksAlgorithm a = 
  if a0 == -1 
    then Nothing 
    else Just a1a  
  where
    q   = fieldSize (Proxy @f)
    a1  = pow a (div4 q)      -- (q-3)/4
    a1a = a  * a1
    a0  = a1 * a1a

-- | Atkin's algorithm for square root over @Fq@ when @q = 5 (mod 8)@ and $q = p^m$ for @m@ odd
atkinAlgorithm :: forall f. Field f => f -> Maybe f
atkinAlgorithm a = 
  if a0 == -1 
    then Nothing 
    else Just x 
  where
    q     = fieldSize (Proxy @f)
    qper8 = div8 q             -- (q-5)/8
    t  = pow 2 qper8
    a1 = pow a qper8
    a1a = a1*a1*a
    a0  = a1a*a1a
    b   = t*a1
    ab  = a*b
    i   = 2*ab*b
    x   = ab*(i-1)

-- | Kong's algorithm for square root over @Fq@ when @q = 9 (mod 16)@ and $q = p^m$ for @m@ odd
kongAlgorithm :: forall f. (Random f, Field f) => f -> Maybe f
kongAlgorithm a = 
  if a0 == -1 
    then Nothing 
    else Just x 
  where
    sqr  y = y*y 
    pow4 y = sqr (sqr y)
    q      = fieldSize (Proxy @f)
    qper8  = div8  (q-9)             -- (q-9)/8
    qper16 = div16  q                -- (q-9)/16
    c  = randomNonresidue (Proxy @f)
    d  = pow c qper8
    e  = c * c
    t  = pow 2 qper16
    a1 = pow a qper16
    a1a = a1*a1*a
    a0 = pow4 a1a 
    b  = t*a1
    ab = a*b
    i  = 2*ab*b
    r  = i*i
    x = if r == -1
      then ab*(i-1)
      else let u = b*d
               i = 2*u*u*e*a
           in  u*c*a*(i-1)

----------------------------------------

-- | prime power Tonelli-Shanks Algorithm
--
-- input: the power-of-two-times-odd decomposition of (q-1)
genericTonelliShanksAlgorithm :: forall f. (Random f, Field f) => PowerOfTwoDecomp -> f -> Maybe f
genericTonelliShanksAlgorithm (POTD s t) a =
  if a0 == -1 
    then Nothing 
    else Just $ go w0 z0 s (a*w0) (a*w0*w0)

  where
    two_pow_sm1 = 2 ^ (s-1)
    (c,z0) = tsPrecomp t two_pow_sm1

    w0 = pow a (div2 $ t-1)
    a0 = pow (w0*w0*a) two_pow_sm1

    go w z v x 1 = x
    go w z v x b = 
      let k  = find_k b 
          w' = pow z (2 ^ (v - k - 1))
          z' = w' * w'
          b' = b  * z'
          x' = x  * w'
          v' = k
      in  go w' z' v' x' b'

    -- finds smallest k such that t^(2^k) == 1
    find_k :: f -> Int
    find_k t = worker 0 t where
      worker i t = if t == 1 then i else worker (i+1) (t*t)

{-# NOINLINE tsPrecomp #-}
tsPrecomp :: forall f. (Random f, Field f) => Integer -> Integer -> (f,f)
tsPrecomp t two_pow_sm1 = unsafePerformIO (tsPrecompIO t two_pow_sm1)

{-# NOINLINE tsPrecompIO #-}
tsPrecompIO :: forall f. (Random f, Field f) => Integer -> Integer -> IO (f,f)
tsPrecompIO t two_pow_sm1 = do
  c <- randomIO :: IO f
  if c == 0
    then tsPrecompIO t two_pow_sm1
    else do
      let z  = pow c t                    -- c ^ t
          c0 = pow z two_pow_sm1          -- z ^ (2^(s-1)) = c ^ (t*2^(s-1)) = c ^ ((q-1)/2)
      if  c0 == 1 
        then tsPrecompIO t two_pow_sm1    -- c is residue, try again
        else return (c,z)                 -- c is nonresidue

----------------------------------------

{-
complexMethod :: forall f. (Random f, Field f, QuadraticExt f) => f -> Maybe f
complexMethod a 
  | isOdd n2    = error "complexMethod: expecting even dimension over the prime subfield" 
  | otherwise   = 
  where

    sqrtq :: ExtBase f -> Maybe (ExtBase f)
    sqrtq = genericFieldSqrt @(ExtBase f)

    beta = randomNonresidue (Proxy @f)
    q2 = fieldSize (Proxy @f)
    n2 = dimension (Proxy @f)
    n  = div n2 2
    p  = characteristic (Proxy @f)
    q  = p ^ n

    ...
-}

----------------------------------------

-- | "Algorithm 9" for square root computation over @F_{q^2}@ , with @q == 3 (mod 4)@
--
-- TODO: THIS IS UNTESTED!
algorithm_9 :: forall f. (Random f, Field f) => SqrtMinus1 f -> f -> Maybe f
algorithm_9 (SqrtMinus1 i) a 
  | (n2 .&. 1) /= 0    = error "algorithm_9: expecting even dimension over the prime subfield" 
  | a0 == -1           = Nothing
  | otherwise          = Just x
  where
    q2 = fieldSize (Proxy @f)
    n2 = dimension (Proxy @f)
    n  = div n2 2
    p  = characteristic (Proxy @f)
    q  = p ^ n

    a1 = pow a (div4 q)       -- (q-3)/4
    a1a = a1*a
    alpha = a1 * a1a
    a0 = pow alpha (q+1)   -- (pow alpha q) * alpha
    x0 = a1a
    x = if alpha == -1 
      then i * x0
      else pow (1+alpha) (div2 q) * x0

----------------------------------------

-- | "Algorithm 10" for square root computation over @F_{q^2}@ , with @q == 1 (mod 4)@
-- 
-- TODO: THIS IS UNTESTED!
algorithm_10 :: forall f. (Random f, Random (ExtBase f), Field f, QuadraticExt f) => f -> Maybe f
algorithm_10 a 
  | (n2 .&. 1) /= 0   = error "algorithm_10: expecting even dimension over the prime subfield" 
  | a0 == -1          = Nothing
  | otherwise         = x
  where

    sqrtq :: ExtBase f -> Maybe (ExtBase f)
    sqrtq = genericFieldSqrt @(ExtBase f)

    q2 = fieldSize (Proxy @f)
    n2 = dimension (Proxy @f)
    n  = div n2 2
    p  = characteristic (Proxy @f)
    q  = p ^ n

    -- these should be precomputed one for the field
    c  = randomNonresidue (Proxy @f)
    d  = pow c (div2 q)
    dc = d*c
    e = inv dc
    f = dc * dc

    b  = pow a (div4 q)    -- (q-1)/4
    b2 = b*b
    a0 = pow b2 (q+1)      -- pow b2 q * b2

    bq  = pow b q
    bqb = bq * b

    x = if bqb == 1
      then (\s -> bq *     embedExtBase s) <$> sqrtq (fromJust $ projectToExtBase $ b2*a  )
      else (\s -> bq * e * embedExtBase s) <$> sqrtq (fromJust $ projectToExtBase $ b2*a*f)

--------------------------------------------------------------------------------

testRandomSqrtN_ :: forall f. (Show f, Random f, Field f) => Proxy f -> Int -> IO ()
testRandomSqrtN_ pxy n = do
  mb <- testRandomSqrtN pxy n
  case mb of 
    Nothing -> putStrLn $ " ...OK"
    Just x  -> putStrLn $ " ...BAD! counterexample = " ++ show x


-- | runs N random tests, returns a counterexample if found
testRandomSqrtN :: forall f. (Random f, Field f) => Proxy f -> Int -> IO (Maybe f)
testRandomSqrtN _ n = (putStr header >> go n) where

  p   = characteristic (Proxy @f)
  dim = dimension      (Proxy @f)

  header = 
    "testing " ++ show n ++ " square roots in Fq with q = " ++ 
    show p ++ "^" ++ show dim ++ 
    " | p == " ++ show (mod p 16) ++ " (mod 16)"

  go 0 = return Nothing
  go n = do
    a <- randomIO :: IO f
    let mb = genericFieldSqrt a
    case mb of
      Nothing -> if isQuadraticResidue a == NonResidue 
        then go (n-1)
        else return $ Just a
      Just x -> if x*x == a 
        then go (n-1)
        else return $ Just a

--------------------------------------------------------------------------------

