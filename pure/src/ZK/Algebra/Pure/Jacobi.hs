
-- | Computing the Jacobi symbol (generalized Legendre symbol)
--
-- See <https://en.wikipedia.org/wiki/Jacobi_symbol>

module ZK.Algebra.Pure.Jacobi where

--------------------------------------------------------------------------------

import Data.Bits

import ZK.Algebra.Pure.Field.Class ( QR(..) )
import ZK.Algebra.Pure.Misc

--------------------------------------------------------------------------------

fromJacobiSymbol :: Integral a => QR -> a
fromJacobiSymbol j = case j of
  Residue    ->  1
  NonResidue -> -1
  Zero       ->  0

flipJacobiSymbol :: QR -> QR
flipJacobiSymbol j = case j of
  Residue    -> NonResidue
  NonResidue -> Residue
  Zero       -> Zero

--------------------------------------------------------------------------------

jacobiSymbol_ :: Integer -> Integer -> Integer
jacobiSymbol_ n k = fromJacobiSymbol (jacobiSymbol n k)

-- | Computes the Jacobi symbol.
--
-- See <https://en.wikipedia.org/wiki/Jacobi_symbol>
--
jacobiSymbol :: Integer -> Integer -> QR
jacobiSymbol n k 
  | k <= 0     = error "in the Jacobi symbol (n|k), k should be positive"
  | isEven k   = error "in the Jacobi symbol (n|k), k should be odd"
  | otherwise  = go (mod n k) k Residue
  where

    go :: Integer -> Integer -> QR -> QR
    go 0 k t = if k == 1 then t else Zero
    go n k t = if even n

      then let r  = k .&. 7
               t' = if r==3 || r==5 then flipJacobiSymbol t else t
           in  go (div2 n) k t'

      else let n4 = n .&. 3
               k4 = k .&. 3
               t' = if n4 == 3 && k4 ==3 then flipJacobiSymbol t else t
           in  go (mod k n) n t'

--------------------------------------------------------------------------------

{-

-- testing

Sum[ (3 + 5*n + 17*k) * (3 + JacobiSymbol[n, k]), {n, -100, 100}, {k, 1, 100, 2} ]
Sum[ (3 + 5*n + 17*k) * (3 + JacobiSymbol[n, k]), {n, -100, 100}, {k, 1, 200, 2} ]
Sum[ (3 + 5*n + 17*k) * (3 + JacobiSymbol[n, k]), {n, -200, 200}, {k, 1, 100, 2} ]
Sum[ (3 + 5*n + 17*k) * (3 + JacobiSymbol[n, k]), {n, -200, 200}, {k, 1, 200, 2} ]
  
26191168
103937406
52208224
207763144

print $ sum [ (3 + 5*n + 17*k) * (3 + jacobiSymbol_ n k) | n<-[-100..100], k<-[1,3..100] ]
print $ sum [ (3 + 5*n + 17*k) * (3 + jacobiSymbol_ n k) | n<-[-100..100], k<-[1,3..200] ]
print $ sum [ (3 + 5*n + 17*k) * (3 + jacobiSymbol_ n k) | n<-[-200..200], k<-[1,3..100] ]
print $ sum [ (3 + 5*n + 17*k) * (3 + jacobiSymbol_ n k) | n<-[-200..200], k<-[1,3..200] ]

-}

--------------------------------------------------------------------------------

{-

-- Lua original on wikipedia:
-- <https://en.wikipedia.org/wiki/Jacobi_symbol#Implementation_in_Lua>

function jacobi(n, k)
  assert(k > 0 and k % 2 == 1)
  n = n % k
  t = 1
  while n ~= 0 do
    while n % 2 == 0 do
      n = n / 2
      r = k % 8
      if r == 3 or r == 5 then
        t = -t
      end
    end
    n, k = k, n
    if n % 4 == 3 and k % 4 == 3 then
      t = -t
    end
    n = n % k
  end
  if k == 1 then
    return t
  else
    return 0
  end
end

-}

--------------------------------------------------------------------------------


