
-- | Inefficient reference implementation of optimal Ate pairing for the bn128 (or BN254) curve

module ZK.Algebra.Reference.Pairing.BN128 where

--------------------------------------------------------------------------------

import Data.Bits
import Data.List

import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Curve
import ZK.Algebra.Class.Misc
import ZK.Algebra.Helpers

import ZK.Algebra.Curves.BN128.Fp.Mont   ( Fp   )
import ZK.Algebra.Curves.BN128.Fp2.Mont  ( Fp2  )
import ZK.Algebra.Curves.BN128.Fp6.Mont  ( Fp6  )
import ZK.Algebra.Curves.BN128.Fp12.Mont ( Fp12 )

import ZK.Algebra.Curves.BN128.G1.Affine ( G1 )
import ZK.Algebra.Curves.BN128.G2.Affine ( G2 )

import qualified ZK.Algebra.Curves.BN128.Fr.Mont   as Fr
import qualified ZK.Algebra.Curves.BN128.Fp.Mont   as Fp
import qualified ZK.Algebra.Curves.BN128.Fp2.Mont  as Fp2
import qualified ZK.Algebra.Curves.BN128.Fp6.Mont  as Fp6
import qualified ZK.Algebra.Curves.BN128.Fp12.Mont as Fp12

import qualified ZK.Algebra.Curves.BN128.G1.Affine as AffG1
import qualified ZK.Algebra.Curves.BN128.G2.Affine as AffG2
import qualified ZK.Algebra.Curves.BN128.G1.Proj   as ProjG1
import qualified ZK.Algebra.Curves.BN128.G2.Proj   as ProjG2

--------------------------------------------------------------------------------

type ProjG1 = ProjG1.G1
type ProjG2 = ProjG2.G2

--------------------------------------------------------------------------------

w_fp12  = quadraticPack (0,1)  :: Fp12
w2_fp12 = w_fp12 ^ 2           :: Fp12
w3_fp12 = w_fp12 ^ 3           :: Fp12
w2_inv_fp12 = inverse w2_fp12  :: Fp12
w3_inv_fp12 = inverse w3_fp12  :: Fp12

--------------------------------------------------------------------------------

fpToFp2 :: Fp -> Fp2
fpToFp2 x = quadraticPack (x,0)

fpToFp12 :: Fp -> Fp12
fpToFp12 x = fp2ToFp12 (quadraticPack (x,0))

fp2ToFp12 :: Fp2 -> Fp12
fp2ToFp12 x = quadraticPack (cubicPack (x,0,0) , 0)

projectFp2 :: Fp12 -> Maybe Fp2
projectFp2 x = case quadraticUnpack x of 
  (x1,0) -> case cubicUnpack x1 of
    (x2,0,0) -> Just x2 
    _        -> Nothing
  _        -> Nothing

--------------------------------------------------------------------------------

frobeniusG2 :: G2 -> G2
frobeniusG2 = inversePsi . frobeniusE12 . psi

type E12 = (Fp12,Fp12)

frobeniusE12 :: E12 -> E12
frobeniusE12 (x,y) = (frobenius x, frobenius y)

checkE12 :: E12 -> Bool
checkE12 (x,y) = (0 == -y*y + x*x*x + 3)

--------------------------------------------------------------------------------

-- | The mapping @psi : E'(F_p^2) -> E(F_p^12)@
--
-- defined by @psi(x,y) := (x*w^2, y*w^3)@
--
-- D-type twist (D as Division)
--
psi' :: (Fp2,Fp2) -> (Fp12,Fp12)
psi' (x,y) = (fp2ToFp12 x * w2_fp12 , fp2ToFp12 y * w3_fp12) where

inversePsi' :: (Fp12,Fp12) -> (Fp2,Fp2)
inversePsi' (x,y) = case ( projectFp2 (x * w2_inv_fp12) , projectFp2 (y * w3_inv_fp12) ) of
  (Just x', Just y') -> (x',y')
  _ -> error "inversePsi'"

psi :: G2 -> E12
psi = psi' . AffG2.coords

inversePsi :: E12 -> G2
inversePsi = mkPoint2 . inversePsi'

--------------------------------------------------------------------------------

line :: (ProjG2, ProjG2) -> G1 -> Fp12
line (t,q) p = affLineE12 (psi (toAffine t), psi (toAffine q)) p

affLineE12 :: (E12,E12) -> G1 -> Fp12
affLineE12 (t,q) p = if t == q then affTangentE12 t p else affSecantE12 t q p where

affSecantE12 :: E12 -> E12 -> G1 -> Fp12
affSecantE12 t q p = eval where
  (xp_,yp_) = coords2 p :: (Fp,Fp)
  (xp ,yp ) = (fpToFp12 xp_, fpToFp12 yp_) :: (Fp12,Fp12)
  (x1 ,y1 ) = t :: (Fp12,Fp12)
  (x2 ,y2 ) = q :: (Fp12,Fp12)
  m = (y2 - y1) / (x2 - x1) :: Fp12
  eval = m * (xp - x1) - (yp - y1)

affTangentE12 :: E12 -> G1 -> Fp12
affTangentE12 t p = eval where
  (xp_,yp_) = coords2 p :: (Fp,Fp)
  (xp ,yp ) = (fpToFp12 xp_, fpToFp12 yp_) :: (Fp12,Fp12)
  (x1, y1 ) = t :: (Fp12,Fp12)
  m  = triple (x1*x1) / double y1 :: Fp12
  eval = m * (xp - x1) - (yp - y1)

--------------------------------------------------------------------------------

double :: Fp12 -> Fp12   -- Field f => f -> f
double x = x+x

triple :: Fp12 -> Fp12   -- Field f => f -> f
triple x = x+x+x

--------------------------------------------------------------------------------

millerLoop :: ProjG2 -> Integer -> G1 -> (ProjG2,Fp12)
millerLoop q u p = foldl' go (q,1) (reverse [0..n-1]) where
  n = fromLog2 (integerLog2 u)

  -- remark: point addition and line function evaluation could be merged
  go :: (ProjG2,Fp12) -> Int -> (ProjG2,Fp12)
  go (!t,!acc) i = 
    case testBit u i of
      False -> (       t2   , acc2 * line (t,t) p                )
      True  -> (grpAdd t2 q , acc2 * line (t,t) p * line (t2,q) p)
    where
      t2   = grpDbl t   :: ProjG2
      acc2 = square acc :: Fp12

--------------------------------------------------------------------------------

finalExpoNaive :: Fp12 -> Fp12
finalExpoNaive x = power x fullExpo

finalExpo :: Fp12 -> Fp12
finalExpo x = power (powEasy2 $ powEasy1 x) hardExpo where

  powEasy1 x = frob6 x / x      -- x^(p^6-1)
  powEasy2 x = frob2 x * x      -- x^(p^2+1)

  frob2, frob6 :: Fp12 -> Fp12
  frob2 = frobenius . frobenius
  frob6 = frob2 . frob2 . frob2

fullExpo :: Integer
fullExpo = div (Fp.prime^12 - 1) Fr.prime

hardExpo :: Integer
hardExpo = div (Fp.prime^4 - Fp.prime^2 + 1) Fr.prime

--------------------------------------------------------------------------------

pairing :: G1 -> G2 -> Fp12
pairing p q 
  | AffG1.isInfinity p  = 1
  | AffG2.isInfinity q  = 1
  | otherwise           = finalExpo finalBase
  where
    x  = 4965661367192848881
    
    (yQ, millerOut) = millerLoop (fromAffine q) (6*x+2) p 

    finalBase = millerOut
              * line (       yQ       ,        phi1Q) p 
              * line (grpAdd yQ phi1Q , grpNeg phi2Q) p

    phi1 = frobeniusG2 q     :: G2  -- pi(Q)
    phi2 = frobeniusG2 phi1  :: G2  -- pi^2(Q)

    phi1Q = fromAffine phi1 :: ProjG2
    phi2Q = fromAffine phi2 :: ProjG2

    expo = div (Fp.prime^12 - 1) Fr.prime

--------------------------------------------------------------------------------
