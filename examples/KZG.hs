
{-# LANGUAGE 
      ScopedTypeVariables, TypeFamilies, DataKinds, KindSignatures,
      BangPatterns, StandaloneDeriving
  #-}
module KZG where

--------------------------------------------------------------------------------

import Data.Proxy
import Data.Kind

import Control.Monad

import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Curve 
import ZK.Algebra.Class.Pairing
import ZK.Algebra.Class.Poly
import ZK.Algebra.Class.Flat
import ZK.Algebra.Class.FFT
import ZK.Algebra.Class.Misc

import qualified ZK.Algebra.Curves.BN128.Fr.Mont   as BN128.Fr
import qualified ZK.Algebra.Curves.BN128.Fp.Mont   as BN128.Fp
import qualified ZK.Algebra.Curves.BN128.Fp2.Mont  as BN128.Fp2
import qualified ZK.Algebra.Curves.BN128.Fp6.Mont  as BN128.Fp6
import qualified ZK.Algebra.Curves.BN128.Fp12.Mont as BN128.Fp12
import qualified ZK.Algebra.Curves.BN128.G1.Affine as BN128.AffG1
import qualified ZK.Algebra.Curves.BN128.G2.Affine as BN128.AffG2
import qualified ZK.Algebra.Curves.BN128.G1.Proj   as BN128.ProjG1
import qualified ZK.Algebra.Curves.BN128.G2.Proj   as BN128.ProjG2
import qualified ZK.Algebra.Curves.BN128.Pairing   as BN128.Pairing
import qualified ZK.Algebra.Curves.BN128.Poly      as BN128.Poly

--------------------------------------------------------------------------------

data KZGSetup (c :: SomeCurve) = MkKZGSetup 
  { _kzgDomain    :: FFTDomain (Fr c)
  , _tauG1s       :: FlatArray (G1 c)
  , _lagrangeTaus :: FlatArray (G1 c)
  , _g2TauG2      :: (G2 c, G2 c)
  }

data VerifierKey (c :: SomeCurve) = VerifKey 
  { _g1   :: !(G1 c)
  , _g2   :: !(G2 c)
  , _tau2 :: !(G2 c)
  }

extractVerifierKey :: forall c. PairingCurve c => KZGSetup c -> VerifierKey c
extractVerifierKey setup = VerifKey g1 g2 tau2 where
  g1 = peekFlatArray (_tauG1s setup) 0
  (g2,tau2) = _g2TauG2 setup

--------------------------------------------------------------------------------

mkKZGSetup :: forall c. PairingCurve c => Proxy c -> Fr c -> Log2 -> KZGSetup c
mkKZGSetup pxy tau m = setup where
 
  n      = exp2_ m
  dom    = getFFTDomain m
  tauG1s = packFlatArrayFromList' n (taus tau n)

  g1 = curveSubgroupGen :: G1 c
  g2 = curveSubgroupGen :: G2 c
 
  setup = MkKZGSetup 
    { _kzgDomain    = dom
    , _tauG1s       = tauG1s
    , _lagrangeTaus = curveIFFT dom tauG1s
    , _g2TauG2      = (g2, tau <**> g2)
    } 

  taus :: Fr c -> Int -> [G1 c]
  taus !tau n = go n 1 where
    go 0 _    = []
    go k !acc = acc <**> g1 : go (k-1) (tau*acc)

newKZGSetup :: forall c. PairingCurve c => Proxy c -> Log2 -> IO (KZGSetup c)
newKZGSetup pxy m = do
  tau <- rndIO 
  return $ mkKZGSetup pxy tau m

--------------------------------------------------------------------------------

newtype Commitment c 
  = Com (G1 c)
  
deriving instance PairingCurve c => Eq   (Commitment c)
deriving instance PairingCurve c => Show (Commitment c)

commitPoly :: forall c. PairingCurve c => Proxy c -> KZGSetup c -> Poly c -> Commitment c
commitPoly pxy setup poly = (Com com) where
  coeffs = unwrapArray poly
  tauG1s = takeFlatArray (flatArrayLength coeffs) (_tauG1s setup)
  com    = msm coeffs tauG1s

commitValues :: forall c. PairingCurve c => Proxy c -> KZGSetup c -> FlatArray (Fr c) -> Commitment c
commitValues pxy setup values
  | fftDomainSize dom == flatArrayLength values = (Com com)
  | otherwise = error "commitValues: expecting a vector of the same size as the KZG setup domain" 
  where
    com = msm values (_lagrangeTaus setup)
    dom = _kzgDomain setup
  
commitInterpolate :: forall c. PairingCurve c => Proxy c -> KZGSetup c -> FlatArray (Fr c) -> Commitment c
commitInterpolate pxy setup values 
  | fftDomainSize dom == flatArrayLength values = com
  | otherwise = error "commitInterpolate: expecting a vector of the same size as the KZG setup domain" 
  where
    com = commitPoly pxy setup (intt dom values)
    dom = _kzgDomain setup

--------------------------------------------------------------------------------

data Opening c = Opening
  { _location :: !(Fr c) 
  , _value    :: !(Fr c)
  , _proof    :: !(G1 c)
  }

deriving instance PairingCurve c => Eq   (Opening c)
deriving instance PairingCurve c => Show (Opening c)

openingProof :: forall c. PairingCurve c => Proxy c -> KZGSetup c -> Poly c -> Fr c -> Opening c
openingProof pxy setup poly loc = Opening loc val prf where 
  val = evalAt loc poly
  mb  = quotByVanishing (poly - constPoly val) (1,loc)   -- divide (p-val) by (x-loc)
  prf = case mb of
    Just q  -> let Com prf = commitPoly pxy setup q in prf
    Nothing -> error "openingProof: fatal error, should not happen" 

--------------------------------------------------------------------------------

verifyProof :: forall c. PairingCurve c => Proxy c -> VerifierKey c -> Commitment c -> Opening c -> Bool
verifyProof pxy (VerifKey g1 g2 tau2) (Com comP) (Opening x0 y0 comQ) = (lhs == rhs) where
  lhs = pairing pxy comQ  tau2
  rhs = pairing pxy point g2 
  point = comP <+> (scalarMul x0 comQ <-> scalarMul y0 g1)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let pxy = Proxy @BN128
  let m = Log2 4
  let n = exp2_ m
  putStrLn $ "size of the domain = " ++ show n
  setup <- newKZGSetup pxy m
  let vkey = extractVerifierKey setup
  let dom = _kzgDomain setup
  ys <- replicateM n rndIO :: IO [BN128.Fr.Fr]
  let values = packFlatArrayFromList' n ys
  let poly = intt dom values
  let com1 = commitValues pxy setup values
  let com2 = commitPoly   pxy setup poly
  putStrLn $ "the two commitments match: " ++ show (com1==com2)
  loc <- rndIO :: IO BN128.Fr.Fr
  let prf = openingProof pxy setup poly loc
  -- print prf
  putStrLn $ "x0 = " ++ show loc
  putStrLn $ "y0 = " ++ show (_value prf)
  let ok = verifyProof pxy vkey com1 prf
  putStrLn $ "verificiation succeeded: " ++ show ok

--------------------------------------------------------------------------------
