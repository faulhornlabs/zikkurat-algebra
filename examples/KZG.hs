
-- | KZG commitment example.
--
-- This small program implements KZG commitment and opening proof \"by hand\"

module Main where

--------------------------------------------------------------------------------

import ZK.Algebra.Class.Curve
import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Poly
import ZK.Algebra.Class.Flat

import ZK.Algebra.Curves.BN128.Fp.Mont ( Fp )
import ZK.Algebra.Curves.BN128.Fr.Mont ( Fr )
import ZK.Algebra.Curves.BN128.Poly    ( Poly )

import qualified ZK.Algebra.Curves.BN128.Fr   as Fr
import qualified ZK.Algebra.Curves.BN128.Poly as Poly

import           ZK.Algebra.Curves.BN128.G1.Proj ( G1 , msm )
import qualified ZK.Algebra.Curves.BN128.G1.Affine as Affine

-- we use the `zikkurat-formats-binary` library to load a powers-of-tau ceremony file
import "zikkurat-formats-binary" ZK.Formats.Binary.Ptau
import "zikkurat-formats-binary" ZK.Formats.ForeignArray

--------------------------------------------------------------------------------

-- | Location of the @.ptau@ file
thePtauFile :: FilePath
thePtauFile = "../ptau/powersOfTau28_hez_final_08.ptau"

-- | Size of polynomial we want to commit
kzgSize :: Int
kzgSize = 256

--------------------------------------------------------------------------------

newtype G2 = G2 (ForeignPtr Word64)

instance Flat G2 where
  sizeInBytes  _ = 4*32
  sizeInQWords _ = 4*4
  withFlat (G2 fptr action) = withForeignPtr fptr action
  makeFlat src = do
    fptr <- mallocForeignPtrArray (4*4)
    withForeignPtr fptr $ \tgt -> copyBytes tgt src (4*32)
    return (G2 fptr)

kzgMain :: PowersOfTau -> IO ()
kzgMain ptau = do

  -- extract the required curve points from the ceremony
  -- TODO: make this nicer
  let G1Array farr1 = _tauG1 ptau
  let G2Array farr2 = _tauG2 ptau
  let tauG1 = MkFlatArray kzgSize (_foreignPtr farr1) :: FlatArray Affine.G1
  let tauG2 = MkFlatArray 2       (_foreignPtr farr2) :: FlatArray G2

  -- create a random polynomial of degree 255
  coeffs <- replicateM kzgSize rndIO :: IO [Fr]
  poly <- mkPoly coeffs :: Poly

  -- compute kzg commitment:
  let commitment = msm tauG1 

main :: IO ()
main = do
  ei <- parsePtauFile thePtauFile
  case ei of
    Left  err  -> putStrLn err
    Right ptau 
      | _fieldPrime        (_fieldConfig ptau) /= Fr.prime  -> putStrLn "error: the prime in the .ptau file is not the BN128 scalar field"
      | _logSizeOfPtauFile (_ceremonyCfg ptau) < 8          -> putStrLn "error: the ceremony size is too small (we need at least 256)"
      | otherwise                                           -> kzgMain ptau
