
-- | Simple MSM example

module Main where

--------------------------------------------------------------------------------

import ZK.Algebra.Class.Curve
import ZK.Algebra.Class.Field
import ZK.Algebra.Class.Flat

import ZK.Algebra.Curves.BLS12_381.Fp.Mont ( Fp )
import ZK.Algebra.Curves.BLS12_381.Fr.Mont ( Fr , toStd , fromStd )

import qualified ZK.Algebra.Curves.BLS12_381.Fr.Std as Std

import           ZK.Algebra.Curves.BLS12_381.G1.Proj ( genG1 , G1 , msm , msmStd )
import qualified ZK.Algebra.Curves.BLS12_381.G1.Affine as Affine

--------------------------------------------------------------------------------

somePointP, somePointQ :: G1
somePointP = grpScale 12345 genG1
somePointQ = grpScale 67890 genG1

someNumberA, someNumberB :: Fr
someNumberA = 1 / (123456789 :: Fr)
someNumberB = 1 / (765432105 :: Fr)

-- an infinite sequence of some random-looking points
points :: [Affine.G1]
points = go 1 somePointP where
  go i pt = toAffine pt : go (i+1) (grpScale (13 + 137*i) pt `grpAdd` somePointQ)

-- an infinite sequence of some random-looking coefficients, in Montgomery representation
coeffs :: [Fr]
coeffs = 3 : go 1 someNumberA where
  go i x = x : go (i+1) ( x * (666 + 197241*i) + someNumberB ) 

-- the same coefficients in standard representation
coeffsStd :: [Std.Fr]
coeffsStd = map toStd coeffs

--------------------------------------------------------------------------------

{-
sanityCheck1a n = unpackFlatArrayToList (packFlatArrayFromList' n coeffs   ) == take n coeffs
sanityCheck1b n = unpackFlatArrayToList (packFlatArrayFromList' n coeffsStd) == take n coeffsStd
sanityCheck2  n = unpackFlatArrayToList (packFlatArrayFromList' n points   ) == take n points
-}

--------------------------------------------------------------------------------

standardMSM :: Int -> G1
standardMSM n = msmStd
  (packFlatArrayFromList' n coeffsStd)
  (packFlatArrayFromList' n points) 

montgomeryMSM :: Int -> G1
montgomeryMSM n = msm
  (packFlatArrayFromList' n coeffs)
  (packFlatArrayFromList' n points) 
  
referenceMSM :: Int -> G1
referenceMSM n = grpSum $ take n $ zipWith scalarMul coeffs (map fromAffine points)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let n = 123
  let ex1 = toAffine (standardMSM   n)
  let ex2 = toAffine (montgomeryMSM n)
  let ref = toAffine (referenceMSM  n)
  putStrLn $ "result of (standard coeff)   MSM of size " ++ show n ++ " = " ++ show ex1 ++ "\n"
  putStrLn $ "result of (Montgomery coeff) MSM of size " ++ show n ++ " = " ++ show ex2 ++ "\n"
  putStrLn $ "result of reference          MSM of size " ++ show n ++ " = " ++ show ref ++ "\n" 
  if (ex1 == ref) && (ex1 == ex2)
    then putStrLn "they match! OK."
    else putStrLn "they do not match! FAIL!"

--------------------------------------------------------------------------------
