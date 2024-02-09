
-- | Simple MSM example

module Main where

--------------------------------------------------------------------------------

import ZK.Algebra.API
import ZK.Algebra.Curves.BLS12_381

import qualified ZK.Algebra.Curves.BLS12_381.G1.Affine as Affine

--------------------------------------------------------------------------------

genG1 :: G1Proj
genG1 = curveSubgroupGen

somePointP, somePointQ :: G1Proj
somePointP = grpScale 12345 genG1
somePointQ = grpScale 67890 genG1

someNumberA, someNumberB :: Fr
someNumberA = 1 / (123456789 :: Fr)
someNumberB = 1 / (765432105 :: Fr)

-- an infinite sequence of some random-looking points
myPoints :: [G1Affine]
myPoints = go 1 somePointP where
  go i pt = toAffine pt : go (i+1) (grpScale (13 + 137*i) pt `grpAdd` somePointQ)

-- an infinite sequence of some random-looking coefficients, in Montgomery representation
myCoeffs :: [Fr]
myCoeffs = 3 : go 1 someNumberA where
  go i x = x : go (i+1) ( x * (666 + 197241*i) + someNumberB ) 

-- the same coefficients in standard representation
myCoeffsStd :: [StdFr]
myCoeffsStd = map toStandardRep myCoeffs

--------------------------------------------------------------------------------

{-
sanityCheck1a n = unpackFlatArrayToList (packFlatArrayFromList' n coeffs   ) == take n coeffs
sanityCheck1b n = unpackFlatArrayToList (packFlatArrayFromList' n coeffsStd) == take n coeffsStd
sanityCheck2  n = unpackFlatArrayToList (packFlatArrayFromList' n points   ) == take n points
-}

--------------------------------------------------------------------------------

standardMSM :: Int -> G1Affine
standardMSM n = Affine.msmStd
  (packFlatArrayFromList' n myCoeffsStd)
  (packFlatArrayFromList' n myPoints) 

montgomeryMSM :: Int -> G1Affine
montgomeryMSM n = msm
  (packFlatArrayFromList' n myCoeffs)
  (packFlatArrayFromList' n myPoints) 
  
referenceMSM :: Int -> G1Affine
referenceMSM n = toAffine $ grpSum $ take n $ zipWith scalarMul myCoeffs (map fromAffine myPoints :: [G1Proj])

--------------------------------------------------------------------------------

main :: IO ()
main = do
  let n = 123
  let ex1 = standardMSM   n
  let ex2 = montgomeryMSM n
  let ref = referenceMSM  n
  putStrLn ""
  putStrLn $ "result of (standard coeff)   MSM of size " ++ show n ++ "\n = " ++ show ex1 ++ "\n"
  putStrLn $ "result of (Montgomery coeff) MSM of size " ++ show n ++ "\n = " ++ show ex2 ++ "\n"
  putStrLn $ "result of reference          MSM of size " ++ show n ++ "\n = " ++ show ref ++ "\n" 
  if (ex1 == ref) && (ex1 == ex2)
    then putStrLn "they all match! OK."
    else putStrLn "they do not match! FAIL!"

--------------------------------------------------------------------------------
