
-- | Compare the implementation against the pure Haskell impl.

{-# LANGUAGE Rank2Types, ScopedTypeVariables, MultiParamTypeClasses, TypeApplications, 
             FlexibleInstances, TypeSynonymInstances 
  #-}
module ZK.Test.Field.AgainstRef where

--------------------------------------------------------------------------------

import Data.Proxy
import Control.Monad

import qualified ZK.Algebra.Pure.Field.Class as Pure
import qualified ZK.Algebra.Pure.Misc        as Pure

import qualified ZK.Algebra.Class.Field      as Fast
import qualified ZK.Algebra.Class.Flat       as Fast

--------------------------------------------------------------------------------

{-

baz :: forall r f. (Pure.ExtField r, Fast.CubicExt f) => Proxy r -> Proxy f -> IO ()
baz pxy1 pxy2 = do
  x  <- Pure.rndIO @r
  x' <- fromRef @r @f x 
  putStrLn $ "x  = " ++ show x
  putStrLn $ "x' = " ++ show x'
  putStrLn ""
  putStrLn $ "inv x  = " ++ show (recip x )
  putStrLn $ "inv x' = " ++ show (recip x')
  putStrLn ""
  putStrLn $ "x  * inv x  = " ++ show (x  * recip x )
  putStrLn $ "x' * inv x' = " ++ show (x' * recip x')
  putStrLn ""

  let irred = Pure.definingPolyCoeffs (Proxy @r)
  print irred

  let [p0_,_,_,_] = irred
  p0 <- fromRef @(Pure.ExtBase r) @(Fast.ExtBase f) p0_

  let (a0,a1,a2) = Fast.cubicUnpack x'
  let denom = ( a0^3 - a1^3*p0 + 3*a0*a1*a2*p0 + a2^3*p0^2 )
  let b0    =   ( a0^2 + a1*a2*p0 ) / denom
  let b1    = - ( a0*a1 + a2^2*p0 ) / denom
  let b2    =   ( a1^2  - a0*a2   ) / denom

  let y' = Fast.cubicPack (b0,b1,b2)
  putStrLn $ "y' = " ++ show y' 
  putStrLn $ "x' * y' = " ++ show(x' * y')

--------------------

bar :: forall r f. (Pure.Field r, Fast.Field f) => Proxy r -> Proxy f -> IO ()
bar pxy1 pxy2 = do
  x  <- Pure.rndIO @r
  y  <- Pure.rndIO @r
  x' <- fromRef @r @f x 
  y' <- fromRef @r @f y 
  print (x ,y )
  print (x',y')
  let z  = recip x  -- Pure.inv     x 
  let z' = recip x' -- Fast.inverse x'
  print (x * z)
  print (x'* z')
  print z
  print z'

-}

--------------------------------------------------------------------------------

-- class (Pure.Field ref, Fast.Field fast) => RefField ref fast

fromRef :: forall ref fast. (Pure.Field ref, Fast.Field fast) => ref -> IO fast
fromRef x = Fast.newFlat (Pure.toWordsMontgomery x)

toRef :: forall ref fast. (Pure.Field ref, Fast.Field fast) => fast -> IO ref
toRef x = Pure.fromWordsMontgomery <$> Fast.peekFlat x

--------------------------------------------------------------------------------

runCmpA :: forall r f. (Pure.Field r, Fast.Field f) => FieldCmp -> Proxy r -> Proxy f -> IO Bool
runCmpA cmp pxy1 pxy2 = case cmp of
  
  FieldCmp1 (refOp,fastOp) _ -> do
    x  <- Pure.rndIO @r
    x' <- fromRef @r @f x
    let lhs =    (refOp  x )
    rhs <- toRef (fastOp x')
    return (lhs == rhs)

  FieldCmp2 (refOp,fastOp) _ -> do
    x  <- Pure.rndIO @r
    y  <- Pure.rndIO @r
    x' <- fromRef @r @f x 
    y' <- fromRef @r @f y 
    let lhs =    (refOp  x  y ) 
    rhs <- toRef (fastOp x' y')
    return (lhs == rhs)

runCmpB :: forall r f. (Pure.Field r, Fast.Field f) => FieldCmp -> Proxy r -> Proxy f -> IO Bool
runCmpB cmp pxy1 pxy2 = case cmp of
  
  FieldCmp1 (refOp,fastOp) _ -> do
    x' <- Fast.rndIO @f
    x  <- toRef @r @f x' 
    lhs <- fromRef (refOp  x )
    let rhs =      (fastOp x')
    return (lhs == rhs)

  FieldCmp2 (refOp,fastOp) _ -> do
    x' <- Fast.rndIO @f
    y' <- Fast.rndIO @f
    x  <- toRef @r @f x'
    y  <- toRef @r @f y'
    lhs <- fromRef (refOp  x  y ) 
    let rhs =      (fastOp x' y')
    return (lhs == rhs)

runSingleCmpTests :: (Pure.Field r, Fast.Field f) => Int -> FieldCmp -> Proxy r -> Proxy f -> IO Bool
runSingleCmpTests n cmp pxy1 pxy2 = do

  let m = div n 2

  as <- replicateM    m  (runCmpA cmp pxy1 pxy2)
  bs <- replicateM (n-m) (runCmpB cmp pxy1 pxy2)

  let oks = as++bs
  let ok = and oks
  case ok of
    True  -> putStrLn $ "ok (passed " ++ show (length oks) ++ " tests)"
    False -> putStrLn $ "FAILED!! (FAILED " ++ show (countFalses oks) ++ " tests!)"
  return ok
 
  where
    countFalses :: [Bool] -> Int
    countFalses = length . filter (==False)

--------------------------------------------------------------------------------

runAllCmpTests :: (Pure.Field r, Fast.Field f) => Int -> Proxy r -> Proxy f -> IO ()
runAllCmpTests n pxy1 pxy2 = do
  putStrLn ""
  putStrLn $ "running comparison against reference tests for " ++ Fast.fieldNamePxy pxy2 
  forM_ comparisons $ \cmp -> do
    putStr $ " - " ++ take 10 (fieldCmpText cmp ++ repeat ' ')
    void $ runSingleCmpTests n cmp pxy1 pxy2

--------------------------------------------------------------------------------

data FieldCmp
  = FieldCmp1 (forall r f. (Pure.Field r, Fast.Field f) => (r -> r      , f -> f     )) String
  | FieldCmp2 (forall r f. (Pure.Field r, Fast.Field f) => (r -> r -> r , f -> f -> f)) String

fieldCmpText :: FieldCmp -> String
fieldCmpText (FieldCmp1 _ text) = text
fieldCmpText (FieldCmp2 _ text) = text

comparisons :: [FieldCmp]
comparisons = 
  [ op_neg
  , op_inv
  , op_sqr
  , op_add
  , op_sub
  , op_mul
  , op_div
  , op_frob
  ]

op_neg  = FieldCmp1 ( negate         , negate         ) "neg"
op_inv  = FieldCmp1 ( recip          , recip          ) "inv"
op_sqr  = FieldCmp1 ( Pure.square    , Fast.square    ) "sqr"
op_add  = FieldCmp2 ( (+)            , (+)            ) "add"
op_sub  = FieldCmp2 ( (-)            , (-)            ) "sub"
op_mul  = FieldCmp2 ( (*)            , (*)            ) "mul"
op_div  = FieldCmp2 ( (*)            , (*)            ) "div"
op_frob = FieldCmp1 ( Pure.frobenius , Fast.frobenius ) "frob"

--------------------------------------------------------------------------------
