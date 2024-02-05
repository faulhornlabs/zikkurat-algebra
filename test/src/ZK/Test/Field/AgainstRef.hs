
-- | Compare the implementation against the pure Haskell impl.

{-# LANGUAGE Rank2Types, ScopedTypeVariables, MultiParamTypeClasses, TypeApplications, 
             FlexibleInstances, TypeSynonymInstances 
  #-}
module ZK.Test.Field.AgainstRef where

--------------------------------------------------------------------------------

import Data.Proxy
import Control.Monad
import System.Random

import qualified ZK.Algebra.Pure.Field.Class as Pure
import qualified ZK.Algebra.Pure.Misc        as Pure

import qualified ZK.Algebra.Class.Field      as Fast
import qualified ZK.Algebra.Class.Flat       as Fast
import qualified ZK.Algebra.Class.Misc       as Fast

--------------------------------------------------------------------------------

-- class (Pure.Field ref, Fast.Field fast) => RefField ref fast

fromRef :: forall ref fast. (Pure.Field ref, Fast.Field fast) => ref -> IO fast
fromRef x = Fast.newFlat (Pure.toWordsMontgomery x)

toRef :: forall ref fast. (Pure.Field ref, Fast.Field fast) => fast -> IO ref
toRef x = Pure.fromWordsMontgomery <$> Fast.peekFlat x

--------------------------------------------------------------------------------

randomSign :: IO Integer
randomSign = do
  a <- randomRIO (0,1)
  return (2*a - 1)

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

  FieldCmp1B (refOp,fastOp) _ -> do
    s  <- randomSign
    a  <- randomRIO (0,4)              :: IO Integer
    b  <- randomRIO (-1000000,1000000) :: IO Integer
    let p = Pure.characteristic pxy1
    let e = s * (p^a + b)
    y  <- Pure.rndIO @r
    y' <- fromRef @r @f y 
    let lhs =    (refOp  y  e) 
    rhs <- toRef (fastOp y' e)
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

  FieldCmp1B (refOp,fastOp) _ -> do
    s  <- randomSign
    a  <- randomRIO (0,4)              :: IO Integer
    b  <- randomRIO (-1000000,1000000) :: IO Integer
    let p = Pure.characteristic pxy1
    let e = s * (p^a + b)
    y' <- Fast.rndIO @f
    y  <- toRef @r @f y'
    lhs <- fromRef (refOp  y  e) 
    let rhs =      (fastOp y' e)
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
  = FieldCmp1  (forall r f. (Pure.Field r, Fast.Field f) => (r -> r            , f -> f            )) String
  | FieldCmp2  (forall r f. (Pure.Field r, Fast.Field f) => (r -> r -> r       , f -> f -> f       )) String
  | FieldCmp1B (forall r f. (Pure.Field r, Fast.Field f) => (r -> Integer -> r , f -> Integer -> f )) String

fieldCmpText :: FieldCmp -> String
fieldCmpText (FieldCmp1  _ text) = text
fieldCmpText (FieldCmp2  _ text) = text
fieldCmpText (FieldCmp1B _ text) = text

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
  , op_pow
  ]

op_neg  = FieldCmp1  ( negate         , negate         ) "neg"
op_inv  = FieldCmp1  ( recip          , recip          ) "inv"
op_sqr  = FieldCmp1  ( Pure.square    , Fast.square    ) "sqr"
op_add  = FieldCmp2  ( (+)            , (+)            ) "add"
op_sub  = FieldCmp2  ( (-)            , (-)            ) "sub"
op_mul  = FieldCmp2  ( (*)            , (*)            ) "mul"
op_div  = FieldCmp2  ( (*)            , (*)            ) "div"
op_frob = FieldCmp1  ( Pure.frobenius , Fast.frobenius ) "frob"
op_pow  = FieldCmp1B ( Pure.pow       , Fast.power     ) "pow"

--------------------------------------------------------------------------------
