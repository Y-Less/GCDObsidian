{- # LANGUAGE NoMonomorphismRestriction # -}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Handy (
		module CUDA,
		module Obsidian.GCDObsidian,
		cd1, cd2, cd3, (??), (\\),
    FromFloat(..),
    FromInt(..)
	) where

import Obsidian.GCDObsidian

import qualified Obsidian.GCDObsidian.CodeGen.CUDA as CUDA

import Obsidian.GCDObsidian.Types
import qualified Obsidian.GCDObsidian.Types as Types

import Prelude hiding (zipWith,splitAt)
import GHC.Word

promote1 :: (a -> a) -> (PullArray a -> PullArray a)
promote1 func arr0 = mkPullArray (\ix -> func (arr0 ! ix)) (len arr0)

promote2 :: (a -> a -> a) -> (PullArray a -> PullArray a -> PullArray a)
promote2 func arr0 arr1 = mkPullArray (\ix -> func (arr0 ! ix) (arr1 ! ix)) (min (len arr0) (len arr1))

promoteG1 :: (a -> a) -> (GlobalPullArray a -> GlobalPullArray a)
promoteG1 func arr0 = mkGlobalPullArray (\ix -> func (arr0 ! ix)) (globLen arr0)

promoteG2 :: (a -> a -> a) -> (GlobalPullArray a -> GlobalPullArray a -> GlobalPullArray a)
promoteG2 func arr0 arr1 = mkGlobalPullArray (\ix -> func (arr0 ! ix) (arr1 ! ix)) (globLen arr0)

--intFromInteger :: Integer -> Array IntE --Kernel (Array IntE)
--intFromInteger arr0 = Array (\ix -> Literal (fromInteger $ toInteger ix)) (fromInteger arr0)

--pureFromInteger :: Integer -> Array FloatE --Kernel (Array IntE)
--pureFromInteger arr0 = Array (\ix -> Literal (fromInteger $ toInteger ix)) (fromInteger arr0)

-- These GHC extensions are VERY handy (for hacking the type system).
type IArr = PullArray IntE
type BArr = PullArray BoolE
type FArr = PullArray FloatE

type IGArr = GlobalPullArray IntE
type BGArr = GlobalPullArray BoolE
type FGArr = GlobalPullArray FloatE

instance Eq IArr where
	(==) = undefined
	(/=) = undefined

instance Num IArr where
	(+) = promote2 (+)
	(*) = promote2 (*)
	(-) = promote2 (-)
	negate      = promote1 (negate)
	abs         = promote1 (abs)
	signum      = promote1 (signum)
	fromInteger arr0 = mkPullArray (\ix -> Literal (fromInteger $ toInteger ix)) (fromInteger arr0)

instance Eq FArr where
	(==) = undefined
	(/=) = undefined

instance Num FArr where
	(+) = promote2 (+)
	(*) = promote2 (*)
	(-) = promote2 (-)
	negate      = promote1 (negate)
	abs         = promote1 (abs)
	signum      = promote1 (signum)
	fromInteger arr0 = mkPullArray (\ix -> Literal (fromInteger $ toInteger ix)) (fromInteger arr0)

instance Eq IGArr where
	(==) = undefined
	(/=) = undefined

instance Num IGArr where
	(+) = promoteG2 (+)
	(*) = promoteG2 (*)
	(-) = promoteG2 (-)
	negate      = promoteG1 (negate)
	abs         = promoteG1 (abs)
	signum      = promoteG1 (signum)
	fromInteger arr0 = mkGlobalPullArray (\ix -> Literal (fromInteger $ toInteger ix)) (Literal $ fromInteger arr0)

instance Eq FGArr where
	(==) = undefined
	(/=) = undefined

instance Num FGArr where
	(+) = promoteG2 (+)
	(*) = promoteG2 (*)
	(-) = promoteG2 (-)
	negate      = promoteG1 (negate)
	abs         = promoteG1 (abs)
	signum      = promoteG1 (signum)
	fromInteger arr0 = mkGlobalPullArray (\ix -> Literal (fromInteger $ toInteger ix)) (Literal $ fromInteger arr0)

cd1 func arr0 = putStrLn $ CUDA.genKernel "kernel" (pure func') (arr0)
	where
		func' a = func a

cd2 func arr0 arr1 = putStrLn $ CUDA.genKernel "kernel" (pure func') (arr0, arr1)
	where
		func' (a, b) = func a b

cd3 func arr0 arr1 arr2 = putStrLn $ CUDA.genKernel "kernel" (pure func') (arr0, (arr1, arr2))
	where
		func' (a, (b, c)) = func a b c

data ThenElse b = ThenElse b

class Triadic a b where
	(??) :: a -> ThenElse (b, b) -> b

(\\) :: b -> b -> ThenElse (b, b)
(\\) tru fls = ThenElse (tru, fls)

infixr 1 ??
infixr 1 \\

instance (Choice b) => Triadic (Exp Bool) b where
	(??) tst (ThenElse (tru, fls)) = ifThenElse tst tru fls

instance Triadic Bool b where
	(??) tst (ThenElse (tru, fls)) = if tst then tru else fls

instance Triadic BArr IArr where
	(??) tst (ThenElse (tru, fls)) = mkPullArray (\ix -> ifThenElse (tst ! ix) (tru ! ix) (fls ! ix)) (len tst)

---------------------------------------
class FromFloat a where
	fromFloat :: Float -> a

instance FromFloat Float where
	fromFloat x = x

instance FromFloat Int where
	fromFloat = round

instance FromFloat IntE where
	fromFloat = Literal . fromFloat

instance FromFloat FloatE where
	fromFloat = Literal

---------------------------------------
class FromWord32 a where
	fromWord32 :: Word32 -> a

instance FromWord32 Int where
	fromWord32 = fromInteger . toInteger

instance FromWord32 IntE where
	fromWord32 = Literal . fromWord32

instance FromWord32 Float where
	fromWord32 = fromInteger . toInteger

instance FromWord32 FloatE where
	fromWord32 = Literal . fromWord32

---------------------------------------
class FromInt a where
	fromInt :: Int -> a

instance FromInt Word32 where
	fromInt = fromInteger . toInteger

instance FromInt UWordE where
	fromInt = Literal . fromInt

instance FromInt Float where
	fromInt = fromInteger . toInteger

instance FromInt FloatE where
	fromInt = Literal . fromInt

---------------------------------------
class FromExpWord32 a where
	fromExpWord32 :: UWordE -> a

instance FromExpWord32 IntE where
	fromExpWord32 = CastOp Types.Word32 Types.Int

instance FromExpWord32 FloatE where
	fromExpWord32 = CastOp Types.Word32 Types.Float

---------------------------------------
class FromExpFloat a where
	fromExpFloat :: FloatE -> a

instance FromExpFloat IntE where
	fromExpFloat = CastOp Types.Float Types.Int

instance FromExpFloat UWordE where
	fromExpFloat = CastOp Types.Float Types.Word32

---------------------------------------
class FromExpInt a where
	fromExpInt :: IntE -> a

instance FromExpInt UWordE where
	fromExpInt = CastOp Types.Int Types.Word32

instance FromExpInt FloatE where
	fromExpInt = CastOp Types.Int Types.Float
