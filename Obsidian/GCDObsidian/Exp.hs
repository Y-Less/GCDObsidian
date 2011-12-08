{-# LANGUAGE GADTs, 
             TypeFamilies,  
             RankNTypes,
             FlexibleContexts,
             FlexibleInstances, 
             UndecidableInstances #-} 

module Obsidian.GCDObsidian.Exp where 

import Data.List
import Data.Word 
import Data.Bits

import qualified Foreign.Storable as Storable

------------------------------------------------------------------------------
-- Obsidian imports
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs

------------------------------------------------------------------------------
-- some synonyms
type Data a = Exp a 

type IntE    = Exp Int      
type FloatE  = Exp Float  
type DoubleE = Exp Double 
type BoolE   = Exp Bool    
type UByteE  = Exp Word8   
type UShortE = Exp Word16 
type UWordE  = Exp Word32 
type ULongE  = Exp Word64 

------------------------------------------------------------------------------
-- Class Scalar. (Things that are not tuples) 
class Show a => Scalar a where 
  sizeOf :: Exp a -> Int   --  
  typeOf :: Exp a -> Type  --   Good enough for me ... 


instance Scalar Bool where 
  --sizeOf _ = Storable.sizeOf (undefined :: Int)
  sizeOf _ = 4
  typeOf _ = Bool 

instance Scalar Int where 
  --sizeOf _ = Storable.sizeOf (undefined :: Int)
  sizeOf _ = 4
  typeOf _ = Int
  
instance Scalar Float where
  sizeOf _ = Storable.sizeOf (undefined :: Float)
  typeOf _ = Float

  
instance Scalar Double where 
  sizeOf _ = Storable.sizeOf (undefined :: Double) 
  typeOf _ = Double

  
instance Scalar Word8 where
  sizeOf _ = 1
  typeOf _ = Word8 

  
instance Scalar Word16 where 
  sizeOf _ = 2
  typeOf _ = Word16

  
instance Scalar Word32 where 
  sizeOf _ = 4 
  typeOf _ = Word32

  
instance Scalar Word64 where 
  sizeOf _ = 8 
  typeOf _ = Word64

----------------------------------------------------------------------------
-- Expressions 
data Exp a where
  Literal :: Scalar a 
             => a 
             -> Exp a 

  Index   :: Scalar a => 
             (Name,[Exp Word32]) 
             -> Exp a 
             
  If      :: Scalar a 
             => Exp Bool
             -> Exp a 
             -> Exp a 
             -> Exp a 
             
  BinOp   :: (Scalar a,
              Scalar b, 
              Scalar c) 
             => Op ((a,b) -> c) 
             -> Exp a 
             -> Exp b 
             -> Exp c 
             
  UnOp    :: (Scalar a, 
              Scalar b)
             => Op (a -> b)            
             -> Exp a 
             -> Exp b 
  
  CastOp  :: (Scalar a,
              Scalar b)
             => Type
             -> Type
             -> Exp b
             -> Exp a
  
----------------------------------------------------------------------------
-- Operations

data Op a where 
  Add :: Num a => Op ((a,a) -> a) 
  Sub :: Num a => Op ((a,a) -> a) 
  Mul :: Num a => Op ((a,a) -> a) 
  Div :: Num a => Op ((a,a) -> a) 
  -- If  :: Op ((Bool,a,a) -> a) 
  
  Mod :: Integral a => Op ((a,a) -> a)
         
  -- Trig
  Sin :: Floating a => Op (a -> a) 
  Cos :: Floating a => Op (a -> a)
  
  -- Comparisons
  Eq  :: Ord a => Op ((a,a) -> Bool)
  Lt  :: Ord a => Op ((a,a) -> Bool) 
  LEq :: Ord a => Op ((a,a) -> Bool) 
  Gt  :: Ord a => Op ((a,a) -> Bool) 
  GEq :: Ord a => Op ((a,a) -> Bool) 
  
  -- Bitwise 
  BitwiseAnd :: Bits a => Op ((a,a) -> a) 
  BitwiseOr  :: Bits a => Op ((a,a) -> a)
  BitwiseXor :: Bits a => Op ((a,a) -> a) 
  BitwiseNeg :: Bits a => Op (a -> a) 
  ShiftL     :: Bits a => Op ((a, Int) -> a) 
  ShiftR     :: Bits a => Op ((a, Int) -> a) 
  
  -- built-ins
  Min        :: Ord a => Op ((a,a) -> a) 
  Max        :: Ord a => Op ((a,a) -> a) 
  
  -- Floating (different CUDA functions for float and double, issue maybe?)
  Exp :: Floating a => Op (a -> a) -- "expf"
  Sqrt :: Floating a => Op (a -> a) -- "sqrtf"
  --RSqrt :: Floating a => Op (a -> a) -- "rsqrtf"
  Log :: Floating a => Op (a -> a) -- "logf"
  Log2 :: Floating a => Op (a -> a) -- "log2f"
  Log10 :: Floating a => Op (a -> a) -- "log10f"
  Pow :: Floating a => Op ((a, a) -> a) -- "powf"
  -- Floating Trig
  Tan :: Floating a => Op (a -> a) -- "tanf"
  ASin :: Floating a => Op (a -> a) -- "asinf"
  ATan :: Floating a => Op (a -> a) -- "atanf"
  ACos :: Floating a => Op (a -> a) -- "acosf"
  SinH :: Floating a => Op (a -> a) -- "sinhf"
  TanH :: Floating a => Op (a -> a) -- "tanhf"
  CosH :: Floating a => Op (a -> a) -- "coshf"
  ASinH :: Floating a => Op (a -> a) -- "asinhf"
  ATanH :: Floating a => Op (a -> a) -- "atanhf"
  ACosH :: Floating a => Op (a -> a) -- "acoshf"
  -- There is no "div" in "Num" but it's already defined above.
  FDiv :: Floating a => Op ((a, a) -> a) -- "acoshf"

------------------------------------------------------------------------------
-- helpers 

variable name = Index (name,[])
index name ix = Index (name,[ix])

 
------------------------------------------------------------------------------
-- Collect array names

collectArrays :: Scalar a => Exp a -> [Name]
collectArrays (Literal _) = []
collectArrays (Index (name,[])) = []
collectArrays (Index (name,_)) = [name]
collectArrays (BinOp _ e1 e2) = collectArrays e1 ++ collectArrays e2
collectArrays (UnOp  _ e) = collectArrays e
collectArrays (If b e1 e2) = collectArrays b ++ 
                             collectArrays e1 ++ 
                             collectArrays e2

collectArrayIndexPairs :: Scalar a => Exp a -> [(Name,Exp Word32)]
collectArrayIndexPairs (Literal _) = []
collectArrayIndexPairs (Index (name,[])) = []
collectArrayIndexPairs (Index (name,[ix])) = [(name,ix)]
collectArrayIndexPairs (BinOp _ e1 e2) = collectArrayIndexPairs e1 ++ collectArrayIndexPairs e2
collectArrayIndexPairs (UnOp  _ e) = collectArrayIndexPairs e
collectArrayIndexPairs (If b e1 e2) = collectArrayIndexPairs b ++ 
                                      collectArrayIndexPairs e1 ++ 
                                      collectArrayIndexPairs e2




--collectArrays (Tuple t) = collectArraysTup t 
--collectArrays (Prj t e) = collectArraysPrj t e 

--collectArraysTup :: forall t. Tuple.Tuple Exp t -> [String]
--collectArraysTup Nil = []
--collectArraysTup (a :. t) = collectArrays a ++ (collectArraysTup t) 
  
--collectArraysPrj = undefined 



------------------------------------------------------------------------------
-- 
  
instance Scalar a => Show (Exp a) where 
  show = printExp 

instance Scalar a => Eq (Exp a) where 
  (==) = undefined 

instance (Scalar a, Ord a) => Ord (Exp a) where 
    min a b = BinOp Min a b
    max a b = BinOp Max a b

------------------------------------------------------------------------------
-- INT Instances 
instance Num (Exp Int) where 
  (+) a (Literal 0) = a
  (+) (Literal 0) a = a
  (+) (Literal a) (Literal b) = Literal (a+b)
  (+) a b = BinOp Add a b  
  
  (-) a (Literal 0) = a 
  (-) (Literal a) (Literal b) = Literal (a - b) 
  (-) a b = BinOp Sub a b 
  
  (*) a (Literal 1) = a 
  (*) (Literal 1) a = a
  (*) _ (Literal 0) = Literal 0 
  (*) (Literal 0) _ = Literal 0
  (*) (Literal a) (Literal b) = Literal (a * b)
  (*) a b = BinOp Mul a b 
  
  signum = undefined 
  abs = undefined
  fromInteger a = Literal (fromInteger a) 
  
  
instance Bits (Exp Int) where 
  (.&.) (Literal a) (Literal b) = Literal (a .&. b) 
  (.&.) a b = BinOp BitwiseAnd a b
  (.|.) (Literal a) (Literal b) = Literal (a .|. b)
  (.|.) a b = BinOp BitwiseOr  a b
  xor (Literal a) (Literal b) = Literal (a `xor` b) 
  xor   a b = BinOp BitwiseXor a b 
  
  --TODO: See that this is not breaking something (32/64 bit, CUDA/Haskell)
  complement (Literal i) = Literal (complement i)
  
  complement a = UnOp BitwiseNeg a
  shiftL a i = BinOp ShiftL  a (Literal i)
  shiftR a i = BinOp ShiftR  a (Literal i)
  bitSize a  = sizeOf a * 8
  isSigned a = True

----------------------------------------------------------------------------
-- Word32 Instances 
instance Num (Exp Word32) where 
  (+) a (Literal 0) = a
  (+) (Literal 0) a = a
  (+) (Literal a) (Literal b) = Literal (a+b)
  (+) a b = BinOp Add a b  
  
  (-) a (Literal 0) = a 
  (-) (Literal a) (Literal b) = Literal (a - b) 
  (-) a b = BinOp Sub a b 
  
  (*) a (Literal 1) = a 
  (*) (Literal 1) a = a
  (*) a b = BinOp Mul a b 
  
  signum = undefined 
  abs = undefined
  fromInteger a = Literal (fromInteger a) 
  
  
instance Bits (Exp Word32) where 
  (.&.) (Literal a) (Literal b) = Literal (a .&. b) 
  (.&.) a b = BinOp BitwiseAnd a b   
  (.|.) (Literal a) (Literal b) = Literal (a .|. b) 
  (.|.) a b = BinOp BitwiseOr  a b
  xor (Literal a) (Literal b) = Literal (a `xor` b) 
  xor   a b = BinOp BitwiseXor a b 
  complement (Literal i) = Literal (complement i) 
  complement a = UnOp BitwiseNeg a
  
  shiftL (Literal j) i = Literal (j `shiftL` i) 
  shiftL a i = BinOp ShiftL a (Literal i)
  
  shiftR (Literal j) i = Literal (j `shiftL` i)
  shiftR a i = BinOp ShiftR a (Literal i)
  bitSize a  = 32
  isSigned a = False

instance Real (Exp Word32) where 
  toRational = undefined
  

instance Enum (Exp Word32) where
  toEnum = undefined
  fromEnum = undefined

instance Integral (Exp Word32) where
  mod a b = BinOp Mod a b 
  div a b = BinOp Div a b
  quotRem = undefined
  toInteger = undefined

--------------------------------------------------------------------------------
-- FLOAT Instances

instance Num (Exp Float) where 
  (+) a (Literal 0) = a
  (+) (Literal 0) a = a
  (+) (Literal a) (Literal b) = Literal (a + b)
  (+) a b = BinOp Add a b  
  
  (-) a (Literal 0) = a 
  (-) (Literal a) (Literal b) = Literal (a - b) 
  (-) a b = BinOp Sub a b 
  
  (*) a (Literal 1) = a 
  (*) (Literal 1) a = a
  (*) _ (Literal 0) = Literal 0 
  (*) (Literal 0) _ = Literal 0
  (*) (Literal a) (Literal b) = Literal (a * b)
  (*) a b = BinOp Mul a b 
  
  signum = undefined 
  abs = undefined
  fromInteger a = Literal (fromInteger a) 

instance Fractional (Exp Float) where
  (/) a b = BinOp FDiv a b
  recip a = (Literal 1) / a
  fromRational a = Literal (fromRational a)

instance Floating (Exp Float) where
  pi = Literal pi
  exp a = UnOp Exp a
  sqrt a = UnOp Sqrt a
  log a = UnOp Log a
  (**) a b = BinOp Pow a b
  
  -- log_b(x) = log_e(x) / log_e(b)
  logBase (Literal 2) b = UnOp Log2 b
  logBase (Literal 10) b  = UnOp Log10 b
  logBase a b = (UnOp Log b) / (UnOp Log a)
  
  sin (Literal 0) = Literal 0
  sin a = UnOp Sin a
  tan (Literal 0) = Literal 0
  tan a = UnOp Tan a
  cos (Literal 0) = Literal 1
  cos a = UnOp Cos a
  
  asin (Literal 0) = Literal 0
  asin a = UnOp ASin a
  atan (Literal 0) = Literal 0
  atan a = UnOp ATan a
  acos (Literal 1) = Literal 0
  acos a = UnOp ACos a
  
  sinh (Literal 0) = Literal 0
  sinh a = UnOp Sin a
  tanh (Literal 0) = Literal 0
  tanh a = UnOp Tan a
  cosh (Literal 0) = Literal 1
  cosh a = UnOp Cos a
  
  asinh a = UnOp ASinH a
  atanh a = UnOp ATanH a
  acosh a = UnOp ACosH a
  
  -- Don't second guess the CUDA compiler (or, more accurately, assume that
  -- other compilers have this).
  --(/) (Literal 1) (UnOp Sqrt b) = UnOp RSqrt b -- Optimisation.

----------------------------------------------------------------------------
  
(==*) (Literal a) (Literal b) = Literal (a == b) 
(==*) a b = BinOp Eq a b
(<*)  (Literal a) (Literal b) = Literal (a < b) 
(<*)  a b = BinOp Lt a b
(<=*) (Literal a) (Literal b) = Literal (a <= b) 
(<=*) a b = BinOp LEq a b
(>*)  a b = BinOp Gt  a b
(>=*) a b = BinOp GEq a b


class Choice a where 
  ifThenElse :: Exp Bool -> a -> a -> a 

instance Scalar a => Choice (Exp a) where  
  ifThenElse (Literal False) e1 e2 = e2
  ifThenElse (Literal True)  e1 e2 = e1
  ifThenElse b e1 e2 = If b e1 e2
  
instance (Choice a, Choice b) => Choice (a,b) where
  ifThenElse b (e1,e1') (e2,e2') = (ifThenElse b e1 e2,
                                    ifThenElse b e1' e2') 
  

----------------------------------------------------------------------------
-- Built-ins


----------------------------------------------------------------------------
-- Print Expressions

printExp :: Scalar a => Exp a -> String
printExp (Literal a) = show a 
printExp (Index (name,[])) = name
printExp (Index (name,es)) = 
  name ++ "[" ++ ((concat . intersperse "," . map printExp) es) ++ "]"
printExp (BinOp op e1 e2) = "(" ++ printOp op ++ " " ++  printExp e1 ++ " " ++ printExp e2 ++ " )"
printExp (UnOp  op e) = "(" ++ printOp op ++ " " ++ printExp e ++ " )"
printExp (If b e1 e2) = "(" ++ printExp b ++ " ? " ++ printExp e1 ++ " : " ++ printExp e2 ++ ")"
--printExp (Op op e) = "(" ++ printOp op  ++ printExp e ++ ")"
--printExp (Tuple t) = printTup t
--printExp (Prj i t) = printPrj i t


printOp :: Op a -> String
printOp Add = " + " 
printOp Sub = " - " 
printOp Mul = " * "

-- printOp If  = " if "

printOp Eq  = " == "
printOp Lt  = " < " 
printOp LEq = " <= " 
printOp Gt  = " > "
printOp GEq = " >= " 

printOp Min = " Min "
printOp Max = " Max " 

printOp Sin = " Sin " 
printOp Cos = " Cos "

printOp BitwiseAnd = " & "
printOp BitwiseOr  = " | " 
printOp BitwiseXor = " ^ " 
printOp BitwiseNeg = " ~ "  

printOp Exp   = " Exp "
printOp Sqrt  = " Sqrt "
printOp Log   = " Log "
printOp Log2  = " Log2 "
printOp Log10 = " Log10 "
printOp Pow   = " ** " -- Or just "Pow", but this is like the Haskell version.
printOp Tan   = " Tan "
printOp ASin  = " ASin "
printOp ATan  = " ATan "
printOp ACos  = " ACos "
printOp SinH  = " SinH "
printOp TanH  = " TanH "
printOp CosH  = " CosH "
printOp ASinH = " ASinH "
printOp ATanH = " ATanH "
printOp ACosH = " ACosH "
printOp FDiv  = " / "

printOp Mod = " % "
printOp Div = " / "
printOp ShiftL = " << "
printOp ShiftR = " >> "
