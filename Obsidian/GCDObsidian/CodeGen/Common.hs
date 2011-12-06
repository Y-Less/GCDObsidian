{-# LANGUAGE RankNTypes, GADTs #-}

module Obsidian.GCDObsidian.CodeGen.Common where 

import Data.List
import Data.Word
import qualified Data.Map as Map 

import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Exp 
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs

import Obsidian.GCDObsidian.CodeGen.Memory

import Control.Monad.State


------------------------------------------------------------------------------
-- TINY TOOLS 
fst2 (x,y,z) = (x,y) 

------------------------------------------------------------------------------ 
data GenConfig = GenConfig { global :: String,
                             local  :: String };
  
genConfig = GenConfig


------------------------------------------------------------------------------
-- Helpers 
mappedName :: Name -> Bool 
mappedName name = isPrefixOf "arr" name


genType _ Int = "int "
genType _ Float = "float "
genType _ Double = "double "
genType _ Bool = "int " 
genType _ Word8 = "uint8_t "
genType _ Word16 = "uint16_t "
genType _ Word32 = "uint32_t "
genType _ Word64 = "uint64_t " 
genType gc (Pointer t) = genType gc t ++ "*"
genType gc (Global t) = global gc ++" "++ genType gc t  -- "__global " ++ genType t
genType gc (Local t)  = local gc  ++" "++ genType gc t 

genCast gc t = "(" ++ genType gc t ++ ")"

parens s = '(' : s ++ ")"

------------------------------------------------------------------------------
-- genExp C-style 
genExp :: Scalar a => GenConfig -> MemMap -> Exp a -> [String]
genExp gc _ (Literal a) = [show a] 
genExp gc _ (Index (name,[])) = [name]
genExp gc mm exp@(Index (name,es)) = 
  [name' ++ genIndices gc mm es]
  where 
    (offs,t)  = 
      case Map.lookup name mm of  
        Nothing -> error "array does not excist in map" 
        (Just x) -> x
    name' = if mappedName name 
            then parens$ genCast gc t ++ 
                 if offs > 0 
                 then "(sbase+" ++ show offs ++ ")"             
                 else "sbase"
            else name

   
genExp gc mm (BinOp op e1 e2) = 
  [genOp op (genExp gc mm e1 ++ genExp gc mm e2)]

genExp gc mm (UnOp op e) = 
  [genOp op (genExp gc mm e)] 
  
genExp gc mm (If b e1 e2) =   
  [genIf (genExp gc mm b ++ 
          genExp gc mm e1 ++ 
          genExp gc mm e2 )] 


genIndices gc mm es = concatMap (pIndex mm) es  
  where 
    pIndex mm e = "[" ++ concat (genExp gc mm e) ++ "]"


genIf         [b,e1,e2] = b ++ " ? " ++ e1 ++ " : " ++ e2
------------------------------------------------------------------------------
-- genOp
genOp :: Op a -> [String] -> String
genOp Add = oper "+"
genOp Sub = oper "-"
genOp Mul = oper "*"
genOp Div = oper "/"

genOp Mod = oper "%"

genOp Sin = func "sin" 
genOp Cos = func "cos"
-- Bool ops
genOp Eq  = oper "=="
genOp Lt  = oper "<"
genOp LEq = oper "<="
genOp Gt  = oper ">"
genOp GEq = oper ">="

-- Bitwise ops
genOp BitwiseAnd = oper "&"
genOp BitwiseOr  = oper "|"
genOp BitwiseXor = oper "^"
genOp BitwiseNeg = unOp "~"
genOp ShiftL     = oper "<<"
genOp ShiftR     = oper ">>"


-- built-ins 
genOp Min = func "min"
genOp Max = func "max"

-- Floating (different CUDA functions for float and double, issue maybe?)
genOp Exp   = func "expf"
genOp Sqrt  = func "sqrtf"
genOp Log   = func "logf"
genOp Log2  = func "log2f"
genOp Log10 = func "log10f"
genOp Pow   = func "powf"
genOp Tan   = func "tanf"
genOp ASin  = func "asinf"
genOp ATan  = func "atanf"
genOp ACos  = func "acosf"
genOp SinH  = func "sinhf"
genOp TanH  = func "tanhf"
genOp CosH  = func "coshf"
genOp ASinH = func "asinhf"
genOp ATanH = func "atanhf"
genOp ACosH = func "atanhf"
genOp FDiv  = oper "/"

--func  f a = f ++ "(" ++ a ++ ")" 
--oper  f a b = "(" ++ a ++ f ++ b ++ ")" 
--unOp  f a   = "(" ++ f ++ a ++ ")"

-- Updated to take any number of function parameters ("min" and "max" already
-- had custom code for them, and the addition of "pow" with almost identical
-- code meant it made sense to improve.
--func f ps = f ++ "(" ++ foldl ((++) . (++ ",")) "" ps ++ ")"
func f ps = f ++ "(" ++ concat (intersperse "," ps) ++ ")"

oper f [a, b] = "(" ++ a ++ f ++ b ++ ")" 
oper _ _      = error "Invalid arguments passed to \"oper\""

unOp  f [a]   = "(" ++ f ++ a ++ ")"
unOp  _ _     = error "Invalid arguments passed to \"unOp\""


------------------------------------------------------------------------------
-- print and indent and stuff... 
--  This is probably very ugly 

-- TODO: There is a chapter about this pretty printing in "implementing functional lang..." 
--       Look at that and learn 


type PP a = State (Int,String) a  

indent :: PP ()
indent = 
  do 
    (i,s) <- get 
    put (i+1,s) 
    
unindent :: PP () 
unindent = 
  do 
    (i,s) <- get 
    if i <= 0 then error "Whats going on" else put (i-1,s) 

line :: String -> PP () 
line str = 
  do 
    (i,s) <- get 
    put (i,s ++ str) 

  

newline :: PP () 
newline = 
  do 
    (i,s) <- get 
    let ind = replicate (i*2) ' '
    put (i,s ++ "\n" ++ ind)
    
runPP :: PP a -> Int -> String
runPP pp i = snd$ execState pp (i,"")



------------------------------------------------------------------------------
-- Configurations, threads,memorymap 

data Config = Config {configThreads  :: NumThreads, 
                      configMM       :: MemMap,
                      configLocalMem :: Word32} 
config = Config


assign :: Scalar a => GenConfig -> MemMap -> Exp a -> Exp a -> PP () 
assign gc mm name val = line ((concat (genExp gc mm name)) ++ 
                           " = " ++  concat (genExp gc mm val) ++ 
                           ";") 
                                                    
cond :: GenConfig -> MemMap -> Exp Bool -> PP ()  
cond gc mm e = line ("if " ++ concat (genExp gc mm e))  

begin :: PP () 
begin = line "{" >> indent >> newline

end :: PP () 
end =  unindent >> newline >> line "}" >> newline


-- used in both OpenCL and CUDA generation
potentialCond gc mm n nt pp 
  | n < nt = 
    do
      cond gc mm (tidx <* (fromIntegral n))
      begin
      pp       
      end 
  | n == nt = pp
              
  | otherwise = error "potentialCond: should not happen"


