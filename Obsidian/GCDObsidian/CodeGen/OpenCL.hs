{- 
   CodeGen.OpenCL

   OpenCL code generation.
-} 

module Obsidian.GCDObsidian.CodeGen.OpenCL (genKernel) where 

import Data.List
import Data.Word 
import qualified Data.Map as Map

import Obsidian.GCDObsidian.Kernel 
import Obsidian.GCDObsidian.Array 
import Obsidian.GCDObsidian.Exp  
import Obsidian.GCDObsidian.Types
import Obsidian.GCDObsidian.Globs
import Obsidian.GCDObsidian.Program

import Obsidian.GCDObsidian.CodeGen.PP
import Obsidian.GCDObsidian.CodeGen.Common
import Obsidian.GCDObsidian.CodeGen.InOut
import Obsidian.GCDObsidian.CodeGen.Liveness
import Obsidian.GCDObsidian.CodeGen.Memory


----------------------------------------------------------------------------
-- When it comes to OpenCL the gc is slightly different ;) 
gc = genConfig "__global" "__local"

syncLine = line "barrier(CLK_LOCAL_MEM_FENCE);"

tidxLine = line "unsigned int tidx = get_local_id(0);"

-- To get something that corresponds to bidx in OpenCL 
-- you need the "blocksize" 
bidxLine = line "unsigned int bidx = (get_global_id(0)-tidx) / get_local_size(0);" 

-- Here the shared memory size is needed (I think) 
-- Note:  You can set the size here (in the kernel) or 
--        from the outside. Setting it from the "outside" requires an 
--        extra parameter passed to the kernel and is thus more cumbersome.
sBase size = 
  if size > 0 
  then line$ "__local unsigned char sbase[" ++ show size ++ "];" 
  else return () 

-- TODO: CODE DUPLICATION
sbaseStr 0 t    = parens$ genCast gc t ++ "sbase" 
sbaseStr addr t = parens$ genCast gc t ++ "(sbase + " ++ show addr ++ ")" 

------------------------------------------------------------------------------
-- Generate OpenCL code to a String 

getOpenCL :: Config -> Program a -> Name -> [(String,Type)] -> [(String,Type)] -> String 
getOpenCL conf c name ins outs = 
  runPP (kernelHead name ins outs >>  
         begin >>
         tidxLine >> newline >>
         bidxLine >> newline >>
         sBase (configLocalMem conf) >> newline >> 
         genOpenCLBody conf c >>
         end ) 0 


genOpenCLBody :: Config -> Program a -> PP () 
genOpenCLBody conf prg = genProg mm nt prg
  where 
    mm = configMM conf
    nt = configThreads conf
  
genProg :: MemMap -> Word32 ->  Program a -> PP () 
genProg mm nt (Assign name ix a) = 
  case Map.lookup name mm of 
    Just (addr,t) -> 
      do
        line$  sbaseStr addr t ++ "[" ++ concat (genExp gc mm ix) ++ "] = " ++ 
          concat (genExp gc mm a) ++ ";" 
        newline
    Nothing ->  --- A result array
      do
        line$  name ++ "[" ++ concat (genExp gc mm ix) ++ "] = " ++ 
          concat (genExp gc mm a) ++ ";"
        newline
genProg mm nt (ForAll f n) =  potentialCond gc mm n nt $ 
                               genProg mm nt (f (variable "tidx"))
genProg mm nt (Allocate name size t _) = return ()
genProg mm nt Skip = return ()
genProg mm nt (Synchronize _) = syncLine >> newline
genProg mm nt (ProgramSeq p1 p2) = 
  do 
    genProg mm nt p1
    genProg mm nt p2


------------------------------------------------------------------------------
-- C style function "header"
kernelHead :: Name -> 
              [(String,Type)] -> 
              [(String,Type)] -> 
              PP () 
kernelHead name ins outs = 
  do 
    line ("__kernel void " ++ name ++ "(" ++ types ++ ")" )   
  where 
    types = concat (intersperse "," (typeList (ins ++ outs)))
    typeList :: [(String,Type)] -> [String] 
    typeList []              = [] 
    typeList ((a,t):xs)      = (genType gc (Global t) ++ a) : typeList xs
  
  
------------------------------------------------------------------------------
-- make "runnable" code 
-- Gives a string that should be a runnable OpenCL kernel

genKernel :: (InOut a, InOut b) => String -> (a -> Kernel b) -> a -> String 
genKernel name kernel a = opencl 
  where 
    (input,ins)  = runInOut (createInputs a) (0,[])
  
    ((res,_),c)  = runKernel (kernel input)
    lc = liveness c
   
    threadBudget = 
      case c of 
        Skip -> gcdThreads res
        a  -> threadsNeeded c 
        
    (m,mm) = mapMemory lc sharedMem Map.empty
    (outCode,outs)   = 
      runInOut (writeOutputs threadBudget res) (0,[])
      
    c' = sc *>* outCode
    sc = c  -- remove 
    
    opencl = getOpenCL (config threadBudget mm (size m)) c' name (map fst2 ins) (map fst2 outs)
    
