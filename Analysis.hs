module Analysis (schedule) where

import Parser
import qualified Data.Map as Map

schedule :: [AST] -> [AST]
schedule insts = scheduleTail insts Map.empty 1 []

scheduleTail (ArithOp name dst src1 src2:insts) avail cycle schedule =
    let inst = ArithOp name dst src1 src2 in
    if (Map.findWithDefault cycle src1 avail) - 3 <= cycle &&
       (Map.findWithDefault cycle src2 avail) - 3 <= cycle then
        scheduleTail insts (Map.insert dst (cycle+4) avail) (cycle+1) (inst:schedule)
    else
        scheduleTail (inst:insts) avail (cycle+1) (Stall:schedule)
scheduleTail (ArithIOp name dst src lit:insts) avail cycle schedule =
    let inst = ArithIOp name dst src lit in
    if (Map.findWithDefault cycle src avail) - 3 <= cycle then
        scheduleTail insts (Map.insert dst (cycle+4) avail) (cycle+1) (inst:schedule)
    else
        scheduleTail (inst:insts) avail (cycle+1) (Stall:schedule)
scheduleTail (BranchOp name src1 src2 lbl:insts) avail cycle schedule =
    let inst = BranchOp name src1 src2 lbl in
    if (Map.findWithDefault cycle src1 avail) - 2 <= cycle &&
       (Map.findWithDefault cycle src2 avail) - 2 <= cycle then
        scheduleTail insts avail (cycle+1) (inst:schedule)
    else
        scheduleTail (inst:insts) avail (cycle+1) (Stall:schedule)
scheduleTail (LoadOp name dst lit src:insts) avail cycle schedule =
    let inst = LoadOp name dst lit src in
    if (Map.findWithDefault cycle src avail) - 3 <= cycle then
        scheduleTail insts (Map.insert dst (cycle+5) avail) (cycle+1) (inst:schedule)
    else
        scheduleTail (inst:insts) avail (cycle+1) (Stall:schedule)
scheduleTail (StoreOp name dst lit src:insts) avail cycle schedule =
    let inst = StoreOp name dst lit src in
    if (Map.findWithDefault cycle src avail) - 3 <= cycle then
        scheduleTail insts avail (cycle+1) (inst:schedule)
    else
        scheduleTail (inst:insts) avail (cycle+1) (Stall:schedule)
scheduleTail [] avail cycle schedule = reverse schedule
