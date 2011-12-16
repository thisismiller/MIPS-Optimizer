module Analysis (insertStalls) where

import Parser
import qualified Data.Map as Map

insertStalls :: [AST] -> [AST]
insertStalls insts = insertStallsTail insts Map.empty 1 []

class Scheduleable op where
  latency :: op -> Int
  target :: op -> Int
  satisfied :: Map.Map Int Int -> Int -> op -> Bool

instance Scheduleable AST where
  latency (ArithOp _ _ _ _) = 4
  latency (ArithIOp _ _ _ _) = 4
  latency (BranchOp _ _ _ _) = 1
  latency (LoadOp _ _ _ _) = 5
  latency (StoreOp _ _ _ _) = 1

  target (ArithOp name dst src1 src2) = dst
  target (ArithIOp name dst src lit) = dst
  target (BranchOp name src1 src2 lbl) = -1
  target (LoadOp name dst lit src) = dst
  target (StoreOp name dst lit src) = -1

  satisfied avail cycle (ArithOp name dst src1 src2) =
    (Map.findWithDefault cycle src1 avail) - 3 <= cycle &&
    (Map.findWithDefault cycle src2 avail) - 3 <= cycle
  satisfied avail cycle (ArithIOp name dst src lit) =
    (Map.findWithDefault cycle src avail) - 3 <= cycle
  satisfied avail cycle (BranchOp name src1 src2 lbl) =
    (Map.findWithDefault cycle src1 avail) - 3 <= cycle &&
    (Map.findWithDefault cycle src2 avail) - 3 <= cycle
  satisfied avail cycle (LoadOp name dst lit src) =
    (Map.findWithDefault cycle src avail) - 3 <= cycle
  satisfied avail cycle (StoreOp name dst lit src) =
    (Map.findWithDefault cycle src avail) - 3 <= cycle

insertStallsTail (inst:insts) avail cycle schedule =
  if satisfied avail cycle inst then
    insertStallsTail insts (Map.insert (target inst) (cycle + (latency inst)) avail) (cycle+1) (inst:schedule)
  else
    insertStallsTail (inst:insts) avail (cycle+1) (Stall:schedule)
insertStallsTail [] avail cycle schedule = reverse schedule
