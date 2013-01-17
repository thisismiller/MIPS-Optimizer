module Analysis (insertStalls, toBB) where

import Parser
import qualified Data.Map as Map

data Instruction = Instruction {
  instLatency :: Int,
  instSources :: [Int],
  instDestinations :: [Int],
  instOp :: AST
} deriving (Show)

fromAST :: AST -> Instruction
fromAST op =
  case op of
    op@(ArithOp name dst src1 src2) -> Instruction 4 [src1, src2] [dst] op
    op@(ArithIOp name dst src lit) -> Instruction 4 [src] [dst] op
    op@(BranchOp name src1 src2 lbl) -> Instruction 1 [src1, src2] [] op
    op@(LoadOp name dst lit src) -> Instruction 5 [src] [dst] op
    op@(StoreOp name dst lit src) -> Instruction 1 [src] [dst] op
    op@(LabelOp name) -> Instruction 0 [] [] op 
    op@Stall -> Instruction 0 [] [] op

satisfied avail cycle inst = all (\src -> Map.findWithDefault cycle src avail - 3 <= cycle) $ instSources inst

insertStalls :: [AST] -> [AST]
insertStalls insts =
  let
    stallInst = Instruction 0 [] [] Stall

    insertAll :: Ord a => [a] -> b -> Map.Map a b -> Map.Map a b
    insertAll (x:xs) val avail = insertAll xs val $ Map.insert x val avail
    insertAll [] val avail = avail

    insertStalls' :: [Instruction] -> Map.Map Int Int -> Int -> [Instruction] -> [Instruction]
    insertStalls' (inst:insts) avail cycle schedule =
      if satisfied avail cycle inst then
        insertStalls' insts (insertAll (instDestinations inst) (cycle + instLatency inst) avail) (cycle+1) (inst:schedule)
      else
        insertStalls' (inst:insts) avail (cycle+1) (stallInst:schedule)
    insertStalls' [] avail cycle schedule = reverse schedule
  in
    map instOp $ insertStalls' (map fromAST insts) Map.empty 1 []


toBB :: [AST] -> [[AST]]
toBB insts =
  let
    toBB' (x:xs) acc bbs =
      case x of
        LabelOp name -> toBB' xs [LabelOp name] (reverse acc : bbs)
        otherwise -> toBB' xs (x:acc) bbs
    toBB' [] acc bbs = tail $ reverse $ reverse acc : bbs
  in
    toBB' insts [] []
