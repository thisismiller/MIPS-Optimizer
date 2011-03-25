{
module Parser (parser, AST(ArithOp, ArithIOp, BranchOp, LoadOp, StoreOp, Stall), printInsts) where

import Lexer
}

%name parserRev
%tokentype { Token }
%token
    ',' { Comma }
    '(' { LParen }
    ')' { RParen }
    reg { Register $$ }
    int { Integer $$ }
    label { Label $$ }
    TokArith { Instruction (Arithmetic $$) }
    TokArithI { Instruction (ArithmeticI $$) }
    TokBranch { Instruction (Branch $$) }
    TokLoad { Instruction (Load $$) }
    TokStore { Instruction (Store $$) }

%%

instlist : instlist instruction { $2 : $1 }
         | instruction { [$1] }

instruction : TokArith reg ',' reg ',' reg { ArithOp $1 $2 $4 $6 }
            | TokArithI reg ',' reg ',' int { ArithIOp $1 $2 $4 $6 }
            | TokBranch reg ',' reg ',' label { BranchOp $1 $2 $4 $6 }
            | TokLoad reg ',' int '(' reg ')' { LoadOp $1 $2 $4 $6 }
            | TokStore reg ',' int '(' reg ')' { StoreOp $1 $2 $4 $6 }

{
happyError :: [Token] -> a
happyError xs = error $ "ERROR: " ++ show xs

data AST =
    ArithOp String Int Int Int
  | ArithIOp String Int Int Int
  | BranchOp String Int Int String
  | LoadOp String Int Int Int
  | StoreOp String Int Int Int
  | Stall
    deriving (Eq,Show)

printInsts insts = printInstsTail insts ""

printInstsTail (ArithOp name dst src1 src2:insts) acc =
    printInstsTail insts (acc ++ (name ++ " R" ++ (show dst) ++ ", R" ++ (show src1) ++ ", R" ++ (show src2) ++ "\n"))
printInstsTail (ArithIOp name dst src lit:insts) acc =
    printInstsTail insts (acc ++ (name ++ " R" ++ (show dst) ++ ", R" ++ (show src) ++ ", #" ++ (show lit) ++ "\n"))
printInstsTail (BranchOp name src1 src2 lbl:insts) acc =
    printInstsTail insts (acc ++ (name ++ " R" ++ (show src1) ++ ", R" ++ (show src2) ++ ", " ++ lbl ++ "\n"))
printInstsTail (LoadOp name dst lit src:insts) acc =
    printInstsTail insts (acc ++ (name ++ " R" ++ (show dst) ++ ", #" ++ (show lit) ++ "(" ++ (show src) ++ ")\n"))
printInstsTail (StoreOp name dst lit src:insts) acc =
    printInstsTail insts (acc ++ (name ++ " R" ++ (show dst) ++ ", #" ++ (show lit) ++ "(" ++ (show src) ++ ")\n"))
printInstsTail (Stall:insts) acc =
    printInstsTail insts (acc ++ "STALL\n")
printInstsTail [] acc = putStrLn acc

parser = reverse . parserRev
}
--  vim: set ts=4 sw=4 tw=0 syntax=haskell ft=haskell:
