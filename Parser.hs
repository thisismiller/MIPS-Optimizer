module Parser (parseInstructions, printInsts, AST(..)) where

import Control.Applicative hiding (many, (<|>))
import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec
import Text.Parsec.Token
import Text.Parsec.Language
import Text.Printf

parser = makeTokenParser emptyDef { commentLine = [';'], caseSensitive = False }

intP = fmap fromIntegral $ integer parser
insnP = symbol parser
identP = identifier parser
commaP = comma parser
colonP = colon parser
parensP = parens parser

data Instruction =
    Arithmetic String
  | ArithmeticI String
  | Branch String
  | Load String
  | Store String
  | Label String
    deriving (Eq,Show)

data AST =
    ArithOp String Int Int Int
  | ArithIOp String Int Int Int
  | BranchOp String Int Int String
  | LoadOp String Int Int Int
  | StoreOp String Int Int Int
  | LabelOp String
  | Stall
    deriving (Eq,Show)

--instruction : TokArith reg ',' reg ',' reg { ArithOp $1 $2 $4 $6 }
--            | TokArithI reg ',' reg ',' int { ArithIOp $1 $2 $4 $6 }
--            | TokBranch reg ',' reg ',' label { BranchOp $1 $2 $4 $6 }
--            | TokLoad reg ',' int '(' reg ')' { LoadOp $1 $2 $4 $6 }
--            | TokStore reg ',' int '(' reg ')' { StoreOp $1 $2 $4 $6 }
--            | TokLabel ':' { LabelOp $2 }

register :: Parser Int
register = char 'R' *> intP

-- inst reg , reg , reg
parseArithmetic :: Parser AST
parseArithmetic = do
  inst <- choice $ map (try.insnP) ariths
  r1 <- register
  commaP
  r2 <- register
  commaP
  r3 <- register
  return $ ArithOp inst r1 r2 r3
  where
    ariths = ["ADD", "ADDU", "AND", "DIV", "DIVU", "MULT", "MULTU",
              "OR", "SLLV", "SLT", "SRLV, SUB", "SUBU", "XOR"]

-- inst reg, reg, imm
parseArithmeticI :: Parser AST
parseArithmeticI = do
  inst <- choice $ map (try.insnP) arithis
  r1 <- register
  commaP
  r2 <- register
  commaP
  imm <- intP
  return $ ArithIOp inst r1 r2 imm
  where
    arithis = ["ADDI", "ADDIU", "ANDI", "ORI", "SLTI",
               "SLTIU", "SRA", "SRL", "XORI"]

-- inst reg, reg, label
parseBranch :: Parser AST
parseBranch = do
  inst <- choice $ map (try.insnP) ["BEQ", "BNE"]
  r1 <- register
  commaP
  r2 <- register
  commaP
  label <- identP
  return $ BranchOp inst r1 r2 label

-- inst reg, offset(reg)
parseLoad :: Parser AST
parseLoad = do
  inst <- choice $ map (try.insnP) ["LW", "LB"]
  dst <- register
  commaP
  offset <- char '#' *> intP
  src <- parensP register
  return $ LoadOp inst dst offset src

-- inst reg, offset(reg)
parseStore :: Parser AST
parseStore = do
  inst <- choice $ map (try.insnP) ["SB", "SW"]
  src <- register
  commaP
  offset <- char '#' *> intP
  dest <- parensP register
  return $ StoreOp inst src offset dest

-- label:
parseLabel :: Parser AST
parseLabel = fmap LabelOp $ identP <* colonP

program :: Parser [AST]
program = many $ choice [parseArithmetic, parseArithmeticI, parseBranch,
                         parseLoad, parseStore, parseLabel]

parseInstructions :: String -> Either ParseError [AST]
parseInstructions s = parse program "stdin" s


printInst inst =
  case inst of
    ArithOp name dst src1 src2 -> printf "%s R%d, R%d, R%d" name dst src1 src2
    ArithIOp name dst src lit -> printf "%s R%d, R%d, #%d" name dst src lit
    BranchOp name src1 src2 lbl -> printf "%s R%d, R%d, %s" name src1 src2 lbl
    LoadOp name dst offset src -> printf "%s R%d, #%d(R%d)" name dst offset src
    StoreOp name src offset dst -> printf "%s R%d, #%d(R%d)" name src offset dst
    LabelOp name -> printf "%s:" name
    Stall -> printf "%s" "STALL"

printInsts :: [AST] -> IO ()
printInsts insts = sequence_ $ map (putStrLn.printInst) insts
