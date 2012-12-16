{
module Lexer (
        lexer,
        Token(..),
        Instruction(..)
        ) where

import Char
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$ident = [a-zA-Z0-9]

tokens :-
    $white+ ;
    \, {\c -> Comma}
    \( {\c -> LParen}
    \) {\c -> RParen}
    \: {\c -> Colon}
    \#$digit+ {\d -> Integer (read (tail d) :: Int)}
    $digit+ {\d -> Integer (read (tail d) :: Int)}
    R$digit+ {\d -> Register (read (tail d) :: Int)}
    \$$digit+ {\d -> Register (read (tail d) :: Int)}
    \$$alpha$digit+ {\s -> namedRegister s}
    \$$alpha+ {\s -> namedRegister s}
    ADD   {\s -> Instruction $ Arithmetic s}
    ADDU  {\s -> Instruction $ Arithmetic s}
    AND   {\s -> Instruction $ Arithmetic s}
    DIV   {\s -> Instruction $ Arithmetic s}
    DIVU  {\s -> Instruction $ Arithmetic s}
    MULT  {\s -> Instruction $ Arithmetic s}
    MULTU {\s -> Instruction $ Arithmetic s}
    OR    {\s -> Instruction $ Arithmetic s}
    SLLV  {\s -> Instruction $ Arithmetic s}
    SLT   {\s -> Instruction $ Arithmetic s}
    SRLV  {\s -> Instruction $ Arithmetic s}
    SUB   {\s -> Instruction $ Arithmetic s}
    SUBU  {\s -> Instruction $ Arithmetic s}
    XOR   {\s -> Instruction $ Arithmetic s}
    ADDI  {\s -> Instruction $ ArithmeticI s}
    ADDIU {\s -> Instruction $ ArithmeticI s}
    ANDI  {\s -> Instruction $ ArithmeticI s}
    ORI   {\s -> Instruction $ ArithmeticI s}
    SLTI  {\s -> Instruction $ ArithmeticI s}
    SLTIU {\s -> Instruction $ ArithmeticI s}
    SRA   {\s -> Instruction $ ArithmeticI s}
    SRL   {\s -> Instruction $ ArithmeticI s}
    XORI  {\s -> Instruction $ ArithmeticI s}
    BEQ   {\s -> Instruction $ Branch s}
    BNE   {\s -> Instruction $ Branch s}
    LB    {\s -> Instruction $ Load s}
    LW    {\s -> Instruction $ Load s}
    SB    {\s -> Instruction $ Store s}
    SW    {\s -> Instruction $ Store s}
    $alpha$ident* {\id -> LabelName id}
{
-- Each action has type :: String -> Token

-- The token type:
data Instruction =
    Arithmetic String
  | ArithmeticI String
  | Branch String
  | Load String
  | Store String
  | Label String
    deriving (Eq,Show)

data Token =
    Register Int
  | Integer Int
  | LabelName String
  | Comma
  | Colon
  | LParen
  | RParen
  | Instruction Instruction
	deriving (Eq,Show)

namedRegister :: String -> Token
namedRegister "$zero" = Register 0
namedRegister "$at" = Register 1
namedRegister reg@('$':'v':r:[]) = 
    if '0' <= r && r <= '1' then 
        Register $ 2 + (digitToInt r)
    else
        error $ "Invalid register " ++ reg
namedRegister reg@('$':'a':r:[]) = 
    if '0' <= r && r <= '3' then
        Register $ 4 + (digitToInt r)
    else
        error $ "Invalid register " ++ reg
namedRegister reg@('$':'t':r:[]) = 
    if '0' <= r && r <= '7' then
        Register $ 8 + (digitToInt r)
    else
        if '8' <= r && r <= '9' then
            Register $ 24 + (digitToInt r)
        else
            error $ "Invald register " ++ reg
namedRegister reg@('$':'s':r:[]) =
    if '0' <= r && r <= '7' then
        Register $ 16 + (digitToInt r)
    else
        if r == 'p' then
            Register 29
        else
            error $ "Invalid register " ++ reg
namedRegister reg@('$':'k':r:[]) =
    if '0' <= r && r <= '1' then
        Register $ 24 + (digitToInt r)
    else
        error $ "Invalid register " ++ reg
namedRegister "$gp" = Register 28
namedRegister "$fp" = Register 30
namedRegister "$ra" = Register 31
namedRegister unknown = error $ "Invalid register " ++ unknown

lexer = alexScanTokens
}
--  vim: set ts=4 sw=4 tw=0 syntax=haskell ft=haskell:
