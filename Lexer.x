{
module Lexer (
        lexer,
        Token(Register,Integer,Comma,LParen,RParen,Label,Instruction),
        Instruction(Arithmetic,ArithmeticI,Branch,Load,Store)
        ) where
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
    \#$digit+ {\d -> Integer (read (tail d) :: Int)}
    R$digit+ {\d -> Register (read (tail d) :: Int)}
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
    $alpha$ident* {\id -> Label id}
{
-- Each action has type :: String -> Token

-- The token type:
data Instruction =
    Arithmetic String
  | ArithmeticI String
  | Branch String
  | Load String
  | Store String
    deriving (Eq,Show)

data Token =
    Register Int
  | Integer Int
  | Comma
  | LParen
  | RParen
  | Label String
  | Instruction Instruction
	deriving (Eq,Show)

lexer = alexScanTokens
}
--  vim: set ts=4 sw=4 tw=0 syntax=haskell ft=haskell:
