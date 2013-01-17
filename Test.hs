{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import Parser

assertParse insn s =
  let
    expected = Right [insn]
    actual = case parseInstructions s of
      Left err -> Left $ "Failed " ++ show err
      Right ast -> Right ast
  in
    assertEqual expected actual

test_parse_ADDU = assertParse (ArithOp "ADDU" 0 0 0) "ADDU R0, R0, R0"
test_parse_ADDIU = assertParse (ArithIOp "ADDIU" 0 0 0) "ADDIU R0, R0, #0"
test_parse_BEQ = assertParse (BranchOp "BEQ" 0 0 "lbl") "BEQ R0, R0, lbl"
test_parse_LW = assertParse (LoadOp "LW" 0 0 0) "LW R0, #0(R0)"
test_parse_SB = assertParse (StoreOp "SB" 0 0 0) "SB R0, #0(R0)"
test_parse_Label = assertParse (LabelOp "lbl") "lbl:"

main = htfMain htf_thisModulesTests
