import Lexer
import Parser
import Analysis

main = do
  s <- getContents
  printInsts $ schedule $ parser $ lexer s
