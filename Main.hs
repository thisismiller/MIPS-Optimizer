import Lexer
import Parser
import Analysis

main = do
  s <- getContents
  printInsts $ insertStalls $ parser $ lexer s
