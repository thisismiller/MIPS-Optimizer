import Parser
import Analysis

main = do
  s <- getContents
  case parseInstructions s of
    Left err -> putStrLn $ show err
    Right ast -> printInsts ast
