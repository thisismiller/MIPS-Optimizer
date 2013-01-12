import Parser
import Analysis

import Control.Monad

main = do
  s <- getContents
  case parseInstructions s of
    Left err -> putStrLn $ show err
    Right ast -> sequence_ $ map printInsts $ toBB ast
