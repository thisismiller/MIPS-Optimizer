import Parser
import Analysis

import Control.Monad

main = do
  s <- getContents
  case parseInstructions s of
    Left err -> print err
    Right ast -> mapM_ (printInsts.insertStalls) $ toBB ast
