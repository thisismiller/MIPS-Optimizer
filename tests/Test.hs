{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework
import {-@ HTF_TESTS @-} ParserTests

main = htfMain htf_importedTests
