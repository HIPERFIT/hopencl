module Main where

import Test.Framework
import qualified HOpenCL.Platform_Test
import qualified HOpenCL.Device_Test
import qualified HOpenCL.Context_Test
import qualified HOpenCL.CommandQueue_Test
import qualified HOpenCL.Program_Test
import qualified HOpenCL.Kernel_Test

-- Use plain output format, to avoid terminal color annotations
main = do opts <- interpretArgsOrExit ["--plain"]
          defaultMainWithOpts [tests] opts

tests = testGroup "HOpenCL"
        [ HOpenCL.Platform_Test.tests
        , HOpenCL.Device_Test.tests
        , HOpenCL.Context_Test.tests
        , HOpenCL.CommandQueue_Test.tests
        , HOpenCL.Program_Test.tests
        , HOpenCL.Kernel_Test.tests
        ]
