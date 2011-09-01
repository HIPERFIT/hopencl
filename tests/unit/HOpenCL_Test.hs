
module Main where

import Test.Framework
import qualified HOpenCL.Platform_Test
import qualified HOpenCL.Device_Test
import qualified HOpenCL.Context_Test
import qualified HOpenCL.CommandQueue_Test
import qualified HOpenCL.Program_Test

main = defaultMain [tests]

tests = testGroup "HopenCL"
        [ HOpenCL.Platform_Test.tests
        , HOpenCL.Device_Test.tests
        , HOpenCL.Context_Test.tests
        , HOpenCL.CommandQueue_Test.tests
        , HOpenCL.Program_Test.tests
        ]
