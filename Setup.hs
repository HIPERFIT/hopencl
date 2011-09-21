--
-- This is taken from the POETS-library
--

import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir)
import Distribution.PackageDescription (PackageDescription)
--import Distribution.Simple.Setup
import System.Cmd (system)
import System.FilePath

main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests = runTests' }

hpcReportDir = "hpcreport"

runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ lbi = do
  putStrLn "Running HOpenCL tests"
  system testprog
  -- putStrLn "Computing code coverage"
  -- hpcReport
  -- putStrLn "Generating code coverage reports"
  -- hpcMarkup
  return ()
    where testprog = (buildDir lbi) </> "unit" </> "unit"
          hpcReport = system $ "hpc report unit"++exclArgs
          hpcMarkup = system $ "hpc markup unit --destdir="++hpcReportDir++exclArgs
          excludedModules = ["Test_Util",
                             "Main",
                             "HOpenCL.CommandQueue_Test",
                             "HOpenCL.Context_Test",
                             "HOpenCL.Platform_Test",
                             "HOpenCL.Device_Test",
                             "HOpenCL.Program_Test"]
          exclArgs = concatMap (" --exclude="++) excludedModules
