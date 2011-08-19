import Distribution.Simple
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, buildDir)
import Distribution.PackageDescription (PackageDescription)
--import Distribution.Simple.Setup
import System.Cmd (system)
import System.FilePath

main = defaultMainWithHooks hooks
  where hooks = simpleUserHooks { runTests = runTests' }


runTests' :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
runTests' _ _ _ lbi = do
  putStrLn "Running HOpenCL tests"
  print testprog
  system testprog
  -- putStrLn "computing code coverage ..."
  -- hpcReport
  -- putStrLn "generating code coverage reports ..."
  -- hpcMarkup
  return ()
    where testprog = (buildDir lbi) </> "hopencl_test" </> "hopencl_test"
          -- hpcReport = system $ "hpc report test"++exclArgs
          -- hpcMarkup = system $ "hpc markup test --destdir="++hpcReportDir++exclArgs
          -- excludedModules = ["Main",
          --                    "Poets.Data.ALaCarte_Gen",
          --                    "Poets.Data.ALaCarte_Test",
          --                    "Poets.Data.Type_Gen",
          --                    "Poets.Data.Type_Test",
          --                    "Poets.Data.Value_Gen",
          --                    "Poets.Data.Value_Test",
          --                    "Poets.Data_Test"]
          -- exclArgs = concatMap (" --exclude="++) excludedModules