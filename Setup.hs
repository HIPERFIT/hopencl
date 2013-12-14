import Control.Monad (void, when)
import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags, fromFlagOrDefault, buildDistPref)
import Distribution.PackageDescription (HookedBuildInfo, emptyHookedBuildInfo)
import System.Cmd (system)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { preBuild = maybeRunC2HS }

-- Because of a problem with Cabal dependency resolution of .chs
-- files, we need to execute C2HS manually on these two files
maybeRunC2HS :: Args -> BuildFlags -> IO HookedBuildInfo
maybeRunC2HS _args flags = do
    chiExists <- doesFileExist chiFile
    when (not chiExists) $ do
      let c2hs_args = "--output-dir=" ++ buildDir ++
                      " --include=" ++ buildDir ++
                      " --cppopts=-Iinclude --cppopts=-U__BLOCKS__"
      createDirectoryIfMissing True internalDir
      void $ system $ "c2hs " ++ c2hs_args ++ chsFile
    return emptyHookedBuildInfo
  where
    distDir = fromFlagOrDefault "dist" (buildDistPref flags)
    buildDir = distDir </> "build"
    internalDir = buildDir </> "Foreign/OpenCL/Bindings/Internal"
    chiFile = internalDir </> "Types.chi"
    chsFile = internalDir </> "Types.chs"
