import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.PackageDescription (HookedBuildInfo, emptyHookedBuildInfo)
import System.Cmd (system)
import System.Directory (doesFileExist, createDirectoryIfMissing)

import Control.Monad (when)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { preBuild = preBuild' }
  where
    preBuild' :: Args -> BuildFlags -> IO HookedBuildInfo
    preBuild' _ _ = do
      maybeRunC2HS
      return emptyHookedBuildInfo

-- Because of a problem with Cabal dependency resolution of .chs
-- files, we need to execute C2HS manually on these two files
maybeRunC2HS = do 
  existsA <- doesFileExist "dist/build/Foreign/OpenCL/Bindings/Internal/Types.chi"
  when (not existsA) $ do
    let c2hs_args="--output-dir=dist/build --include=dist/build  --cppopts=-Iinclude"
    createDirectoryIfMissing True "dist/build/Foreign/OpenCL/Bindings/Internal"
    _ <- system $ "c2hs " ++ c2hs_args ++ " Foreign/OpenCL/Bindings/Internal/Types.chs"
    return ()