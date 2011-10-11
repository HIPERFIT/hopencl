import Distribution.Simple
import Distribution.Simple.Setup (BuildFlags)
import Distribution.PackageDescription (HookedBuildInfo, emptyHookedBuildInfo)
import System.Cmd (system)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks { preBuild = preBuild' }
  where
    preBuild' :: Args -> BuildFlags -> IO HookedBuildInfo
    preBuild' _ _ = do
      _ <- system "./prepare-install.sh"
      return emptyHookedBuildInfo
