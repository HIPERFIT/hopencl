import Foreign.OpenCL.Extern.Platform

platformInfoFn = [
  platformProfile,
  platformVersion,
  platformName,
  platformVendor,
  platformExtensions]

main = do ps <- getPlatforms
          sequence_ [info p >>= putStrLn | p <- ps, info <- platformInfoFn]
