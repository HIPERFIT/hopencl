module Test_Util where

import Test.HUnit (assertBool, Assertion)

oneOf :: (Eq a, Show a) => a -> [a] -> Assertion
oneOf x ys = assertBool msg (x `elem` ys)
  where msg = show x ++ "should be one of" ++ show ys

oneOfM :: (Eq a, Show a) => IO a -> [a] -> Assertion
oneOfM mx ys = do x <- mx
                  x `oneOf` ys

-- This is included in later versions of GHC
void :: Functor f => f a -> f ()
void = fmap (const ())

