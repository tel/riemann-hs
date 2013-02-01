import Test.Framework (defaultMain, testGroup)
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain [
  testGroup "(default)" [
     testProperty "isGood" (\a -> a == (a `asTypeOf` True))
     ]
  ]