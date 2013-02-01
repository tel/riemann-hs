import Test.Framework (defaultMain, testGroup)
import Test.HUnit
import Test.Framework.Providers.HUnit (testCase)

main :: IO ()
main = defaultMain [
  testGroup "(default)" [
     testCase "isGood" (True @=? True)
     ]
  ]