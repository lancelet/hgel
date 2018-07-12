import Test.Tasty as T

import qualified GaussElimSimpleSpec (tests)

main :: IO ()
main = T.defaultMain tests


tests :: T.TestTree
tests = T.testGroup "All Tests"
    [ GaussElimSimpleSpec.tests
    ]
