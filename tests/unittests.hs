import Test.Tasty
import qualified Tests.DDFF.Project
import Prelude

main :: IO ()
main =
  defaultMain $
    testGroup
      "."
      [ Tests.DDFF.Project.tests
      ]
