import           Test.Arc
import           Test.Cuckoo.Filter

main :: IO ()
main =
  sequence_ [
      Test.Arc.tests
    , Test.Cuckoo.Filter.tests
    ]
