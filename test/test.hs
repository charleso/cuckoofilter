import           Test.Cuckoo.Filter

main :: IO ()
main =
  sequence_ [
      Test.Cuckoo.Filter.tests
    ]
