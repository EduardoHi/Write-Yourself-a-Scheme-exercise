import Test.QuickCheck
import AxoParser
import Text.Megaparsec


-- Receives the Double to parse, and returns success or failure
testparseFloat :: Double -> Bool
testparseFloat x =
  let r = runParser parseReal "" (show x) in
    case r of
      Right a -> a == LispReal x
      Left _ -> False



-- main :: IO ()
-- main = quickCheck
