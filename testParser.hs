import Test.QuickCheck
import AxoParser
import Text.Megaparsec


-- Receives the Double to parse, and returns success or failure
testparseFloat :: Double -> Bool
testparseFloat x =
  let r = runParser parseFloat "" (show x) in
    case r of
      Right a -> a == AxoFloat x
      Left _ -> False



-- main :: IO ()
-- main = quickCheck
