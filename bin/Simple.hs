
import Prelude
import System.Environment (getArgs)
import Clash.Main (defaultMain)

main :: IO ()
main = defaultMain
  [ "--systemverilog"
  -- , "src/Example/Project.hs"
  , "src/ISR/Project.hs"
  ]
