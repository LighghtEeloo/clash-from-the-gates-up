import Clash.Main (defaultMain)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main =
  defaultMain
    [ "--systemverilog",
      -- , "src/Example/Project.hs"
      "src/ISR/Project.hs"
    ]
