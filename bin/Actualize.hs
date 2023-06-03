import Clash.Main (defaultMain)
import System.Environment (getArgs)
import Prelude

main :: IO ()
main = do
  defaultMain
    [ "--systemverilog",
      "src/Example/Project.hs"
    ]
  defaultMain
    [ "--systemverilog",
      "src/Fib/Project.hs"
    ]
  defaultMain
    [ "--systemverilog",
      "src/ISR/Project.hs"
    ]
  defaultMain
    [ "--systemverilog",
      "src/ISR/ISR.hs"
    ]