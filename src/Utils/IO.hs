module Utils.IO where

import           System.Info    (os)
import           System.Process (system)

clearScreen :: IO ()
clearScreen = do
    _ <- system $ if os == "mingw32" then "cls" else "clear"
    return ()
