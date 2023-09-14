module Utils.IO (clearScreen, blackTextOnWhiteBg) where

import           System.Info    (os)
import           System.Process (system)

clearScreen :: IO ()
clearScreen = do
    _ <- system $ if os == "mingw32" then "cls" else "clear"
    return ()


blackTextOnWhiteBg :: String -> String
blackTextOnWhiteBg str = "\ESC[30m\ESC[47m" ++ str ++ "\ESC[0m"
