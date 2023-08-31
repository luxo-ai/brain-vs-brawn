import qualified Models.ErrorSpec
import qualified Models.GameSpec
import qualified Models.MoveSpec
import qualified Models.PieceSpec
import qualified Models.PlayerSpec

import           Test.Hspec

main :: IO ()
main = hspec $ do
  Models.ErrorSpec.spec
  Models.MoveSpec.spec
  Models.PieceSpec.spec
  Models.PlayerSpec.spec
  Models.GameSpec.spec

