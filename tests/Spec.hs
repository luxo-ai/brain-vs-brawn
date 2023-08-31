import qualified AISpec
import qualified Models.ErrorSpec
import qualified Models.GameSpec
import qualified Models.MoveSpec
import qualified Models.PieceSpec
import qualified Models.PlayerSpec

import           Test.Hspec

main :: IO ()
main = hspec $ do
  AISpec.spec
  Models.ErrorSpec.spec
  Models.GameSpec.spec
  Models.MoveSpec.spec
  Models.PieceSpec.spec
  Models.PlayerSpec.spec

