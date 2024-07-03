import           Prelude

import           Test.Tasty

import qualified Heron.Tests.Board
import qualified Heron.Tests.Core.Collector
import qualified Heron.Tests.Core.Heap
import qualified Heron.Tests.Core.ParStack
import qualified Heron.Tests.Core.Rom
import qualified Heron.Tests.Core.Stack
import qualified Heron.Tests.Template

main :: IO ()
main = defaultMain $ testGroup "."
  [ Heron.Tests.Template.tests
  , Heron.Tests.Core.ParStack.tests
  , Heron.Tests.Core.Stack.tests
  , Heron.Tests.Core.Heap.tests
  , Heron.Tests.Core.Rom.tests
  , Heron.Tests.Board.tests
  , Heron.Tests.Core.Collector.tests
  ]
