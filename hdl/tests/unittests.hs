import Prelude

import Test.Tasty

import qualified Heron.Tests.Template
import qualified Heron.Tests.Core.ParStack
import qualified Heron.Tests.Core.Stack
import qualified Heron.Tests.Core.Heap
import qualified Heron.Tests.Core.Rom
import qualified Heron.Tests.Core.Board

main :: IO ()
main = defaultMain $ testGroup "."
  [ Heron.Tests.Template.tests
  , Heron.Tests.Core.ParStack.tests
  , Heron.Tests.Core.Stack.tests
  , Heron.Tests.Core.Heap.tests
  , Heron.Tests.Core.Rom.tests
  , Heron.Tests.Core.Board.tests
  ]
