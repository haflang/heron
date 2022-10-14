{-|
Module      : Heron
Description : The Heron processor for acceleration of function languages
Copyright   : (c) Craig Ramsay, 2023
                  HAFLANG Project, 2023
License     : BSD-3
Maintainer  : craig.ramsay@hw.ac.uk
Stability   : experimental
Portability : POSIX

The synthesisable description of the Heron Core processor. We accelerate the
evaluation of functional programs written in F-lite by directly matching the
evaluation model of template instantiation, rather than using complex, deeply
pipelined architectures with compiled graph reduction.

See our project homepage at [haflang.github.io](http://haflang.github.io/). Our
curated [history of specialised graph reduction
machines](http://haflang.github.io/history.html) might be a good place to start.

-}
module Heron
  (sim
  ,topEntity
  ,testBench
  ,compileBenchmark
  ,dumpTemplates
  ,runEmulator
  ) where

import Heron.Encode
import Heron.External
import Heron.Core.Board
