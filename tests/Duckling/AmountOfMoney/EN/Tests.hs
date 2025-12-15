-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


module Duckling.AmountOfMoney.EN.Tests
  ( tests
  ) where

import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.AmountOfMoney.EN.Corpus
import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Testing.Asserts
import Duckling.Testing.Types (testContext, testOptions, withLocale)
import Duckling.Types (Range(..))
import qualified Duckling.AmountOfMoney.EN.US.Corpus as US

tests :: TestTree
tests = testGroup "EN Tests"
  [ makeCorpusTest [Seal AmountOfMoney] corpus
  , makeNegativeCorpusTest [Seal AmountOfMoney] negativeCorpus
  , localeTests
  , intersectTests
  , rangeTests
  , latentTests
  ]

localeTests :: TestTree
localeTests = testGroup "Locale Tests"
  [ testGroup "EN_US Tests"
    [ makeCorpusTest [Seal AmountOfMoney]
      $ withLocale corpus localeUS US.allExamples
    , makeNegativeCorpusTest [Seal AmountOfMoney]
      $ withLocale negativeCorpus localeUS US.negativeExamples
    ]
  ]
  where
    localeUS = makeLocale EN $ Just US


intersectTests :: TestTree
intersectTests = testCase "Intersect Test" $
  mapM_ (analyzedNTest testContext testOptions . withTargets [Seal AmountOfMoney]) xs
  where
    xs = [ ("7c7", 2)
         , ("7c7c", 3)
         , ("10 dollars 0.17", 2)
         , ("1.1 dollars 6", 2)
         , ("10 cents and 8 cents", 2)
         , ("10 dollars and 2 dollars", 2)
         , ("between 4.1 dollars and 4 dollars", 3)
         , ("between 4 dollars and 7 euros", 2)
         ]

rangeTests :: TestTree
rangeTests = testCase "Range Test" $
  mapM_ (analyzedRangeTest testContext testOptions . withTargets [Seal AmountOfMoney]) xs
  where
    xs = [ ("between 3 and 1 dollars", Range 14 23)
         , ("between 1 and between 2 and 3 dollars", Range 14 37)
         , ("10 cents and 0.1", Range 0 8)
         , ("Pay Kiran1 10eur", Range 11 16)
         ]

latentTests :: TestTree
latentTests = makeCorpusTest [Seal AmountOfMoney] latentCorpus
