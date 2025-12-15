-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Duckling.Time.EN.Tests
  ( tests
  ) where

import Data.Aeson
import Data.Aeson.Types ((.:), parseMaybe, withObject)
import Data.String
import Prelude
import Test.Tasty
import Test.Tasty.HUnit

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Resolve
import Duckling.Testing.Asserts
import Duckling.Testing.Types hiding (examples)
import Duckling.Time.Corpus
import Duckling.Time.EN.Corpus
import Duckling.TimeGrain.Types (Grain(..))
import Duckling.Types (Range(..))
import qualified Duckling.Time.EN.US.Corpus as US

tests :: TestTree
tests = testGroup "EN Tests"
  [ makeCorpusTest [Seal Time] defaultCorpus
  , makeNegativeCorpusTest [Seal Time] negativeCorpus
  , makeCorpusTest [Seal Time] diffCorpus
  , exactSecondTests
  , valuesTest
  , intersectTests
  , rangeTests
  , localeTests
  , makeCorpusTest [Seal Time] latentCorpus
  ]

localeTests :: TestTree
localeTests = testGroup "Locale Tests"
  [ testGroup "EN_US Tests"
    [ makeCorpusTest [Seal Time] $ withLocale corpus localeUS US.allExamples
    , makeNegativeCorpusTest [Seal Time] $ withLocale negativeCorpus localeUS []
    ]
  ]
  where
    localeUS = makeLocale EN $ Just US

exactSecondTests :: TestTree
exactSecondTests = testCase "Exact Second Tests" $
  mapM_ (analyzedFirstTest context testOptions . withTargets [Seal Time]) xs
  where
    context = testContext {referenceTime = refTime (2016, 12, 6, 13, 21, 42) 1}
    xs = concat
      [ examples (datetime (2016, 12, 6, 13, 21, 45) Second)
                 [ "in 3 seconds"
                 ]
      , examples (datetime (2016, 12, 6, 13, 31, 42) Second)
                 [ "in ten minutes"
                 ]
      , examples (datetimeInterval
          ((2016, 12, 6, 13, 21, 42), (2016, 12, 12, 0, 0, 0)) Second)
                 [ "by next week"
                 , "by Monday"
                 ]
      ]

valuesTest :: TestTree
valuesTest = testCase "Values Test" $
  mapM_ (analyzedFirstTest testContext testOptions . withTargets [Seal Time]) xs
  where
    xs = examplesCustom (parserCheck 1 parseValuesSize)
                        [ "now"
                        , "8 o'clock tonight"
                        , "yesterday"
                        ]
    parseValuesSize :: Value -> Maybe Int
    parseValuesSize x = length <$> parseValues x
    parseValues :: Value -> Maybe [Object]
    parseValues = parseMaybe $ withObject "value object" (.: "values")

intersectTests :: TestTree
intersectTests = testCase "Intersect Test" $
  mapM_ (analyzedNTest testContext testOptions . withTargets [Seal Time]) xs
  where
    xs = [ ("tomorrow July", 2)
         , ("Mar tonight", 2)
         , ("Feb tomorrow", 1) -- we are in February
         ]

rangeTests :: TestTree
rangeTests = testCase "Range Test" $
  mapM_ (analyzedRangeTest testContext testOptions . withTargets [Seal Time]) xs
  where
    xs = [ ("at 615.", Range 0 6) -- make sure ruleHHMMLatent allows this
         , ("last in 2'", Range 5 10) -- ruleLastTime too eager
         , ("this in 2'", Range 5 10) -- ruleThisTime too eager
         , ("next in 2'", Range 5 10) -- ruleNextTime too eager
         , ("this this week", Range 5 14) -- ruleThisTime too eager
         , ("one ninety nine a m", Range 11 19) -- ruleMilitarySpelledOutAMPM2
         , ("thirteen fifty nine a m", Range 15 23) -- ruleMilitarySpelledOutAMPM
         , ("table Wednesday for 30 people", Range 6 15)
           -- do not parse "for 30" as year intersect
         , ("house 1 on december 2013", Range 11 24) -- ruleAbsorbOnDay
         , ("at 6pm GMT PDT", Range 0 10) -- ruleTimezone
         , ("at 6pm (PDT) GMT", Range 0 12) -- ruleTimezoneBracket
         , ("6pm GMT - 8pm GMT PDT", Range 0 17)
           -- ruleTimezone will not match because TimeData hasTimezone.
         , ("Monday - 1", Range 0 6) -- ruleYearLatent to not accept negative years.
         ]
