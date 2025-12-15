-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}


module Duckling.Rules.EN
  ( defaultRules
  , langRules
  , localeRules
  ) where

import Prelude

import Duckling.Dimensions.Types
import Duckling.Locale
import Duckling.Types
import qualified Duckling.AmountOfMoney.EN.Rules as AmountOfMoney
import qualified Duckling.AmountOfMoney.EN.US.Rules as AmountOfMoneyUS
import qualified Duckling.Distance.EN.Rules as Distance
import qualified Duckling.Duration.EN.Rules as Duration
import qualified Duckling.Email.EN.Rules as Email
import qualified Duckling.Numeral.EN.Rules as Numeral
import qualified Duckling.Ordinal.EN.Rules as Ordinal
import qualified Duckling.Quantity.EN.Rules as Quantity
import qualified Duckling.Region as R
import qualified Duckling.Temperature.EN.Rules as Temperature
import qualified Duckling.Time.EN.Rules as Time
import qualified Duckling.Time.EN.US.Rules as TimeUS
import qualified Duckling.TimeGrain.EN.Rules as TimeGrain
import qualified Duckling.Volume.EN.Rules as Volume

defaultRules :: Seal Dimension -> [Rule]
defaultRules dim@(Seal Time) = TimeUS.rulesBackwardCompatible ++ langRules dim
defaultRules dim             = langRules dim

localeRules :: Region -> Seal Dimension -> [Rule]
localeRules US (Seal AmountOfMoney) = AmountOfMoneyUS.rules
localeRules US (Seal Time) = TimeUS.rules
localeRules region (Seal (CustomDimension dim)) = dimLocaleRules region dim
localeRules _ _ = []

langRules :: Seal Dimension -> [Rule]
langRules (Seal AmountOfMoney) = AmountOfMoney.rules
langRules (Seal CreditCardNumber) = []
langRules (Seal Distance) = Distance.rules
langRules (Seal Duration) = Duration.rules
langRules (Seal Email) = Email.rules
langRules (Seal Numeral) = Numeral.rules
langRules (Seal Ordinal) = Ordinal.rules
langRules (Seal PhoneNumber) = []
langRules (Seal Quantity) = Quantity.rules
langRules (Seal RegexMatch) = []
langRules (Seal Temperature) = Temperature.rules
langRules (Seal Time) = Time.rules
langRules (Seal TimeGrain) = TimeGrain.rules
langRules (Seal Url) = []
langRules (Seal Volume) = Volume.rules
langRules (Seal (CustomDimension dim)) = dimLangRules EN dim
