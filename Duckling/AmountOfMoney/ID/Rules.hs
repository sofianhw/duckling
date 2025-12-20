-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.AmountOfMoney.ID.Rules
  ( rules
  ) where

import Data.Maybe
import Data.String
import Prelude
import qualified Data.Text as Text

import Duckling.AmountOfMoney.Helpers
import Duckling.AmountOfMoney.Types (Currency(..), AmountOfMoneyData(..))
import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (isNatural, isPositive, parseDouble)
import Duckling.Numeral.Types (NumeralData (..))
import Duckling.Regex.Types
import Duckling.Types
import qualified Duckling.AmountOfMoney.Types as TAmountOfMoney
import qualified Duckling.Numeral.Types as TNumeral

ruleUnitAmount :: Rule
ruleUnitAmount = Rule
  { name = "<unit> <amount>"
  , pattern =
    [ Predicate isCurrencyOnly
    , Predicate isPositive
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.currency = c}:
       Token Numeral NumeralData{TNumeral.value = v}:
       _) -> Just . Token AmountOfMoney . withValue v $ currencyOnly c
      _ -> Nothing
  }

ruleDollar :: Rule
ruleDollar = Rule
  { name = "$"
  , pattern =
    [ regex "dolar?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Dollar
  }

ruleIdr :: Rule
ruleIdr = Rule
  { name = "IDR"
  , pattern =
    [ regex "(?i)rp\\.?|rupiah"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly IDR
  }

rulePounds :: Rule
rulePounds = Rule
  { name = "£"
  , pattern =
    [ regex "pound( sterling)?"
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly Pound
  }

ruleIntersect :: Rule
ruleIntersect = Rule
  { name = "intersect"
  , pattern =
    [ Predicate isWithoutCents
    , Predicate isNatural
    ]
  , prod = \tokens -> case tokens of
      (Token AmountOfMoney fd:
       Token Numeral NumeralData{TNumeral.value = c}:
       _) -> Just . Token AmountOfMoney $ withCents c fd
      _ -> Nothing
  }

ruleJpy :: Rule
ruleJpy = Rule
  { name = "JPY"
  , pattern =
    [ regex "¥\\."
    ]
  , prod = \_ -> Just . Token AmountOfMoney $ currencyOnly JPY
  }

rulePrecision :: Rule
rulePrecision = Rule
  { name = "about|exactly <amount-of-money>"
  , pattern =
    [ regex "tepatnya|kira-kira|sekitar|hampir"
    , Predicate isMoneyWithValue
    ]
  , prod = \case
      (_:token:_) -> Just token
      _ -> Nothing
  }

ruleIntervalBetweenNumeral :: Rule
ruleIntervalBetweenNumeral = Rule
  { name = "between|from <numeral> to|and <amount-of-money>"
  , pattern =
    [ regex "sekitar|dari|antara"
    , Predicate isPositive
    , regex "sampai|hingga"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (_:
       Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) | from < to ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalBetween :: Rule
ruleIntervalBetween = Rule
  { name = "between|from <amount-of-money> to|and <amount-of-money>"
  , pattern =
    [ regex "sekitar|dari|antara|kira-kira"
    , Predicate isSimpleAmountOfMoney
    , regex "sampai|hingga"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just from,
                  TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c2}:
       _) | from < to && c1 == c2 ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalNumeralDash :: Rule
ruleIntervalNumeralDash = Rule
  { name = "<numeral> - <amount-of-money>"
  , pattern =
    [ Predicate isNatural
    , regex "\\-"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = from}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) | from < to ->
         Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalDash :: Rule
ruleIntervalDash = Rule
  { name = "<amount-of-money> - <amount-of-money>"
  , pattern =
    [ Predicate isSimpleAmountOfMoney
    , regex "\\-"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just from,
                  TAmountOfMoney.currency = c1}:
       _:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c2}:
       _) | from < to && c1 == c2 ->
        Just . Token AmountOfMoney . withInterval (from, to) $ currencyOnly c1
      _ -> Nothing
  }

ruleIntervalMax :: Rule
ruleIntervalMax = Rule
  { name = "under/less/lower/no more than <amount-of-money>"
  , pattern =
    [ regex "(kurang|lebih murah) dari|tidak sampai|di bawah"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMax to $ currencyOnly c
      _ -> Nothing
  }

ruleIntervalMin :: Rule
ruleIntervalMin = Rule
  { name = "over/above/at least/more than <amount-of-money>"
  , pattern =
    [ regex "di atas|lebih dari|melebihi|melewati"
    , Predicate isSimpleAmountOfMoney
    ]
  , prod = \case
      (_:
       Token AmountOfMoney AmountOfMoneyData{TAmountOfMoney.value = Just to,
                  TAmountOfMoney.currency = c}:
       _) -> Just . Token AmountOfMoney . withMin to $ currencyOnly c
      _ -> Nothing
  }

-- "rb", "ribu" = thousand (multiply by 1000)
-- Handles both "100 rb" (with space) and "100rb" (attached)
ruleRibu :: Rule
ruleRibu = Rule
  { name = "<numeral> ribu/rb"
  , pattern =
    [ Predicate isPositive
    , regex "(?i)(rb|ribu)"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token AmountOfMoney . withValue (1000 * v) $ currencyOnly IDR
      _ -> Nothing
  }

-- Handle "100rb" as single token (number attached to abbreviation)
ruleRibuAttached :: Rule
ruleRibuAttached = Rule
  { name = "<number>rb (attached)"
  , pattern =
    [ regex "(\\d+)(?i)(rb|ribu)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (num:abbr:_)):_) -> do
        v <- parseDouble num
        Just . Token AmountOfMoney . withValue (1000 * v) $ currencyOnly IDR
      _ -> Nothing
  }

-- "jt", "juta" = million (multiply by 1,000,000)
ruleJuta :: Rule
ruleJuta = Rule
  { name = "<numeral> juta/jt"
  , pattern =
    [ Predicate isPositive
    , regex "(?i)(jt|juta)"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token AmountOfMoney . withValue (1000000 * v) $ currencyOnly IDR
      _ -> Nothing
  }

-- Handle "100jt" as single token (number attached to abbreviation)
ruleJutaAttached :: Rule
ruleJutaAttached = Rule
  { name = "<number>jt (attached)"
  , pattern =
    [ regex "(\\d+)(?i)(jt|juta)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (num:abbr:_)):_) -> do
        v <- parseDouble num
        Just . Token AmountOfMoney . withValue (1000000 * v) $ currencyOnly IDR
      _ -> Nothing
  }

-- "milyar" = billion (multiply by 1,000,000,000)
ruleMilyar :: Rule
ruleMilyar = Rule
  { name = "<numeral> milyar"
  , pattern =
    [ Predicate isPositive
    , regex "(?i)milyar"
    ]
  , prod = \case
      (Token Numeral NumeralData{TNumeral.value = v}:_) ->
        Just . Token AmountOfMoney . withValue (1000000000 * v) $ currencyOnly IDR
      _ -> Nothing
  }

-- Handle "100milyar" as single token (number attached to abbreviation)
ruleMilyarAttached :: Rule
ruleMilyarAttached = Rule
  { name = "<number>milyar (attached)"
  , pattern =
    [ regex "(\\d+)(?i)milyar"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (num:_)):_) -> do
        v <- parseDouble num
        Just . Token AmountOfMoney . withValue (1000000000 * v) $ currencyOnly IDR
      _ -> Nothing
  }

-- Handle "100.000" as amount of money (Indonesian number format with dots as thousand separators)
-- This pattern should come before other rules to prioritize amount-of-money over phone-number
ruleNumberWithDots :: Rule
ruleNumberWithDots = Rule
  { name = "<number with dots> (as amount of money)"
  , pattern =
    [ regex "(\\d{1,3}(?:\\.\\d{3})+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        -- Remove dots and parse as number (Indonesian uses dots as thousand separators)
        let cleaned = Text.replace (Text.pack ".") Text.empty match
        v <- parseDouble cleaned
        Just . Token AmountOfMoney . mkLatent $ valueOnly v
      _ -> Nothing
  }

rules :: [Rule]
rules =
  [ ruleNumberWithDots      -- Must come early to catch "100.000" before phone-number
  , ruleRibuAttached         -- Must come before ruleRibu to catch "100rb" (attached)
  , ruleJutaAttached         -- Must come before ruleJuta to catch "100jt" (attached)
  , ruleMilyarAttached       -- Must come before ruleMilyar to catch "100milyar" (attached)
  , ruleRibu                 -- Handles "100 rb" (with space)
  , ruleJuta                 -- Handles "100 juta" (with space)
  , ruleMilyar               -- Handles "100 milyar" (with space)
  , ruleUnitAmount
  , ruleDollar
  , ruleIdr
  , ruleIntersect
  , ruleJpy
  , rulePounds
  , rulePrecision
  , ruleIntervalBetweenNumeral
  , ruleIntervalBetween
  , ruleIntervalMax
  , ruleIntervalMin
  , ruleIntervalNumeralDash
  , ruleIntervalDash
  ]
