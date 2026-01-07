-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.ID.Rules
  ( rules
  ) where

import Prelude
import Data.Maybe
import qualified Data.Text as Text

import Duckling.Dimensions.Types
import Duckling.Numeral.Helpers (parseInt)
import Duckling.Regex.Types
import Duckling.Time.Computed
import Duckling.Time.Helpers
import Duckling.Time.Types (TimeData(..))
import Duckling.Types
import qualified Duckling.Numeral.Types as TNumeral
import qualified Duckling.Time.Types as TTime
import qualified Duckling.TimeGrain.Types as TG

-----------------------------------------------------------------
-- Util: bulan Indonesia
-----------------------------------------------------------------

parseMonthID :: Text.Text -> Maybe Int
parseMonthID raw =
  case Text.toLower raw of
    "januari" -> Just 1
    "jan"     -> Just 1
    "februari" -> Just 2
    "feb"      -> Just 2
    "maret"    -> Just 3
    "mar"      -> Just 3
    "april"    -> Just 4
    "apr"      -> Just 4
    "mei"      -> Just 5
    "jun"      -> Just 6
    "juni"     -> Just 6
    "jul"      -> Just 7
    "juli"     -> Just 7
    "agustus"  -> Just 8
    "agu"      -> Just 8
    "ags"      -> Just 8
    "sep"      -> Just 9
    "september"-> Just 9
    "oktober"  -> Just 10
    "okt"      -> Just 10
    "november" -> Just 11
    "nov"      -> Just 11
    "nopember" -> Just 11
    "desember" -> Just 12
    "des"      -> Just 12
    _          -> Nothing

-----------------------------------------------------------------
-- Aturan tanggal numerik: dd/mm, dd-mm, dd.mm, ISO
-----------------------------------------------------------------

-- dd/mm atau d/m
ruleDDMM :: Rule
ruleDDMM = Rule
  { name = "tanggal dd/mm atau d/m"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\s*/\\s*(1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:_)) : _) -> do
        d <- parseInt dd
        m <- parseInt mm
        tt $ monthDay m d
      _ -> Nothing
  }

-- dd-mm atau d-m
ruleDDMMDash :: Rule
ruleDDMMDash = Rule
  { name = "tanggal dd-mm atau d-m"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\s*-\\s*(1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:_)) : _) -> do
        d <- parseInt dd
        m <- parseInt mm
        tt $ monthDay m d
      _ -> Nothing
  }

-- dd.mm atau d.m
ruleDDMMDot :: Rule
ruleDDMMDot = Rule
  { name = "tanggal dd.mm atau d.m"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\s*\\.\\s*(1[0-2]|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:_)) : _) -> do
        d <- parseInt dd
        m <- parseInt mm
        tt $ monthDay m d
      _ -> Nothing
  }

-- dd/mm/yyyy atau dd/mm/yy
ruleDDMMYYYY :: Rule
ruleDDMMYYYY = Rule
  { name = "tanggal dd/mm/yyyy atau dd/mm/yy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\s*/\\s*(1[0-2]|0?[1-9])\\s*/\\s*(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)) : _) -> do
        d <- parseInt dd
        m <- parseInt mm
        y <- parseInt yy
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

-- dd-mm-yyyy atau dd-mm-yy
ruleDDMMYYYYDash :: Rule
ruleDDMMYYYYDash = Rule
  { name = "tanggal dd-mm-yyyy atau dd-mm-yy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\s*-\\s*(1[0-2]|0?[1-9])\\s*-\\s*(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)) : _) -> do
        d <- parseInt dd
        m <- parseInt mm
        y <- parseInt yy
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

-- dd.mm.yyyy atau dd.mm.yy
ruleDDMMYYYYDot :: Rule
ruleDDMMYYYYDot = Rule
  { name = "tanggal dd.mm.yyyy atau dd.mm.yy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\s*\\.\\s*(1[0-2]|0?[1-9])\\s*\\.\\s*(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mm:yy:_)) : _) -> do
        d <- parseInt dd
        m <- parseInt mm
        y <- parseInt yy
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

-- ISO 8601: yyyy-mm-dd
ruleYYYYMMDDISO :: Rule
ruleYYYYMMDDISO = Rule
  { name = "tanggal ISO yyyy-mm-dd"
  , pattern =
    [ regex "(\\d{4})-(1[0-2]|0?[1-9])-(3[01]|[12]\\d|0?[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (yy:mm:dd:_)) : _) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

-- yyyymmdd (without separators)
ruleYYYYMMDDNoSep :: Rule
ruleYYYYMMDDNoSep = Rule
  { name = "tanggal yyyymmdd"
  , pattern =
    [ regex "(\\d{4})(1[0-2]|0[1-9])(3[01]|[12][0-9]|0[1-9])"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (yy:mm:dd:_)) : _) -> do
        y <- parseInt yy
        m <- parseInt mm
        d <- parseInt dd
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

-----------------------------------------------------------------
-- Tanggal dengan nama bulan: "25 Desember 2025", "1 Jan 25"
-----------------------------------------------------------------

-- dd <bulan> yyyy / yy
ruleDDMonthYYYY :: Rule
ruleDDMonthYYYY = Rule
  { name = "tanggal dd bulan yyyy"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\s+([[:alpha:]\\.]+)\\s+(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mmText:yy:_)) : _) -> do
        d <- parseInt dd
        y <- parseInt yy
        m <- parseMonthID mmText
        tt $ yearMonthDay y m d
      _ -> Nothing
  }

-- dd <bulan> (tanpa tahun) -> tahun ini
ruleDDMonth :: Rule
ruleDDMonth = Rule
  { name = "tanggal dd bulan (tahun ini)"
  , pattern =
    [ regex "(3[01]|[12]\\d|0?[1-9])\\s+([[:alpha:]\\.]+)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (dd:mmText:_)) : _) -> do
        d <- parseInt dd
        m <- parseMonthID mmText
        tt $ monthDay m d
      _ -> Nothing
  }

-- <bulan> <tahun> seperti "Jan 2025", "Desember 2025"
ruleMonthYear :: Rule
ruleMonthYear = Rule
  { name = "bulan tahun"
  , pattern =
    [ regex "([[:alpha:]\\.]+)\\s+(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (mmText:yy:_)) : _) -> do
        m <- parseMonthID mmText
        y <- parseInt yy
        tt $ yearMonth y m
      _ -> Nothing
  }

-----------------------------------------------------------------
-- Hari dalam seminggu (Indonesia)
-----------------------------------------------------------------

ruleMonday :: Rule
ruleMonday = Rule
  { name = "Senin"
  , pattern =
    [ regex "senin|sen\\.?"
    ]
  , prod = \_ -> tt $ dayOfWeek 1
  }

ruleTuesday :: Rule
ruleTuesday = Rule
  { name = "Selasa"
  , pattern =
    [ regex "selasa|sel\\.?"
    ]
  , prod = \_ -> tt $ dayOfWeek 2
  }

ruleWednesday :: Rule
ruleWednesday = Rule
  { name = "Rabu"
  , pattern =
    [ regex "rabu|rab\\.?"
    ]
  , prod = \_ -> tt $ dayOfWeek 3
  }

ruleThursday :: Rule
ruleThursday = Rule
  { name = "Kamis"
  , pattern =
    [ regex "kamis|kam\\.?"
    ]
  , prod = \_ -> tt $ dayOfWeek 4
  }

ruleFriday :: Rule
ruleFriday = Rule
  { name = "Jumat"
  , pattern =
    [ regex "jum('?a|a)?t|jum\\.?"
    ]
  , prod = \_ -> tt $ dayOfWeek 5
  }

ruleSaturday :: Rule
ruleSaturday = Rule
  { name = "Sabtu"
  , pattern =
    [ regex "sabtu|sab\\.?"
    ]
  , prod = \_ -> tt $ dayOfWeek 6
  }

ruleSunday :: Rule
ruleSunday = Rule
  { name = "Minggu"
  , pattern =
    [ regex "\\b(ahad|min\\.?)\\b"
    ]
  , prod = \_ -> tt $ dayOfWeek 7
  }

-----------------------------------------------------------------
-- Ekspresi relatif: hari ini, besok, lusa, dll.
-----------------------------------------------------------------

ruleHariIni :: Rule
ruleHariIni = Rule
  { name = "hari ini"
  , pattern =
    [ regex "hari ini"
    ]
  , prod = \_ -> tt today
  }

ruleSekarang :: Rule
ruleSekarang = Rule
  { name = "sekarang"
  , pattern =
    [ regex "skrg|sekarang"
    ]
  , prod = \_ -> tt now
  }

ruleBesok :: Rule
ruleBesok = Rule
  { name = "besok"
  , pattern =
    [ regex "besok"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 1
  }

ruleLusa :: Rule
ruleLusa = Rule
  { name = "lusa"
  , pattern =
    [ regex "lusa"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day 2
  }

ruleKemarin :: Rule
ruleKemarin = Rule
  { name = "kemarin"
  , pattern =
    [ regex "kemarin"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day (-1)
  }

ruleKemarinLusa :: Rule
ruleKemarinLusa = Rule
  { name = "kemarin lusa"
  , pattern =
    [ regex "kemarin lusa"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day (-2)
  }

-- "N minggu lalu" atau "N minggu yang lalu" (N weeks ago)
-- This must come before ruleMingguLalu to match "2 minggu lalu" correctly
ruleNumeralMingguLalu :: Rule
ruleNumeralMingguLalu = Rule
  { name = "<numeral> minggu lalu"
  , pattern =
    [ dimension Numeral
    , regex "minggu"
    , regex "(lalu|yang lalu|kemaren|kemarin)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral TNumeral.NumeralData{TNumeral.value = v}:_:_) ->
        let weeks = floor v
            days = weeks * 7
        in tt $ cycleNth TG.Day (-days)
      _ -> Nothing
  }

-- "N minggu depan" atau "N minggu kemudian" (N weeks from now)
ruleNumeralMingguDepan :: Rule
ruleNumeralMingguDepan = Rule
  { name = "<numeral> minggu depan"
  , pattern =
    [ dimension Numeral
    , regex "minggu"
    , regex "(depan|kemudian|mendatang)"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral TNumeral.NumeralData{TNumeral.value = v}:_:_) ->
        let weeks = floor v
            days = weeks * 7
        in tt $ cycleNth TG.Day days
      _ -> Nothing
  }

-- "minggu depan" - returns full week interval
ruleMingguDepan :: Rule
ruleMingguDepan = Rule
  { name = "minggu depan"
  , pattern =
    [ regex "minggu"
    , regex "depan"
    ]
  , prod = \_ ->
      let end = cycleNthAfter True TG.Day (-1) $ cycleNth TG.Week 2
      in Token Time <$> interval TTime.Closed (cycleNth TG.Week 1) end
  }

ruleMingguKemudian :: Rule
ruleMingguKemudian = Rule
  { name = "minggu kemudian"
  , pattern =
    [ regex "minggu"
    , regex "kemudian"
    ]
  , prod = \_ ->
      let end = cycleNthAfter True TG.Day (-1) $ cycleNth TG.Week 2
      in Token Time <$> interval TTime.Closed (cycleNth TG.Week 1) end
  }

-- "minggu lalu" - returns full week interval
ruleMingguLalu :: Rule
ruleMingguLalu = Rule
  { name = "minggu lalu"
  , pattern =
    [ regex "minggu"
    , regex "lalu"
    ]
  , prod = \_ ->
      let end = cycleNthAfter True TG.Day (-1) $ cycleNth TG.Week 0
      in Token Time <$> interval TTime.Closed (cycleNth TG.Week (-1)) end
  }

ruleMingguKemarenKemarin :: Rule
ruleMingguKemarenKemarin = Rule
  { name = "minggu kemaren|kemarin"
  , pattern =
    [ regex "minggu"
    , regex "kemaren|kemarin"
    ]
  , prod = \_ -> tt $ cycleNth TG.Day (-7)
  }

-- "minggu ini" (this week) - returns date range
ruleMingguIni :: Rule
ruleMingguIni = Rule
  { name = "minggu ini"
  , pattern =
    [ regex "minggu"
    , regex "ini"
    ]
  , prod = \_ ->
      let end = cycleNthAfter True TG.Day (-2) $ cycleNth TG.Week 1
      in Token Time <$> interval TTime.Closed (cycleNth TG.Week 0) end
  }

ruleBulanDepan :: Rule
ruleBulanDepan = Rule
  { name = "bulan depan"
  , pattern =
    [ regex "bulan depan"
    ]
  , prod = \_ -> do
      -- Return full month interval for next month
      let nextMonth = cycleNth TG.Month 1
      start <- intersect (dayOfMonth 1) nextMonth
      end <- Just $ cycleNthAfter True TG.Day (-1) $ cycleNth TG.Month 2
      Token Time <$> interval TTime.Closed start end
  }

ruleBulanLalu :: Rule
ruleBulanLalu = Rule
  { name = "bulan lalu"
  , pattern =
    [ regex "bulan lalu"
    ]
  , prod = \_ -> do
      -- Return full month interval for last month
      let lastMonth = cycleNth TG.Month (-1)
      start <- intersect (dayOfMonth 1) lastMonth
      end <- Just $ cycleNthAfter True TG.Day (-1) $ cycleNth TG.Month 0
      Token Time <$> interval TTime.Closed start end
  }

-- "bulan ini" (this month) - returns date range
ruleBulanIni :: Rule
ruleBulanIni = Rule
  { name = "bulan ini"
  , pattern =
    [ regex "bulan"
    , regex "ini"
    ]
  , prod = \_ ->
      let end = cycleNthAfter True TG.Day (-1) $ cycleNth TG.Month 1
      in Token Time <$> interval TTime.Closed (cycleNth TG.Month 0) end
  }

-- "tahun lalu" (last year) - returns full year interval
ruleTahunLalu :: Rule
ruleTahunLalu = Rule
  { name = "tahun lalu"
  , pattern =
    [ regex "tahun"
    , regex "lalu"
    ]
  , prod = \_ -> do
      -- Get last year
      let lastYear = cycleNth TG.Year (-1)
      -- Start: January 1 of last year (need to do in two steps since intersect returns Maybe)
      janLastYear <- intersect (month 1) lastYear
      start <- intersect (dayOfMonth 1) janLastYear
      -- End: December 31 of last year (last day of last year)
      end <- Just $ cycleLastOf TG.Day lastYear
      Token Time <$> interval TTime.Closed start end
  }

-- "tahun depan" (next year) - returns full year interval
ruleTahunDepan :: Rule
ruleTahunDepan = Rule
  { name = "tahun depan"
  , pattern =
    [ regex "tahun"
    , regex "depan"
    ]
  , prod = \_ -> do
      -- Get next year
      let nextYear = cycleNth TG.Year 1
      -- Start: January 1 of next year (need to do in two steps since intersect returns Maybe)
      janNextYear <- intersect (month 1) nextYear
      start <- intersect (dayOfMonth 1) janNextYear
      -- End: December 31 of next year (last day of next year)
      end <- Just $ cycleLastOf TG.Day nextYear
      Token Time <$> interval TTime.Closed start end
  }

-- "akhir minggu" (weekend)
ruleAkhirMinggu :: Rule
ruleAkhirMinggu = Rule
  { name = "akhir minggu"
  , pattern =
    [ regex "akhir\\s+minggu"
    ]
  , prod = \_ -> tt $ mkOkForThisNext weekend
  }

-- "awal bulan" (beginning of month) - returns date range
ruleAwalBulan :: Rule
ruleAwalBulan = Rule
  { name = "awal bulan"
  , pattern =
    [ regex "awal|permulaan"
    , regex "bulan"
    ]
  , prod = \_ -> do
      start <- intersect (dayOfMonth 1) $ cycleNth TG.Month 0
      end <- intersect (dayOfMonth 10) $ cycleNth TG.Month 0
      Token Time <$> interval TTime.Open start end
  }

-- "bulan ini sampai sekarang" atau "dari awal bulan sampai sekarang" (month to date)
-- Returns interval from first day of current month until now
ruleBulanIniSampaiSekarang :: Rule
ruleBulanIniSampaiSekarang = Rule
  { name = "bulan ini sampai sekarang"
  , pattern =
    [ regex "bulan"
    , regex "ini"
    , regex "sampai( dengan)?|hingga"
    , regex "sekarang|hari ini"
    ]
  , prod = \_ -> do
      start <- intersect (dayOfMonth 1) $ cycleNth TG.Month 0
      Token Time <$> interval TTime.Closed start now
  }

-- "dari awal bulan sampai sekarang" (from beginning of month until now)
ruleDariAwalBulanSampaiSekarang :: Rule
ruleDariAwalBulanSampaiSekarang = Rule
  { name = "dari awal bulan sampai sekarang"
  , pattern =
    [ regex "dari"
    , regex "awal|permulaan"
    , regex "bulan"
    , regex "sampai( dengan)?|hingga"
    , regex "sekarang|hari ini"
    ]
  , prod = \_ -> do
      start <- intersect (dayOfMonth 1) $ cycleNth TG.Month 0
      Token Time <$> interval TTime.Closed start now
  }

-- "sejak awal bulan" (since beginning of month)
ruleSejakAwalBulan :: Rule
ruleSejakAwalBulan = Rule
  { name = "sejak awal bulan"
  , pattern =
    [ regex "sejak"
    , regex "awal|permulaan"
    , regex "bulan"
    ]
  , prod = \_ -> do
      start <- intersect (dayOfMonth 1) $ cycleNth TG.Month 0
      Token Time <$> interval TTime.Closed start now
  }

-- "sejak bulan lalu" (since last month) - from first day of last month until now
-- Example: "sejak bulan lalu"
ruleSejakBulanLalu :: Rule
ruleSejakBulanLalu = Rule
  { name = "sejak bulan lalu"
  , pattern =
    [ regex "sejak"
    , regex "bulan"
    , regex "lalu"
    ]
  , prod = \_ -> do
      -- Start: first day of last month
      let lastMonth = cycleNth TG.Month (-1)
      start <- intersect (dayOfMonth 1) lastMonth
      -- End: now (today)
      Token Time <$> interval TTime.Closed start now
  }

-- "sejak minggu lalu" (since last week) - from Monday of last week until now
-- Example: "sejak minggu lalu"
ruleSejakMingguLalu :: Rule
ruleSejakMingguLalu = Rule
  { name = "sejak minggu lalu"
  , pattern =
    [ regex "sejak"
    , regex "minggu"
    , regex "lalu"
    ]
  , prod = \_ -> do
      -- Start: Monday of last week
      let lastWeek = cycleNth TG.Week (-1)
      -- End: now (today)
      Token Time <$> interval TTime.Closed lastWeek now
  }

-- "sejak minggu ini" (since this week) - from Monday of this week until now
-- Example: "sejak minggu ini"
ruleSejakMingguIni :: Rule
ruleSejakMingguIni = Rule
  { name = "sejak minggu ini"
  , pattern =
    [ regex "sejak"
    , regex "minggu"
    , regex "ini"
    ]
  , prod = \_ -> do
      -- Start: Monday of this week
      let thisWeek = cycleNth TG.Week 0
      -- End: now (today)
      Token Time <$> interval TTime.Closed thisWeek now
  }

-- "sejak tahun lalu" (since last year) - from January 1 of last year until now
-- Example: "sejak tahun lalu"
ruleSejakTahunLalu :: Rule
ruleSejakTahunLalu = Rule
  { name = "sejak tahun lalu"
  , pattern =
    [ regex "sejak"
    , regex "tahun"
    , regex "lalu"
    ]
  , prod = \_ -> do
      -- Start: January 1 of last year (need to do in two steps since intersect returns Maybe)
      let lastYear = cycleNth TG.Year (-1)
      janLastYear <- intersect (month 1) lastYear
      start <- intersect (dayOfMonth 1) janLastYear
      -- End: now (today)
      Token Time <$> interval TTime.Closed start now
  }

-- "selama bulan <month> <year>" (during the month of <month> in <year>) - returns full month interval
-- Example: "selama bulan desember 2025"
ruleSelamaBulanTahun :: Rule
ruleSelamaBulanTahun = Rule
  { name = "selama bulan <month> <year>"
  , pattern =
    [ regex "selama"
    , regex "bulan"
    , regex "([[:alpha:]\\.]+)"
    , regex "(\\d{2,4})"
    ]
  , prod = \tokens -> case tokens of
      (_:_:Token RegexMatch (GroupMatch (mmText:_)):Token RegexMatch (GroupMatch (yy:_)):_) -> do
        m <- parseMonthID mmText
        y <- parseInt yy
        start <- intersect (dayOfMonth 1) $ yearMonth y m
        end <- Just $ cycleLastOf TG.Day $ yearMonth y m
        Token Time <$> interval TTime.Closed start end
      _ -> Nothing
  }

-- "selama bulan <month>" (during the month of <month>) - returns full month interval
-- Example: "selama bulan desember"
ruleSelamaBulan :: Rule
ruleSelamaBulan = Rule
  { name = "selama bulan <month>"
  , pattern =
    [ regex "selama"
    , regex "bulan"
    , regex "([[:alpha:]\\.]+)"
    ]
  , prod = \tokens -> case tokens of
      (_:_:Token RegexMatch (GroupMatch (mmText:_)):_) -> do
        m <- parseMonthID mmText
        -- Use month predicate which defaults to current year
        start <- intersect (dayOfMonth 1) $ month m
        end <- Just $ cycleLastOf TG.Day $ month m
        Token Time <$> interval TTime.Closed start end
      _ -> Nothing
  }

-- "selama minggu ini" (during this week) - returns full week interval
-- Example: "selama minggu ini"
ruleSelamaMingguIni :: Rule
ruleSelamaMingguIni = Rule
  { name = "selama minggu ini"
  , pattern =
    [ regex "selama"
    , regex "minggu"
    , regex "ini"
    ]
  , prod = \_ ->
      -- Full week: Monday of this week to Sunday of this week
      -- Start: Monday of this week (cycleNth TG.Week 0)
      -- End: Sunday of this week (day before Monday of next week)
      let end = cycleNthAfter True TG.Day (-1) $ cycleNth TG.Week 1
      in Token Time <$> interval TTime.Closed (cycleNth TG.Week 0) end
  }

-- "selama minggu depan" (during next week) - returns full week interval
-- Example: "selama minggu depan"
ruleSelamaMingguDepan :: Rule
ruleSelamaMingguDepan = Rule
  { name = "selama minggu depan"
  , pattern =
    [ regex "selama"
    , regex "minggu"
    , regex "depan"
    ]
  , prod = \_ ->
      -- Full week: Monday of next week to Sunday of next week
      -- Start: Monday of next week (cycleNth TG.Week 1)
      -- End: Sunday of next week (day before Monday of week after next)
      let end = cycleNthAfter True TG.Day (-1) $ cycleNth TG.Week 2
      in Token Time <$> interval TTime.Closed (cycleNth TG.Week 1) end
  }

-- "selama minggu lalu" (during last week) - returns full week interval
-- Example: "selama minggu lalu"
ruleSelamaMingguLalu :: Rule
ruleSelamaMingguLalu = Rule
  { name = "selama minggu lalu"
  , pattern =
    [ regex "selama"
    , regex "minggu"
    , regex "lalu"
    ]
  , prod = \_ ->
      -- Full week: Monday of last week to Sunday of last week
      -- Start: Monday of last week (cycleNth TG.Week (-1))
      -- End: Sunday of last week (day before Monday of this week)
      let end = cycleNthAfter True TG.Day (-1) $ cycleNth TG.Week 0
      in Token Time <$> interval TTime.Closed (cycleNth TG.Week (-1)) end
  }

-- "selama bulan lalu" (during last month) - returns full month interval
-- Example: "selama bulan lalu"
ruleSelamaBulanLalu :: Rule
ruleSelamaBulanLalu = Rule
  { name = "selama bulan lalu"
  , pattern =
    [ regex "selama"
    , regex "bulan"
    , regex "lalu"
    ]
  , prod = \_ -> do
      -- Get last month
      let lastMonth = cycleNth TG.Month (-1)
      -- Start: first day of last month
      start <- intersect (dayOfMonth 1) lastMonth
      -- End: last day of last month (start of current month minus 1 day)
      end <- Just $ cycleNthAfter True TG.Day (-1) $ cycleNth TG.Month 0
      Token Time <$> interval TTime.Closed start end
  }

-- "selama bulan depan" (during next month) - returns full month interval
-- Example: "selama bulan depan"
ruleSelamaBulanDepan :: Rule
ruleSelamaBulanDepan = Rule
  { name = "selama bulan depan"
  , pattern =
    [ regex "selama"
    , regex "bulan"
    , regex "depan"
    ]
  , prod = \_ -> do
      -- Get next month
      let nextMonth = cycleNth TG.Month 1
      -- Start: first day of next month
      start <- intersect (dayOfMonth 1) nextMonth
      -- End: last day of next month (start of month after next minus 1 day)
      end <- Just $ cycleNthAfter True TG.Day (-1) $ cycleNth TG.Month 2
      Token Time <$> interval TTime.Closed start end
  }

-----------------------------------------------------------------
-- Ekspresi durasi: "dalam 2 hari", "2 hari kemudian", "2 hari yang lalu"
-----------------------------------------------------------------

-- "dalam X" atau "dalam X hari/jam/menit" (in X)
ruleDalamDuration :: Rule
ruleDalamDuration = Rule
  { name = "dalam <duration>"
  , pattern =
    [ regex "dalam"
    , dimension Duration
    ]
  , prod = \tokens -> case tokens of
      (_:Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

-- "X kemudian" atau "X hari/jam/menit kemudian" (X later)
ruleDurationKemudian :: Rule
ruleDurationKemudian = Rule
  { name = "<duration> kemudian"
  , pattern =
    [ dimension Duration
    , regex "kemudian|lagi|mendatang"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ inDuration dd
      _ -> Nothing
  }

-- "X yang lalu" atau "X hari/jam/menit yang lalu" (X ago)
ruleDurationYangLalu :: Rule
ruleDurationYangLalu = Rule
  { name = "<duration> yang lalu"
  , pattern =
    [ dimension Duration
    , regex "yang (lalu|sudah lewat)|lalu"
    ]
  , prod = \tokens -> case tokens of
      (Token Duration dd:_) -> tt $ durationAgo dd
      _ -> Nothing
  }

-- "X hari terakhir" atau "X hari yang terakhir" (last X days) - returns date range
-- Uses cycleN like English "last N days" rule for consistency
-- cycleN True Day (-days) creates interval from (days) days ago to yesterday
-- This matches English behavior: "last 7 days" returns Dec 11 to Dec 18
ruleHariTerakhir :: Rule
ruleHariTerakhir = Rule
  { name = "<numeral> hari terakhir"
  , pattern =
    [ dimension Numeral
    , regex "hari"
    , regex "terakhir|yang terakhir"
    ]
  , prod = \tokens -> case tokens of
      (Token Numeral TNumeral.NumeralData{TNumeral.value = v}:_:_) -> do
        let days = floor v
            -- Use cycleN like English "last N days" rule
            -- cycleN True Day (-days) creates Closed interval from (days) days ago to yesterday
        tt $ cycleN True TG.Day (-days)
      _ -> Nothing
  }

-- "dalam X hari terakhir" (in last X days) - returns date range
-- Same as ruleHariTerakhir, uses cycleN for consistency with English
ruleDalamHariTerakhir :: Rule
ruleDalamHariTerakhir = Rule
  { name = "dalam <numeral> hari terakhir"
  , pattern =
    [ regex "dalam"
    , dimension Numeral
    , regex "hari"
    , regex "terakhir|yang terakhir"
    ]
  , prod = \tokens -> case tokens of
      (_:Token Numeral TNumeral.NumeralData{TNumeral.value = v}:_:_) -> do
        let days = floor v
            -- Use cycleN like English "last N days" rule
            -- cycleN True Day (-days) creates Closed interval from (days) days ago to yesterday
        tt $ cycleN True TG.Day (-days)
      _ -> Nothing
  }

-----------------------------------------------------------------
-- Interval waktu: "dari X sampai Y", "X sampe Y"
-----------------------------------------------------------------

-- "dari X sampai Y" atau "dari X sampe Y" (from X until Y)
ruleIntervalDariSampai :: Rule
ruleIntervalDariSampai = Rule
  { name = "dari <time> sampai <time>"
  , pattern =
    [ regex "dari"
    , dimension Time
    , regex "sampai|sampe|hingga"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

-- "X sampai Y" atau "X sampe Y" (X until Y)
ruleIntervalSampai :: Rule
ruleIntervalSampai = Rule
  { name = "<time> sampai <time>"
  , pattern =
    [ dimension Time
    , regex "sampai|sampe|hingga"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:_:Token Time td2:_) ->
        Token Time <$> interval TTime.Closed td1 td2
      _ -> Nothing
  }

-- "sampai <time>" atau "sampe <time>" (until <time>)
ruleIntervalSampaiTime :: Rule
ruleIntervalSampaiTime = Rule
  { name = "sampai <time>"
  , pattern =
    [ regex "sampai|sampe|hingga"
    , dimension Time
    ]
  , prod = \tokens -> case tokens of
      (_:Token Time td:_) -> tt $ withDirection TTime.Before $ notLatent td
      _ -> Nothing
  }

-----------------------------------------------------------------
-- Waktu / jam: "pukul 14.30", "jam 2", "jam 2 siang"
-----------------------------------------------------------------

-- pukul HH:MM atau HH.MM (24 jam)
rulePukulHHMM :: Rule
rulePukulHHMM = Rule
  { name = "pukul HH:MM atau HH.MM (24 jam)"
  , pattern =
    [ regex "(pukul|pk|jam)\\s*(2[0-3]|[01]?\\d)[:\\.]([0-5]\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:hh:mm:_)) : _) -> do
        h <- parseInt hh
        m <- parseInt mm
        tt $ hourMinute True h m
      _ -> Nothing
  }

-- jam HH (24 jam)
ruleJamHH :: Rule
ruleJamHH = Rule
  { name = "jam HH (24 jam)"
  , pattern =
    [ regex "(pukul|pk|jam)\\s*(2[0-3]|[01]?\\d)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:hh:_)) : _) -> do
        h <- parseInt hh
        tt $ hour True h
      _ -> Nothing
  }

-- jam HH (pagi/sore/malam) -> 12 jam dengan modifier
ruleJamHHPartOfDay :: Rule
ruleJamHHPartOfDay = Rule
  { name = "jam HH pagi/sore/malam"
  , pattern =
    [ regex "(pukul|pk|jam)\\s*(1[0-2]|0?\\d)\\s*(pagi|siang|sore|malam)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (_:hh:part:_)) : _) -> do
        h <- parseInt hh
        let p = Text.toLower part
        -- sangat sederhana: pagi/siang -> +0, sore/malam -> +12
        let h' =
              if p == "pagi" || p == "siang"
                then h
                else if h < 12 then h + 12 else h
        tt $ hour True h'
      _ -> Nothing
  }

-- Part of day: pagi, siang, sore, malam
rulePartOfDays :: Rule
rulePartOfDays = Rule
  { name = "part of days"
  , pattern =
    [ regex "(pagi|siang|sore|malam)"
    ]
  , prod = \tokens -> case tokens of
      (Token RegexMatch (GroupMatch (match:_)):_) -> do
        let (start, end) = case Text.toLower match of
              "pagi"  -> (hour False 0, hour False 12)
              "siang" -> (hour False 12, hour False 15)
              "sore"  -> (hour False 15, hour False 18)
              "malam" -> (hour False 18, hour False 0)
              _       -> (hour False 12, hour False 19)
        td <- interval TTime.Open start end
        tt . partOfDay $ mkLatent td
      _ -> Nothing
  }

-- "<time> <part-of-day>" seperti "besok pagi", "kemarin malam"
ruleTimePartOfDay :: Rule
ruleTimePartOfDay = Rule
  { name = "<time> <part-of-day>"
  , pattern =
    [ dimension Time
    , Predicate isAPartOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) ->
        Token Time <$> intersect td1 td2
      _ -> Nothing
  }

-- "<time> jam <hour>" seperti "besok siang jam 2", "kemarin malam jam 10"
-- Combines a time expression (possibly with part-of-day) with an hour
-- When the time has a part-of-day context, interprets hour in 12-hour format
ruleTimeJamHour :: Rule
ruleTimeJamHour = Rule
  { name = "<time> jam <hour>"
  , pattern =
    [ dimension Time
    , Predicate isATimeOfDay
    ]
  , prod = \tokens -> case tokens of
      (Token Time td1:Token Time td2:_) -> do
        -- Extract hour from td2
        h <- case TTime.form td2 of
          Just (TTime.TimeOfDay (Just hourVal) _) -> Just hourVal
          _ -> Nothing
        -- Check if td1 has a part-of-day context by examining the predicate structure
        -- Even if form is not PartOfDay, we can detect part-of-day intervals from the predicate
        let extractStartHour pred = case pred of
              TTime.IntersectPredicate p1 p2 ->
                case extractStartHour p1 of
                  Just h -> Just h
                  Nothing -> extractStartHour p2
              TTime.TimeIntervalsPredicate
                _ TTime.TimeDatePredicate{TTime.tdHour = Just (False, start)} _ -> Just start
              _ -> Nothing
        let startHour = extractStartHour (TTime.timePred td1)
        -- If we found a start hour >= 12, it's afternoon/evening/night
        -- Convert 1-11 to PM (13-23), keep 12 as noon
        let h' = case startHour of
              Just start | start >= 12 ->
                -- Afternoon/evening/night context
                if h >= 1 && h <= 11
                  then h + 12  -- PM
                  else if h == 12
                  then 12  -- Keep 12 as noon
                  else h
              _ ->
                -- No afternoon context or can't determine, use hour as-is
                -- But if hour is 1-11 and we have PartOfDay form, assume PM
                case TTime.form td1 of
                  Just TTime.PartOfDay ->
                    if h >= 1 && h <= 11
                      then h + 12  -- Assume PM for part-of-day
                      else h
                  _ -> h  -- No part-of-day context, use hour as-is (24-hour format)
        -- Intersect the time with the adjusted hour
        let hourTd = hour True h'
        Token Time <$> intersect td1 hourTd
      _ -> Nothing
  }

-- "tadi malam" (last night)
ruleTadiMalam :: Rule
ruleTadiMalam = Rule
  { name = "tadi malam"
  , pattern =
    [ regex "tadi"
    , regex "malam"
    ]
  , prod = \_ -> do
      let td1 = cycleNth TG.Day (-1)
      td2 <- interval TTime.Open (hour False 18) (hour False 0)
      Token Time . partOfDay <$> intersect td1 td2
  }

-----------------------------------------------------------------
-- Hari libur tetap Indonesia (tanggal fix setiap tahun)
-----------------------------------------------------------------

ruleHolidays :: [Rule]
ruleHolidays = mkRuleHolidays
  [ -- Januari
    ( "Tahun Baru Masehi"
    , "tahun baru( masehi)?|tahun baru"
    , monthDay 1 1
    )
  , ( "Hari Peristiwa Kapal Selam"
    , "hari peristiwa kapal selam"
    , monthDay 1 24
    )
  -- Februari
  , ( "Hari Gizi Nasional"
    , "hari gizi nasional"
    , monthDay 2 25
    )
  -- Maret
  , ( "Hari Perempuan Internasional"
    , "hari perempuan internasional|hari wanita internasional"
    , monthDay 3 8
    )
  , ( "Hari Musik Nasional"
    , "hari musik nasional"
    , monthDay 3 9
    )
  -- April
  , ( "Hari Kartini"
    , "hari kartini"
    , monthDay 4 21
    )
  , ( "Hari Bumi"
    , "hari bumi"
    , monthDay 4 22
    )
  -- Mei
  , ( "Hari Buruh Internasional"
    , "hari buruh( internasional)?|may day|hari pekerja"
    , monthDay 5 1
    )
  , ( "Hari Pendidikan Nasional"
    , "hari pendidikan nasional|hardiknas"
    , monthDay 5 2
    )
  , ( "Hari Kebangkitan Nasional"
    , "hari kebangkitan nasional|harkitnas"
    , monthDay 5 20
    )
  -- Juni
  , ( "Hari Lahir Pancasila"
    , "hari lahir pancasila"
    , monthDay 6 1
    )
  , ( "Hari Lingkungan Hidup Sedunia"
    , "hari lingkungan hidup( sedunia)?"
    , monthDay 6 5
    )
  , ( "Hari Lahir Jakarta"
    , "hari lahir jakarta|hari jadi jakarta"
    , monthDay 6 22
    )
  -- Juli
  , ( "Hari Bhayangkara"
    , "hari bhayangkara|hari polisi"
    , monthDay 7 1
    )
  , ( "Hari Anak Nasional"
    , "hari anak( nasional)?"
    , monthDay 7 23
    )
  -- Agustus
  , ( "Hari Proklamasi Kemerdekaan"
    , "hari kemerdekaan( ri)?|17 agustus(an)?|hari ulang tahun( ri)?|hut ri|hut indonesia"
    , monthDay 8 17
    )
  -- September
  , ( "Hari Olahraga Nasional"
    , "hari olahraga nasional|haornas"
    , monthDay 9 9
    )
  -- Oktober
  , ( "Hari Kesaktian Pancasila"
    , "hari kesaktian pancasila"
    , monthDay 10 1
    )
  , ( "Hari Batik Nasional"
    , "hari batik( nasional)?"
    , monthDay 10 2
    )
  , ( "Hari Sumpah Pemuda"
    , "hari sumpah pemuda"
    , monthDay 10 28
    )
  -- November
  , ( "Hari Pahlawan"
    , "hari pahlawan"
    , monthDay 11 10
    )
  , ( "Hari Guru Nasional"
    , "hari guru( nasional)?"
    , monthDay 11 25
    )
  -- Desember
  , ( "Hari Ibu"
    , "hari ibu"
    , monthDay 12 22
    )
  , ( "Hari Raya Natal"
    , "hari raya natal|natal|hari natal"
    , monthDay 12 25
    )
  ]

-----------------------------------------------------------------
-- Daftar semua rules
-----------------------------------------------------------------

rules :: [Rule]
rules =
  [ ruleDDMM
  , ruleDDMMDash
  , ruleDDMMDot
  , ruleDDMMYYYY
  , ruleDDMMYYYYDash
  , ruleDDMMYYYYDot
  , ruleYYYYMMDDISO
  , ruleYYYYMMDDNoSep
  , ruleDDMonthYYYY
  , ruleDDMonth
  , ruleMonthYear
  , ruleNumeralMingguLalu
  , ruleNumeralMingguDepan
  , ruleMingguDepan
  , ruleMingguKemudian
  , ruleMingguLalu
  , ruleMingguKemarenKemarin
  , ruleSelamaMingguIni
  , ruleSelamaMingguDepan
  , ruleSelamaMingguLalu
  , ruleMingguIni
  , ruleMonday
  , ruleTuesday
  , ruleWednesday
  , ruleThursday
  , ruleFriday
  , ruleSaturday
  , ruleSunday
  , ruleHariIni
  , ruleSekarang
  , ruleBesok
  , ruleLusa
  , ruleKemarin
  , ruleKemarinLusa
  , ruleBulanDepan
  , ruleSelamaBulanLalu
  , ruleSelamaBulanDepan
  , ruleBulanLalu
  , ruleBulanIni
  , ruleBulanIniSampaiSekarang
  , ruleDariAwalBulanSampaiSekarang
  , ruleSejakAwalBulan
  , ruleSejakBulanLalu
  , ruleSejakMingguLalu
  , ruleSejakMingguIni
  , ruleSejakTahunLalu
  , ruleSelamaBulanTahun
  , ruleSelamaBulan
  , ruleTahunLalu
  , ruleTahunDepan
  , ruleAkhirMinggu
  , ruleAwalBulan
  , rulePukulHHMM
  , ruleJamHH
  , ruleJamHHPartOfDay
  , rulePartOfDays
  , ruleTimePartOfDay
  , ruleTimeJamHour
  , ruleTadiMalam
  , ruleHariTerakhir
  , ruleDalamHariTerakhir
  , ruleDalamDuration
  , ruleDurationKemudian
  , ruleDurationYangLalu
  , ruleIntervalDariSampai
  , ruleIntervalSampai
  , ruleIntervalSampaiTime
  ]
  ++ ruleHolidays