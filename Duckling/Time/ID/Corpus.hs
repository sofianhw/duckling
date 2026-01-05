-- Copyright (c) 2016-present, Facebook, Inc.
-- All rights reserved.
--
-- This source code is licensed under the BSD-style license found in the
-- LICENSE file in the root directory of this source tree.


{-# LANGUAGE OverloadedStrings #-}

module Duckling.Time.ID.Corpus
  ( corpus
  , negativeCorpus
  ) where

import Data.String
import Prelude

import Duckling.Locale
import Duckling.Resolve
import Duckling.Time.Corpus
import Duckling.Time.Types hiding (Month)
import Duckling.TimeGrain.Types hiding (add)
import Duckling.Testing.Types hiding (examples)

context :: Context
context = testContext {locale = makeLocale ID Nothing}

corpus :: Corpus
corpus = (context, testOptions, allExamples)

negativeCorpus :: NegativeCorpus
negativeCorpus = (context, testOptions, examples)
  where
    examples =
      [ "satu hotel"
      , "dua tiket"
      , "tiga"
      ]

allExamples :: [Example]
allExamples = concat
  [ -- Relative time: sekarang, hari ini, besok, kemarin
    examples (datetime (2013, 2, 12, 4, 30, 0) Second)
             [ "sekarang"
             , "skrg"
             , "hari ini"
             ]
  , examples (datetime (2013, 2, 12, 0, 0, 0) Day)
             [ "hari ini"
             ]
  , examples (datetime (2013, 2, 11, 0, 0, 0) Day)
             [ "kemarin"
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "besok"
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "lusa"
             ]
  , examples (datetime (2013, 2, 10, 0, 0, 0) Day)
             [ "kemarin lusa"
             ]
  
  -- Days of week
  , examples (datetime (2013, 2, 18, 0, 0, 0) Day)
             [ "senin"
             , "sen"
             , "sen."
             ]
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "selasa"
             , "sel"
             , "sel."
             ]
  , examples (datetime (2013, 2, 13, 0, 0, 0) Day)
             [ "rabu"
             , "rab"
             , "rab."
             ]
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "kamis"
             , "kam"
             , "kam."
             ]
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "jumat"
             , "jum'at"
             , "jum"
             , "jum."
             ]
  , examples (datetime (2013, 2, 16, 0, 0, 0) Day)
             [ "sabtu"
             , "sab"
             , "sab."
             ]
  , examples (datetime (2013, 2, 17, 0, 0, 0) Day)
             [ "minggu"
             , "ahad"
             , "min"
             , "min."
             ]
  
  -- Date formats: dd/mm, dd-mm, dd.mm
  , examples (datetime (2013, 2, 15, 0, 0, 0) Day)
             [ "15/2"
             , "15/02"
             , "15 / 2"
             , "15-2"
             , "15-02"
             , "15.2"
             , "15.02"
             ]
  , examples (datetime (2013, 10, 31, 0, 0, 0) Day)
             [ "31/10/2013"
             , "31/10/13"
             , "31-10-2013"
             , "31-10-13"
             , "31.10.2013"
             , "31.10.13"
             ]
  
  -- Date with month name
  , examples (datetime (2013, 1, 15, 0, 0, 0) Day)
             [ "15 januari"
             , "15 jan"
             , "15 januari 2013"
             , "15 jan 2013"
             ]
  , examples (datetime (2013, 8, 17, 0, 0, 0) Day)
             [ "17 agustus"
             , "17 agu"
             , "17 ags"
             , "17 agustus 2013"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "25 desember"
             , "25 des"
             , "25 desember 2013"
             ]
  
  -- ISO format
  , examples (datetime (2013, 10, 31, 0, 0, 0) Day)
             [ "2013-10-31"
             ]
  
  -- Time expressions
  , examples (datetime (2013, 2, 12, 14, 30, 0) Minute)
             [ "pukul 14:30"
             , "pukul 14.30"
             , "pk 14:30"
             , "jam 14:30"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "pukul 14"
             , "pk 14"
             , "jam 14"
             ]
  , examples (datetime (2013, 2, 12, 2, 0, 0) Hour)
             [ "jam 2 pagi"
             , "pukul 2 pagi"
             ]
  , examples (datetime (2013, 2, 12, 14, 0, 0) Hour)
             [ "jam 2 sore"
             , "pukul 2 sore"
             ]
  , examples (datetime (2013, 2, 12, 22, 0, 0) Hour)
             [ "jam 10 malam"
             , "pukul 10 malam"
             ]
  , examples (datetime (2013, 2, 12, 15, 0, 0) Hour)
             [ "pukul 3 siang"
             ]
  , examples (datetime (2013, 2, 12, 9, 0, 0) Hour)
             [ "jam 9 pagi"
             ]
  
  -- Relative cycles
  , examples (datetime (2013, 2, 19, 0, 0, 0) Day)
             [ "minggu depan"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "minggu lalu"
             ]
  , examples (datetime (2013, 3, 1, 0, 0, 0) Day)
             [ "bulan depan"
             ]
  , examples (datetime (2013, 1, 1, 0, 0, 0) Day)
             [ "bulan lalu"
             ]
  , examples (datetime (2014, 1, 1, 0, 0, 0) Day)
             [ "tahun depan"
             ]
  , examples (datetime (2012, 1, 1, 0, 0, 0) Day)
             [ "tahun lalu"
             ]
  
  -- Relative dates with numbers
  , examples (datetime (2013, 2, 14, 0, 0, 0) Day)
             [ "2 hari lagi"
             ]
  , examples (datetime (2013, 2, 9, 0, 0, 0) Day)
             [ "3 hari lalu"
             ]
  , examples (datetime (2013, 2, 5, 0, 0, 0) Day)
             [ "7 hari lalu"
             ]
  , examples (datetime (2013, 1, 29, 0, 0, 0) Day)
             [ "2 minggu lalu"
             ]
  , examples (datetime (2013, 1, 22, 0, 0, 0) Day)
             [ "3 minggu lalu"
             ]
  , examples (datetime (2013, 1, 12, 0, 0, 0) Day)
             [ "1 bulan lalu"
             ]
  , examples (datetime (2012, 12, 12, 0, 0, 0) Day)
             [ "2 bulan lalu"
             ]
  
  -- Part of day
  , examples (datetimeInterval ((2013, 2, 12, 0, 0, 0), (2013, 2, 12, 12, 0, 0)) Hour)
             [ "pagi"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 12, 0, 0), (2013, 2, 12, 15, 0, 0)) Hour)
             [ "siang"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 15, 0, 0), (2013, 2, 12, 18, 0, 0)) Hour)
             [ "sore"
             ]
  , examples (datetimeInterval ((2013, 2, 12, 18, 0, 0), (2013, 2, 13, 0, 0, 0)) Hour)
             [ "malam"
             ]
  
  -- Date + time combinations
  , examples (datetime (2013, 2, 13, 6, 0, 0) Hour)
             [ "besok pagi"
             ]
  , examples (datetime (2013, 2, 13, 12, 0, 0) Hour)
             [ "besok siang"
             ]
  , examples (datetime (2013, 2, 13, 15, 0, 0) Hour)
             [ "besok sore"
             ]
  , examples (datetime (2013, 2, 13, 18, 0, 0) Hour)
             [ "besok malam"
             ]
  , examples (datetime (2013, 2, 11, 6, 0, 0) Hour)
             [ "kemarin pagi"
             ]
  , examples (datetime (2013, 2, 11, 18, 0, 0) Hour)
             [ "kemarin malam"
             , "tadi malam"
             ]
  , examples (datetime (2013, 2, 12, 3, 0, 0) Hour)
             [ "hari ini jam 3"
             ]
  , examples (datetime (2013, 2, 13, 14, 30, 0) Minute)
             [ "besok jam 14:30"
             ]
  , examples (datetime (2013, 2, 11, 10, 0, 0) Hour)
             [ "kemarin pukul 10 pagi"
             ]
  , examples (datetime (2013, 12, 13, 15, 0, 0) Hour)
             [ "13 desember jam 15:00"
             ]
  , examples (datetime (2013, 2, 13, 14, 0, 0) Hour)
             [ "besok sore jam 2"
             ]
  
  -- Additional date formats
  , examples (datetime (2013, 12, 13, 0, 0, 0) Day)
             [ "13 desember"
             ]
  , examples (datetime (2025, 2, 14, 0, 0, 0) Day)
             [ "14 februari 2025"
             ]
  , examples (datetime (2024, 1, 1, 0, 0, 0) Day)
             [ "1 jan 2024"
             ]
  , examples (datetime (2025, 12, 25, 0, 0, 0) Day)
             [ "25 desember 2025"
             ]
  , examples (datetime (2025, 11, 12, 0, 0, 0) Day)
             [ "2025-11-12"
             ]
  , examples (datetime (2025, 11, 12, 0, 0, 0) Day)
             [ "20251112"
             ]
  , examples (datetime (2024, 12, 25, 0, 0, 0) Day)
             [ "25/12/2024"
             ]
  , examples (datetime (2013, 12, 25, 0, 0, 0) Day)
             [ "25/12"
             ]
  , examples (datetime (2025, 1, 12, 0, 0, 0) Day)
             [ "12 Jan 2025"
             ]
  , examples (datetime (2025, 1, 1, 0, 0, 0) Day)
             [ "Jan 2025"
             ]
  , examples (datetime (2025, 1, 15, 0, 0, 0) Day)
             [ "15/01/2025"
             , "15-01-2025"
             , "15.01.2025"
             ]
  
  -- Interval expressions
  , examples (datetimeInterval ((2013, 1, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Day)
             [ "minggu ini"
             ]
  , examples (datetimeInterval ((2013, 2, 1, 0, 0, 0), (2013, 3, 1, 0, 0, 0)) Day)
             [ "bulan ini"
             ]
  , examples (datetimeInterval ((2013, 1, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Day)
             [ "awal bulan"
             ]
  -- "selama bulan <month>" - full month interval
  , examples (datetimeInterval ((2013, 12, 1, 0, 0, 0), (2014, 1, 1, 0, 0, 0)) Day)
             [ "selama bulan desember"
             , "selama bulan des"
             ]
  , examples (datetimeInterval ((2013, 1, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Day)
             [ "selama bulan januari"
             , "selama bulan jan"
             ]
  , examples (datetimeInterval ((2013, 8, 1, 0, 0, 0), (2013, 9, 1, 0, 0, 0)) Day)
             [ "selama bulan agustus"
             , "selama bulan agu"
             ]
  , examples (datetimeInterval ((2025, 12, 1, 0, 0, 0), (2026, 1, 1, 0, 0, 0)) Day)
             [ "selama bulan desember 2025"
             , "selama bulan des 2025"
             ]
  -- "selama minggu" - full week interval
  -- Note: Week intervals depend on the reference date (2013-02-12 is a Tuesday)
  -- This week: Feb 11 (Mon) to Feb 17 (Sun)
  , examples (datetimeInterval ((2013, 2, 11, 0, 0, 0), (2013, 2, 18, 0, 0, 0)) Day)
             [ "selama minggu ini"
             ]
  -- Next week: Feb 18 (Mon) to Feb 24 (Sun)
  , examples (datetimeInterval ((2013, 2, 18, 0, 0, 0), (2013, 2, 25, 0, 0, 0)) Day)
             [ "selama minggu depan"
             ]
  -- Last week: Feb 4 (Mon) to Feb 10 (Sun)
  , examples (datetimeInterval ((2013, 2, 4, 0, 0, 0), (2013, 2, 11, 0, 0, 0)) Day)
             [ "selama minggu lalu"
             ]
  -- "selama bulan lalu/depan" - full month interval
  -- Last month: Jan 1 to Feb 1 (exclusive end, so Jan 1-31)
  , examples (datetimeInterval ((2013, 1, 1, 0, 0, 0), (2013, 2, 1, 0, 0, 0)) Day)
             [ "selama bulan lalu"
             ]
  -- Next month: Mar 1 to Apr 1 (exclusive end, so Mar 1-31)
  , examples (datetimeInterval ((2013, 3, 1, 0, 0, 0), (2013, 4, 1, 0, 0, 0)) Day)
             [ "selama bulan depan"
             ]
  
  -- Holidays
  , examples (datetimeHoliday (2013, 1, 1, 0, 0, 0) Day "Tahun Baru Masehi")
             [ "tahun baru"
             , "tahun baru masehi"
             ]
  , examples (datetimeHoliday (2013, 5, 1, 0, 0, 0) Day "Hari Buruh Internasional")
             [ "hari buruh"
             , "hari buruh internasional"
             ]
  , examples (datetimeHoliday (2013, 5, 20, 0, 0, 0) Day "Hari Kebangkitan Nasional")
             [ "hari kebangkitan nasional"
             ]
  , examples (datetimeHoliday (2013, 6, 1, 0, 0, 0) Day "Hari Lahir Pancasila")
             [ "hari lahir pancasila"
             ]
  , examples (datetimeHoliday (2013, 8, 17, 0, 0, 0) Day "Hari Proklamasi Kemerdekaan")
             [ "hari kemerdekaan"
             , "hari kemerdekaan ri"
             , "17 agustusan"
             ]
  , examples (datetimeHoliday (2013, 10, 1, 0, 0, 0) Day "Hari Kesaktian Pancasila")
             [ "hari kesaktian pancasila"
             ]
  , examples (datetimeHoliday (2013, 10, 28, 0, 0, 0) Day "Hari Sumpah Pemuda")
             [ "hari sumpah pemuda"
             ]
  , examples (datetimeHoliday (2013, 11, 10, 0, 0, 0) Day "Hari Pahlawan")
             [ "hari pahlawan"
             ]
  , examples (datetimeHoliday (2013, 12, 25, 0, 0, 0) Day "Hari Raya Natal")
             [ "hari raya natal"
             , "natal"
             ]
  , examples (datetimeHoliday (2013, 4, 21, 0, 0, 0) Day "Hari Kartini")
             [ "hari kartini"
             ]
  , examples (datetimeHoliday (2013, 5, 2, 0, 0, 0) Day "Hari Pendidikan Nasional")
             [ "hari pendidikan nasional"
             , "hardiknas"
             ]
  , examples (datetimeHoliday (2013, 7, 23, 0, 0, 0) Day "Hari Anak Nasional")
             [ "hari anak"
             , "hari anak nasional"
             ]
  , examples (datetimeHoliday (2013, 10, 2, 0, 0, 0) Day "Hari Batik Nasional")
             [ "hari batik"
             , "hari batik nasional"
             ]
  , examples (datetimeHoliday (2013, 11, 25, 0, 0, 0) Day "Hari Guru Nasional")
             [ "hari guru"
             , "hari guru nasional"
             ]
  , examples (datetimeHoliday (2013, 12, 22, 0, 0, 0) Day "Hari Ibu")
             [ "hari ibu"
             ]
  
  -- Date ranges: "X hari terakhir", "dalam X hari terakhir"
  , examples (datetimeInterval ((2013, 2, 5, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "7 hari terakhir"
             , "dalam 7 hari terakhir"
             ]
  , examples (datetimeInterval ((2013, 2, 10, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "2 hari terakhir"
             , "dalam 2 hari terakhir"
             ]
  , examples (datetimeInterval ((2013, 1, 29, 0, 0, 0), (2013, 2, 12, 0, 0, 0)) Day)
             [ "14 hari terakhir"
             , "dalam 14 hari terakhir"
             ]
  ]

