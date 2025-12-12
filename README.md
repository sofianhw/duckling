![Duckling Logo](https://github.com/facebook/duckling/raw/main/logo.png)

### Docker

For detailed Docker build and deployment instructions, including Indonesian (ID) time parsing support with Asia/Jakarta timezone, see the [Docker Guide](DOCKER_GUIDE.md).

**Quick start:**
```bash
# Build with BuildKit (recommended)
DOCKER_BUILDKIT=1 docker build -t duckling .

# Run the container
docker run -d -p 8000:8000 --name duckling duckling
```

The Docker image includes:
- **Languages**: English (EN) and Indonesian (ID) only
- **Timezone**: Asia/Jakarta (WIB, UTC+7) by default
- **Optimized builds**: BuildKit cache mounts for faster rebuilds

### Changes Made

The modification is minimal and surgical - only the date resolution logic in `Duckling/Time/Types.hs` was changed to prefer past dates when both past and future candidates exist. See the commit history for details.

### Indonesian (ID) Time Parsing Support

This fork includes comprehensive Indonesian time parsing support with the following features:

#### Supported Time Expressions

**Relative Time:**
- `hari ini` (today), `kemarin` (yesterday), `besok` (tomorrow), `lusa` (day after tomorrow)
- `sekarang` (now), `kemarin lusa` (day before yesterday)

**Week Expressions:**
- `minggu depan` / `minggu kemudian` (next week)
- `minggu lalu` / `minggu kemaren` / `minggu kemarin` (last week)
- `N minggu lalu` / `N minggu depan` (N weeks ago/from now)
- `minggu ini` (this week - returns interval)
- `akhir minggu` (weekend)

**Month Expressions:**
- `bulan depan` (next month), `bulan lalu` (last month)
- `bulan ini` (this month - returns interval)
- `awal bulan` (beginning of month - returns interval)
- `bulan ini sampai sekarang` / `dari awal bulan sampai sekarang` / `sejak awal bulan` (month to date)

**Year Expressions:**
- `tahun depan` (next year)

**Duration-Based:**
- `dalam X` / `X lagi` (in X duration)
- `X yang lalu` / `X lalu` (X ago)

**Date Formats:**
- `DD/MM/YYYY`, `DD-MM-YYYY`, `DD.MM.YYYY`
- `DD/MM`, `DD-MM`, `DD.MM` (current year)
- `YYYY-MM-DD` (ISO format)
- `YYYYMMDD` (no separators)
- `DD bulan` / `DD bulan YYYY` (with month names)
- `bulan YYYY` (month and year only)

**Time Expressions:**
- `pukul HH:MM` / `jam HH:MM` (24-hour format)
- `jam HH pagi/sore/malam` (12-hour with part of day)
- `besok pagi` (tomorrow morning)
- `tadi malam` (last night)

**Days of Week:**
- `senin`, `selasa`, `rabu`, `kamis`, `jumat`, `sabtu`, `minggu` / `ahad`

**Intervals:**
- `dari X sampai Y` / `X sampe Y` (from X until Y)
- `sampai X` (until X)

**Indonesian Holidays:**
- Hari Kemerdekaan (17 Agustus), Tahun Baru, Hari Pahlawan, Hari Raya Natal, and more

#### Configuration

- **Timezone**: Asia/Jakarta (WIB, UTC+7) by default
- **Default timezone**: Set in `exe/ExampleMain.hs` to `Asia/Jakarta`
- **Docker timezone**: Configured in Dockerfile

#### Testing

Run the comprehensive test suite:
```bash
./test_indonesian.sh
```

See [DOCKER_GUIDE.md](DOCKER_GUIDE.md) for detailed testing instructions and examples.

#### Files Modified

- `Duckling/Time/ID/Rules.hs` - Indonesian time parsing rules (52 rules)
- `Duckling/Time/ID/Corpus.hs` - Test corpus for Indonesian
- `Duckling/TimeGrain/ID/Rules.hs` - Time grain rules
- `Duckling/Dimensions/ID.hs` - Dimension definitions
- `exe/ExampleMain.hs` - Default timezone set to Asia/Jakarta
- `Dockerfile` - Timezone configuration and BuildKit optimizations

---

# Original Duckling Documentation

[![Support Ukraine](https://img.shields.io/badge/Support-Ukraine-FFD500?style=flat&labelColor=005BBB)](https://opensource.fb.com/support-ukraine) [![Build Status](https://travis-ci.org/facebook/duckling.svg?branch=master)](https://travis-ci.org/facebook/duckling)

Duckling is a Haskell library that parses text into structured data.

```bash
"the first Tuesday of October"
=> {"value":"2017-10-03T00:00:00.000-07:00","grain":"day"}
```

## Requirements

A Haskell environment is required. We recommend using
[stack](https://docs.haskellstack.org/en/stable/).

On Linux and MacOS you'll need to install PCRE development headers. On Linux,
use your package manager to install them. On MacOS, the easiest way to install
them is with [Homebrew](https://brew.sh/):

```bash
brew install pcre
```

If that doesn't help, try running `brew doctor` and fix the issues it finds.

## Quickstart

To compile and run the binary:

```bash
stack build
stack exec duckling-example-exe
```

The first time you run it, it will download all required packages.

This runs a basic HTTP server. Example request:

```bash
curl -XPOST http://0.0.0.0:8000/parse --data 'locale=en_GB&text=tomorrow at eight'
```

In the example application, all dimensions are enabled by default. Provide the
parameter `dims` to specify which ones you want. Examples:

```bash
Identify credit card numbers only:
$ curl -XPOST http://0.0.0.0:8000/parse --data 'locale=en_US&text="4111-1111-1111-1111"&dims="["credit-card-number"]"'
If you want multiple dimensions, comma-separate them in the array:
$ curl -XPOST http://0.0.0.0:8000/parse --data 'locale=en_US&text="3 cups of sugar"&dims="["quantity","numeral"]"'
```

See `exe/ExampleMain.hs` for an example on how to integrate Duckling in your
project. If your backend doesn't run Haskell or if you don't want to spin your
own Duckling server, you can directly use [wit.ai](https://wit.ai)'s built-in
entities.

## Supported dimensions

Duckling supports many languages, but most don't support all dimensions yet
(**we need your help!**). Please look into
[this directory](https://github.com/facebook/duckling/blob/master/Duckling/Dimensions)
for language-specific support.

| Dimension          | Example input                     | Example value output                                                                                                                                         |
| ------------------ | --------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `AmountOfMoney`    | "42€"                             | `{"value":42,"type":"value","unit":"EUR"}`                                                                                                                   |
| `CreditCardNumber` | "4111-1111-1111-1111"             | `{"value":"4111111111111111","issuer":"visa"}`                                                                                                               |
| `Distance`         | "6 miles"                         | `{"value":6,"type":"value","unit":"mile"}`                                                                                                                   |
| `Duration`         | "3 mins"                          | `{"value":3,"minute":3,"unit":"minute","normalized":{"value":180,"unit":"second"}}`                                                                          |
| `Email`            | "duckling-team@fb.com"            | `{"value":"duckling-team@fb.com"}`                                                                                                                           |
| `Numeral`          | "eighty eight"                    | `{"value":88,"type":"value"}`                                                                                                                                |
| `Ordinal`          | "33rd"                            | `{"value":33,"type":"value"}`                                                                                                                                |
| `PhoneNumber`      | "+1 (650) 123-4567"               | `{"value":"(+1) 6501234567"}`                                                                                                                                |
| `Quantity`         | "3 cups of sugar"                 | `{"value":3,"type":"value","product":"sugar","unit":"cup"}`                                                                                                  |
| `Temperature`      | "80F"                             | `{"value":80,"type":"value","unit":"fahrenheit"}`                                                                                                            |
| `Time`             | "today at 9am"                    | `{"values":[{"value":"2016-12-14T09:00:00.000-08:00","grain":"hour","type":"value"}],"value":"2016-12-14T09:00:00.000-08:00","grain":"hour","type":"value"}` |
| `Url`              | "https://api.wit.ai/message?q=hi" | `{"value":"https://api.wit.ai/message?q=hi","domain":"api.wit.ai"}`                                                                                          |
| `Volume`           | "4 gallons"                       | `{"value":4,"type":"value","unit":"gallon"}`                                                                                                                 |

[Custom dimensions](https://github.com/facebook/duckling/blob/master/exe/CustomDimensionExample.hs)
are also supported.

## Extending Duckling

To regenerate the classifiers and run the test suite:

```bash
stack build :duckling-regen-exe && stack exec duckling-regen-exe && stack test
```

It's important to regenerate the classifiers after updating the code and before
running the test suite.

To extend Duckling's support for a dimension in a given language, typically 4
files need to be updated:

- `Duckling/<Dimension>/<Lang>/Rules.hs`

- `Duckling/<Dimension>/<Lang>/Corpus.hs`

- `Duckling/Dimensions/<Lang>.hs` (if not already present in
  `Duckling/Dimensions/Common.hs`)

- `Duckling/Rules/<Lang>.hs`

To add a new language:

- Make sure that the language code used follows the
  [ISO-639-1 standard](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes).
- The first dimension to implement is `Numeral`.
- Follow
  [this example](https://github.com/facebook/duckling/commit/24d3f199768be970149412c95b1c1bf5d76f8240).

To add a new locale:

- There should be a need for diverging rules between the locale and the
  language.
- Make sure that the locale code is a valid
  [ISO3166 alpha2 country code](https://www.iso.org/obp/ui/#search/code/).
- Follow
  [this example](https://github.com/facebook/duckling/commit/1ab5f447d2635fe6d48887a501d333a52adff5b9).

Rules have a name, a pattern and a production. Patterns are used to perform
character-level matching (regexes on input) and concept-level matching
(predicates on tokens). Productions are arbitrary functions that take a list of
tokens and return a new token.

The corpus (resp. negative corpus) is a list of examples that should (resp.
shouldn't) parse. The reference time for the corpus is Tuesday Feb 12, 2013 at
4:30am.

`Duckling.Debug` provides a few debugging tools:

```bash
$ stack repl --no-load
> :l Duckling.Debug
> debug (makeLocale EN $ Just US) "in two minutes" [Seal Time]
in|within|after <duration> (in two minutes)
-- regex (in)
-- <integer> <unit-of-duration> (two minutes)
-- -- integer (0..19) (two)
-- -- -- regex (two)
-- -- minute (grain) (minutes)
-- -- -- regex (minutes)
[Entity {dim = "time", body = "in two minutes", value = RVal Time (TimeValue (SimpleValue (InstantValue {vValue = 2013-02-12 04:32:00 -0200, vGrain = Second})) [SimpleValue (InstantValue {vValue = 2013-02-12 04:32:00 -0200, vGrain = Second})] Nothing), start = 0, end = 14}]
```

## License

Duckling is [BSD-licensed](LICENSE).
