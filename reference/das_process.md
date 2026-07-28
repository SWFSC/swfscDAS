# Process DAS data

Process DAS data (the output of
[`das_read`](https://swfsc.github.io/swfscDAS/reference/das_read.md)),
including extracting state and condition information for each DAS event

## Usage

``` r
das_process(x, ...)

# S3 method for class 'character'
das_process(x, ...)

# S3 method for class 'data.frame'
das_process(x, ...)

# S3 method for class 'das_dfr'
das_process(
  x,
  days.gap = 20,
  reset.event = TRUE,
  reset.effort = TRUE,
  reset.day = TRUE,
  add.dtll.sight = TRUE,
  ...
)
```

## Arguments

- x:

  an object of class `das_dfr`, an object that can be coerced to class
  `das_dfr`, or a character (file path) which is first passed to
  [`das_read`](https://swfsc.github.io/swfscDAS/reference/das_read.md)

- ...:

  passed to
  [`das_read`](https://swfsc.github.io/swfscDAS/reference/das_read.md)
  if `x` is a character. Otherwise ignored

- days.gap:

  numeric of length 1; default is `20`. Time gap (in days) used to
  identify a new cruise in concatenated DAS files, and thus also when
  state/condition information (cruise number, weather, Bft, Mode, etc)
  is reset

- reset.event:

  logical; default is `TRUE`. Indicates if state/condition information
  (weather, Bft, Mode, etc) should be reset to `NA` if there is an
  applicable event with an `NA` for that state/condition

- reset.effort:

  logical; default is `TRUE`. Indicates if state/condition information
  should be reset to `NA` when beginning a new continuous effort
  section. See Details section

- reset.day:

  logical; default is `TRUE`. Indicates if state/condition information
  should be reset to `NA` at the beginning of each day. This argument
  should only be set to `FALSE` for comparison with older methods, such
  as REPORT

- add.dtll.sight:

  logical indicating if the DateTime (dt) and latitude and longitude
  (ll) columns should be added to the sighting events (?, 1, 2, 3, 4, 5,
  6, 7, and 8) from the corresponding (immediately preceding) A event

## Value

A `das_df` object, which is also a data frame. It consists of the input
data frame, i.e. the output of
[`das_read`](https://swfsc.github.io/swfscDAS/reference/das_read.md),
with the following columns added:

|                               |               |                         |
|-------------------------------|---------------|-------------------------|
| *State/condition*             | *Column name* | *Data source*           |
| On/off effort                 | OnEffort      | B/R and E events        |
| Cruise number                 | Cruise        | Event: B; Column: Data1 |
| Effort mode                   | Mode          | Event: B; Column: Data2 |
| GMT offset of DateTime data   | OffsetGMT     | Event: B; Column: Data3 |
| Effort type                   | EffType       | Event: R; Column: Data1 |
| Number of sides with observer | ESWSide       | Event: R; Column: Data2 |
| Course (ship direction)       | Course        | Event: N; Column: Data1 |
| Speed (ship speed, knots)     | SpdKt         | Event: N; Column: Data2 |
| Beaufort sea state            | Bft           | Event: V; Column: Data1 |
| Swell height (ft)             | SwellHght     | Event: V; Column: Data2 |
| Wind speed (knots)            | WindSpdKt     | Event: V; Column: Data5 |
| Rain/fog/haze code            | RainFog       | Event: W; Column: Data1 |
| Horizontal sun (clock system) | HorizSun      | Event: W; Column: Data2 |
| Vertical sun (clock system)   | VertSun       | Event: W; Column: Data3 |
| Glare                         | Glare         | HorizSun and VertSun    |
| Visibility (nm)               | Vis           | Event: W; Column: Data5 |
| Left observer                 | ObsL          | Event: P; Column: Data1 |
| Data recorder                 | Rec           | Event: P; Column: Data2 |
| Right observer                | ObsR          | Event: P; Column: Data3 |
| Independent observer          | ObsInd        | Event: P; Column: Data4 |

OffsetGMT represents the difference in hours between the DateTime data
(which should be in local time) and GMT (i.e., UTC).

Internal warning messages are printed with row numbers of the input file
(NOT of the output data frame) of unexpected event codes and r events,
as well as if there is are potential issues with the number and/or order
of R and E events

## Details

If `x` is a character, it is assumed to be a file path and first passed
to [`das_read`](https://swfsc.github.io/swfscDAS/reference/das_read.md).
This output is then passed to `das_process`.

DAS data is event-based, meaning most events indicate when a state or
weather condition changes. For instance, a 'V' event indicates when one
or more sea state viewing conditions (such as Beaufort sea state)
change, and these conditions are the same for subsequent events until
the next 'V' event. For each state/condition: a new column is created,
the state/condition information is extracted from relevant events, and
extracted information is propagated to appropriate subsequent rows
(events). Thus, each row in the output data frame contains all pertinent
state/condition information for that row.

The following assumptions/decisions are made during processing:

- Event codes are expected to be one of the following: \#, \*, ?, 1, 2,
  3, 4, 5, 6, 7, 8, A, B, C, E, F, k, K, M, N, P, Q, r, R, s, S, t, V,
  W, g, G, p, X, Y, Z

- All '#' events (deleted events) are removed

- r events are converted to R events with non-standard effort; see
  [`das_format_pdf`](https://swfsc.github.io/swfscDAS/reference/das_format_pdf.md)
  for more details

- An event is considered 'on effort' if it is 1) an R event, 2) a B
  event immediately preceding an R event, or 3) between corresponding R
  and E events (not including the E event). The 'EffortDot' column is
  not used when determining on effort data. Note that effort is reset to
  'off effort' at the beginning of a new day.

- All state/condition information is reset at the beginning of each
  cruise. New cruises are identified using `days.gap`.

- All state/condition information relating to B, R, P, V, N, and W
  events are reset every time there is a BR event sequence if
  `reset.effort == TRUE`, because in WinCruz a BR event sequence should
  always be a BRPVNW event sequence. An event sequence means that all of
  the events have the same Lat/Lon/DateTime info, and thus previous
  values for conditions set during the event sequence should not carry
  over to any part of the event sequence.

- 'OffsetGMT' is converted to an integer. Values are expected to be
  consistent within a day for each cruise, so events will have an
  OffsetGMT value if there is any B event with the offset data on the
  same day, whether that event is before or after the B event. Thus, if
  any date/cruise combinations have multiple OffsetGMT values in the
  data, then a warning message will be printed and the OffsetGMT values
  will be all NA (for the entire output).#'

- 'Mode' is capitalized, and 'Mode' values of `NA` are assigned a value
  of "C"

- 'EffType' is capitalized, and values of `NA` are assigned a value of
  "S"

- 'ESWsides' represents the number of sides being searched during that
  effort section - a value of `NA` (for compatibility with older data)
  or "F" means 2 sides are being searched, and a value of "H" means 1
  side is being searched. ESWsides will be `NA` for values that are not
  one of "F", `NA`, or "H"

- 'Glare': `TRUE` if 'HorizSun' is 11, 12 or 1 and 'VertSun' is 2 or 3,
  or if 'HorizSun' is 12 and 'VertSun' is 1; `NA` if 'HorizSun' or
  'VertSun' is `NA`; otherwise `FALSE`

- Missing values are `NA` rather than `-1`

## Examples

``` r
y <- system.file("extdata", "das_sample.das", package = "swfscDAS")
das_process(y)
#>     Event            DateTime      Lat       Lon OnEffort Cruise Mode OffsetGMT
#> 1       B 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 2       R 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 3       P 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 4       V 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 5       N 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 6       W 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 7       V 2013-01-13 06:29:56 39.32583 -137.6018     TRUE   1000    C         5
#> 8       W 2013-01-13 06:30:10 39.32650 -137.6015     TRUE   1000    C         5
#> 9       W 2013-01-13 06:34:01 39.33600 -137.5970     TRUE   1000    C         5
#> 10      * 2013-01-13 06:37:25 39.34450 -137.5927     TRUE   1000    C         5
#> 11      P 2013-01-13 06:41:08 39.35400 -137.5880     TRUE   1000    C         5
#> 12      V 2013-01-13 06:41:08 39.35400 -137.5880     TRUE   1000    C         5
#> 13      N 2013-01-13 06:41:08 39.35400 -137.5880     TRUE   1000    C         5
#> 14      W 2013-01-13 06:41:08 39.35400 -137.5880     TRUE   1000    C         5
#> 15      S 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 16      A 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 17      1 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 18      2 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 19      3 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 20      E 2013-01-13 06:46:25 39.36717 -137.5817    FALSE   1000    C         5
#> 21      * 2013-01-13 06:47:25 39.36967 -137.5807    FALSE   1000    C         5
#> 22      * 2013-01-13 06:57:25 39.37467 -137.5987    FALSE   1000    C         5
#> 23      R 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 24      P 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 25      V 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 26      N 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 27      W 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 28      * 2013-01-13 07:07:25 39.39883 -137.5868     TRUE   1000    C         5
#> 29      * 2013-01-13 07:17:25 39.42317 -137.5747     TRUE   1000    C         5
#> 30      P 2013-01-13 07:20:02 39.42950 -137.5715     TRUE   1000    C         5
#> 31      V 2013-01-13 07:20:02 39.42950 -137.5715     TRUE   1000    C         5
#> 32      N 2013-01-13 07:20:02 39.42950 -137.5715     TRUE   1000    C         5
#> 33      W 2013-01-13 07:20:02 39.42950 -137.5715     TRUE   1000    C         5
#> 34      * 2013-01-13 07:27:25 39.44733 -137.5627     TRUE   1000    C         5
#> 35      * 2013-01-13 07:37:25 39.47133 -137.5507     TRUE   1000    C         5
#> 36      W 2013-01-13 07:38:06 39.47300 -137.5500     TRUE   1000    C         5
#> 37      * 2013-01-13 07:47:25 39.49567 -137.5390     TRUE   1000    C         5
#> 38      S 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 39      A 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 40      1 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 41      2 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 42      3 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 43      E 2013-01-13 07:57:05 39.51933 -137.5277    FALSE   1000    C         5
#> 44      * 2013-01-13 07:57:25 39.52017 -137.5272    FALSE   1000    C         5
#> 45      s 2013-01-13 08:06:00 39.54217 -137.5263    FALSE   1000    C         5
#> 46      * 2013-01-13 08:07:25 39.54583 -137.5262    FALSE   1000    C         5
#> 47      s 2013-01-13 08:08:58 39.55000 -137.5255    FALSE   1000    C         5
#> 48      s 2013-01-13 08:15:32 39.56000 -137.5218    FALSE   1000    C         5
#> 49      * 2013-01-13 08:17:25 39.56233 -137.5210    FALSE   1000    C         5
#> 50      s 2013-01-13 08:17:28 39.56233 -137.5210    FALSE   1000    C         5
#> 51      s 2013-01-13 08:19:39 39.56483 -137.5197    FALSE   1000    C         5
#> 52      s 2013-01-13 08:26:45 39.56400 -137.5140    FALSE   1000    C         5
#> 53      * 2013-01-13 08:27:25 39.56333 -137.5133    FALSE   1000    C         5
#> 54      * 2013-01-13 08:37:25 39.54867 -137.5007    FALSE   1000    C         5
#> 55      * 2013-01-13 08:47:25 39.54533 -137.4807    FALSE   1000    C         5
#> 56      * 2013-01-13 08:57:25 39.54433 -137.4717    FALSE   1000    C         5
#> 57      * 2013-01-13 09:07:25 39.54333 -137.4627    FALSE   1000    C         5
#> 58      * 2013-01-13 09:17:25 39.55833 -137.4573    FALSE   1000    C         5
#> 59      R 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 60      P 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 61      V 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 62      N 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 63      W 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 64      * 2013-01-13 09:27:25 39.57983 -137.4475     TRUE   1000    C         5
#> 65      t 2013-01-13 09:34:27 39.59733 -137.4400     TRUE   1000    C         5
#> 66      * 2013-01-13 09:37:25 39.60467 -137.4368     TRUE   1000    C         5
#> 67      * 2013-01-13 09:47:25 39.62983 -137.4262     TRUE   1000    C         5
#> 68      * 2013-01-13 09:57:25 39.65517 -137.4155     TRUE   1000    C         5
#> 69      W 2013-01-13 09:59:38 39.66083 -137.4132     TRUE   1000    C         5
#> 70      V 2013-01-13 09:59:50 39.66133 -137.4130     TRUE   1000    C         5
#> 71      C 2013-01-13 10:04:23 39.67300 -137.4083     TRUE   1000    C         5
#> 72      P 2013-01-13 10:04:35 39.67350 -137.4080     TRUE   1000    C         5
#> 73      V 2013-01-13 10:04:35 39.67350 -137.4080     TRUE   1000    C         5
#> 74      N 2013-01-13 10:04:35 39.67350 -137.4080     TRUE   1000    C         5
#> 75      W 2013-01-13 10:04:35 39.67350 -137.4080     TRUE   1000    C         5
#> 76      * 2013-01-13 10:07:25 39.68083 -137.4050     TRUE   1000    C         5
#> 77      V 2013-01-13 10:11:00 39.69000 -137.4012     TRUE   1000    C         5
#> 78      W 2013-01-13 10:11:09 39.69050 -137.4010     TRUE   1000    C         5
#> 79      * 2013-01-13 10:17:25 39.70650 -137.3943     TRUE   1000    C         5
#> 80      N 2013-01-13 10:20:38 39.71483 -137.3920     TRUE   1000    C         5
#> 81      C 2013-01-13 10:20:43 39.71483 -137.3920     TRUE   1000    C         5
#> 82      * 2013-01-13 10:27:25 39.73150 -137.3993     TRUE   1000    C         5
#> 83      W 2013-01-13 10:30:28 39.73917 -137.4032     TRUE   1000    C         5
#> 84      W 2013-01-13 10:35:14 39.75117 -137.4092     TRUE   1000    C         5
#> 85      P 2013-01-13 10:36:06 39.75350 -137.4103     TRUE   1000    C         5
#> 86      V 2013-01-13 10:36:06 39.75350 -137.4103     TRUE   1000    C         5
#> 87      N 2013-01-13 10:36:06 39.75350 -137.4103     TRUE   1000    C         5
#> 88      W 2013-01-13 10:36:06 39.75350 -137.4103     TRUE   1000    C         5
#> 89      E 2013-01-13 10:36:27 39.75433 -137.4107    FALSE   1000    C         5
#> 90      * 2013-01-13 10:37:25 39.75683 -137.4118    FALSE   1000    C         5
#> 91      * 2013-01-13 10:47:25 39.78450 -137.4093    FALSE   1000    C         5
#> 92      * 2013-01-13 10:57:25 39.81250 -137.4087    FALSE   1000    C         5
#> 93      * 2013-01-13 11:07:25 39.83983 -137.4100    FALSE   1000    C         5
#> 94      * 2013-01-13 11:17:25 39.86700 -137.4110    FALSE   1000    C         5
#> 95      * 2013-01-13 11:27:25 39.89000 -137.4000    FALSE   1000    C         5
#> 96      * 2013-01-13 11:37:25 39.91233 -137.3875    FALSE   1000    C         5
#> 97      * 2013-01-13 11:47:25 39.93500 -137.3750    FALSE   1000    C         5
#> 98      R 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 99      P 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 100     V 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 101     N 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 102     W 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 103     * 2013-01-13 11:57:25 39.95767 -137.3613     TRUE   1000    C         5
#> 104     P 2013-01-13 12:02:29 39.96900 -137.3542     TRUE   1000    C         5
#> 105     V 2013-01-13 12:02:29 39.96900 -137.3542     TRUE   1000    C         5
#> 106     N 2013-01-13 12:02:29 39.96900 -137.3542     TRUE   1000    C         5
#> 107     W 2013-01-13 12:02:29 39.96900 -137.3542     TRUE   1000    C         5
#> 108     * 2013-01-13 12:07:25 39.97983 -137.3472     TRUE   1000    C         5
#> 109     W 2013-01-13 12:11:06 39.98767 -137.3418     TRUE   1000    C         5
#> 110     * 2013-01-13 12:17:25 40.00117 -137.3332     TRUE   1000    C         5
#> 111     * 2013-01-13 12:27:25 40.02183 -137.3197     TRUE   1000    C         5
#> 112     * 2013-01-13 12:37:25 40.04317 -137.3060     TRUE   1000    C         5
#> 113     P 2013-01-13 12:43:14 40.05567 -137.2978     TRUE   1000    C         5
#> 114     V 2013-01-13 12:43:14 40.05567 -137.2978     TRUE   1000    C         5
#> 115     N 2013-01-13 12:43:14 40.05567 -137.2978     TRUE   1000    C         5
#> 116     W 2013-01-13 12:43:14 40.05567 -137.2978     TRUE   1000    C         5
#> 117     * 2013-01-13 12:47:25 40.06450 -137.2920     TRUE   1000    C         5
#> 118     * 2013-01-13 12:57:25 40.08583 -137.2777     TRUE   1000    C         5
#> 119     * 2013-01-13 13:07:25 40.10750 -137.2627     TRUE   1000    C         5
#> 120     E 2013-01-13 13:16:38 40.12750 -137.2487    FALSE   1000    C         5
#> 121     * 2013-01-13 13:17:25 40.12917 -137.2477    FALSE   1000    C         5
#> 122     * 2013-01-13 13:27:25 40.13817 -137.2248    FALSE   1000    C         5
#> 123     t 2013-01-13 13:35:18 40.14000 -137.2048    FALSE   1000    C         5
#> 124     * 2013-01-13 13:37:25 40.14100 -137.1993    FALSE   1000    C         5
#> 125     * 2013-01-13 13:47:25 40.14717 -137.1782    FALSE   1000    C         5
#> 126     R 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 127     P 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 128     V 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 129     N 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 130     W 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 131     * 2013-01-13 13:57:25 40.16967 -137.1670     TRUE   1000    C         5
#> 132     P 2013-01-13 14:00:31 40.17700 -137.1642     TRUE   1000    C         5
#> 133     V 2013-01-13 14:00:31 40.17700 -137.1642     TRUE   1000    C         5
#> 134     N 2013-01-13 14:00:31 40.17700 -137.1642     TRUE   1000    C         5
#> 135     W 2013-01-13 14:00:31 40.17700 -137.1642     TRUE   1000    C         5
#> 136     t 2013-01-13 14:02:55 40.18283 -137.1622     TRUE   1000    C         5
#> 137     * 2013-01-13 14:07:25 40.19333 -137.1583     TRUE   1000    C         5
#> 138     * 2013-01-13 14:17:25 40.21667 -137.1505     TRUE   1000    C         5
#> 139     * 2013-01-13 14:27:25 40.24017 -137.1432     TRUE   1000    C         5
#> 140     * 2013-01-13 14:37:25 40.26417 -137.1353     TRUE   1000    C         5
#> 141     S 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 142     A 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 143     1 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 144     2 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 145     3 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 146     E 2013-01-13 14:38:13 40.26617 -137.1348    FALSE   1000    C         5
#> 147     * 2013-01-13 14:47:25 40.26317 -137.1353    FALSE   1000    C         5
#> 148     * 2013-01-13 14:57:25 40.26383 -137.1282    FALSE   1000    C         5
#> 149     R 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 150     P 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 151     V 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 152     N 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 153     W 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 154     * 2013-01-13 15:07:25 40.28850 -137.1205     TRUE   1000    C         5
#> 155     * 2013-01-13 15:17:25 40.31300 -137.1132     TRUE   1000    C         5
#> 156     P 2013-01-13 15:20:26 40.32033 -137.1108     TRUE   1000    C         5
#> 157     V 2013-01-13 15:20:26 40.32033 -137.1108     TRUE   1000    C         5
#> 158     N 2013-01-13 15:20:26 40.32033 -137.1108     TRUE   1000    C         5
#> 159     W 2013-01-13 15:20:26 40.32033 -137.1108     TRUE   1000    C         5
#> 160     * 2013-01-13 15:27:25 40.33700 -137.1057     TRUE   1000    C         5
#> 161     t 2013-01-13 15:36:47 40.36050 -137.0978     TRUE   1000    C         5
#> 162     * 2013-01-13 15:37:25 40.36217 -137.0973     TRUE   1000    C         5
#> 163     E 2013-01-13 15:43:08 40.37600 -137.0915    FALSE   1000    C         5
#> 164     * 2013-01-13 15:47:25 40.37933 -137.0927    FALSE   1000    C         5
#> 165     * 2013-01-13 15:57:25 40.37950 -137.0990    FALSE   1000    C         5
#> 166     R 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 167     P 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 168     V 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 169     N 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 170     W 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 171     * 2013-01-13 16:07:25 40.40200 -137.0885     TRUE   1000    C         5
#> 172     * 2013-01-13 16:17:25 40.42383 -137.0777     TRUE   1000    C         5
#> 173     V 2013-01-13 16:20:02 40.42967 -137.0745     TRUE   1000    C         5
#> 174     * 2013-01-13 16:27:25 40.44600 -137.0658     TRUE   1000    C         5
#> 175     S 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 176     A 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 177     1 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 178     2 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 179     3 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 180     E 2013-01-13 16:29:50 40.45133 -137.0628    FALSE   1000    C         5
#> 181     s 2013-01-13 16:36:34 40.46667 -137.0570    FALSE   1000    C         5
#> 182     * 2013-01-13 16:37:25 40.46867 -137.0567    FALSE   1000    C         5
#> 183     * 2013-01-13 16:47:25 40.49250 -137.0563    FALSE   1000    C         5
#> 184     C 2013-01-13 16:55:27 40.51250 -137.0593    FALSE   1000    C         5
#> 185     * 2013-01-13 16:57:25 40.51650 -137.0567    FALSE   1000    C         5
#> 186     R 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 187     P 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 188     V 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 189     N 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 190     W 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 191     S 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 192     A 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 193     1 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 194     2 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 195     3 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 196     4 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 197     E 2013-01-13 17:01:21 40.52533 -137.0515    FALSE   1000    C         5
#> 198     * 2013-01-13 17:07:25 40.52167 -137.0392    FALSE   1000    C         5
#> 199     t 2013-01-13 17:08:11 40.52083 -137.0383    FALSE   1000    C         5
#> 200     * 2013-01-13 17:17:25 40.51983 -137.0377    FALSE   1000    C         5
#> 201     * 2013-01-13 17:27:25 40.51750 -137.0427    FALSE   1000    C         5
#> 202     * 2013-01-13 17:37:25 40.50750 -137.0418    FALSE   1000    C         5
#> 203     t 2013-01-13 17:45:10 40.50450 -137.0437    FALSE   1000    C         5
#> 204     * 2013-01-13 17:47:32 40.50383 -137.0443    FALSE   1000    C         5
#> 205     * 2013-01-13 17:57:32 40.50033 -137.0472    FALSE   1000    C         5
#> 206     * 2013-01-13 18:07:32 40.50183 -137.0530    FALSE   1000    C         5
#> 207     * 2013-01-13 18:17:32 40.50567 -137.0432    FALSE   1000    C         5
#> 208     * 2013-01-14 08:45:06 40.70300 -135.8103    FALSE   1000 <NA>         5
#> 209     * 2013-01-14 08:55:06 40.71917 -135.7988    FALSE   1000 <NA>         5
#> 210     * 2013-01-14 09:05:06 40.73533 -135.7875    FALSE   1000 <NA>         5
#> 211     * 2013-01-14 09:15:06 40.74917 -135.7770    FALSE   1000 <NA>         5
#> 212     S 2013-01-14 09:17:21 40.75183 -135.7748    FALSE   1000 <NA>         5
#> 213     A 2013-01-14 09:17:21 40.75183 -135.7748    FALSE   1000 <NA>         5
#> 214     1 2013-01-14 09:17:21 40.75183 -135.7748    FALSE   1000 <NA>         5
#> 215     C 2013-01-14 09:18:04 40.75283 -135.7742    FALSE   1000 <NA>         5
#> 216     * 2013-01-14 09:25:06 40.76233 -135.7655    FALSE   1000 <NA>         5
#> 217     * 2013-01-14 09:35:06 40.76700 -135.7470    FALSE   1000 <NA>         5
#> 218     * 2013-01-14 09:45:06 40.77450 -135.7313    FALSE   1000 <NA>         5
#> 219     * 2013-01-14 09:55:06 40.78867 -135.7203    FALSE   1000 <NA>         5
#> 220     * 2013-01-14 10:05:06 40.80750 -135.7092    FALSE   1000 <NA>         5
#> 221     * 2013-01-14 10:15:06 40.82933 -135.6968    FALSE   1000 <NA>         5
#> 222     * 2013-01-14 10:25:06 40.85200 -135.6843    FALSE   1000 <NA>         5
#> 223     * 2013-01-14 10:35:06 40.87550 -135.6703    FALSE   1000 <NA>         5
#> 224     * 2013-01-14 10:45:06 40.89883 -135.6565    FALSE   1000 <NA>         5
#> 225     * 2013-01-14 10:55:06 40.92183 -135.6417    FALSE   1000 <NA>         5
#> 226     * 2013-01-14 11:05:06 40.94400 -135.6273    FALSE   1000 <NA>         5
#> 227     * 2013-01-14 11:15:06 40.96633 -135.6123    FALSE   1000 <NA>         5
#> 228     B 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 229     R 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 230     P 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 231     V 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 232     N 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 233     W 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 234     * 2013-01-14 11:25:06 40.98833 -135.5972     TRUE   1000    C         5
#> 235     F 2013-01-14 11:25:32 40.98950 -135.5965     TRUE   1000    C         5
#> 236     * 2013-01-14 11:35:06 41.01067 -135.5817     TRUE   1000    C         5
#> 237     E 2013-01-14 11:37:24 41.01583 -135.5782    FALSE   1000    C         5
#> 238     C 2013-01-14 11:37:27 41.01600 -135.5780    FALSE   1000    C         5
#> 239     R 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 240     P 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 241     V 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 242     N 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 243     W 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 244     * 2013-01-14 11:45:06 41.03400 -135.5678     TRUE   1000    C         5
#> 245     S 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 246     A 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 247     1 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 248     2 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 249     S 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 250     A 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 251     ? 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 252     1 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 253     2 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 254     3 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 255     4 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 256     E 2013-01-14 11:50:29 41.04600 -135.5595    FALSE   1000    C         5
#>     EffType ESWsides Course SpdKt Bft SwellHght WindSpdKt RainFog HorizSun
#> 1      <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 2         S        2     NA    NA  NA        NA        NA      NA       NA
#> 3         S        2     NA    NA  NA        NA        NA      NA       NA
#> 4         S        2     NA    NA   3         3        10      NA       NA
#> 5         S        2     23   9.8   3         3        10      NA       NA
#> 6         S        2     23   9.8   3         3        10       1       NA
#> 7         S        2     23   9.8   3         3        10       1       NA
#> 8         S        2     23   9.8   3         3        10       1        2
#> 9         S        2     23   9.8   3         3        10       1       NA
#> 10        S        2     23   9.8   3         3        10       1       NA
#> 11        S        2     23   9.8   3         3        10       1       NA
#> 12        S        2     23   9.8   3         3        10       1       NA
#> 13        S        2     25  10.2   3         3        10       1       NA
#> 14        S        2     25  10.2   3         3        10       1       NA
#> 15        S        2     25  10.2   3         3        10       1       NA
#> 16        S        2     25  10.2   3         3        10       1       NA
#> 17        S        2     25  10.2   3         3        10       1       NA
#> 18        S        2     25  10.2   3         3        10       1       NA
#> 19        S        2     25  10.2   3         3        10       1       NA
#> 20        S        2     25  10.2   3         3        10       1       NA
#> 21        S        2     25  10.2   3         3        10       1       NA
#> 22        S        2     25  10.2   3         3        10       1       NA
#> 23        S        2     NA    NA  NA        NA        NA      NA       NA
#> 24        S        2     NA    NA  NA        NA        NA      NA       NA
#> 25        S        2     NA    NA   3         3        10      NA       NA
#> 26        S        2     29   9.1   3         3        10      NA       NA
#> 27        S        2     29   9.1   3         3        10       1       NA
#> 28        S        2     29   9.1   3         3        10       1       NA
#> 29        S        2     29   9.1   3         3        10       1       NA
#> 30        S        2     29   9.1   3         3        10       1       NA
#> 31        S        2     29   9.1   3         3        10       1       NA
#> 32        S        2     26   9.7   3         3        10       1       NA
#> 33        S        2     26   9.7   3         3        10       3       NA
#> 34        S        2     26   9.7   3         3        10       3       NA
#> 35        S        2     26   9.7   3         3        10       3       NA
#> 36        S        2     26   9.7   3         3        10       3        2
#> 37        S        2     26   9.7   3         3        10       3        2
#> 38        S        2     26   9.7   3         3        10       3        2
#> 39        S        2     26   9.7   3         3        10       3        2
#> 40        S        2     26   9.7   3         3        10       3        2
#> 41        S        2     26   9.7   3         3        10       3        2
#> 42        S        2     26   9.7   3         3        10       3        2
#> 43        S        2     26   9.7   3         3        10       3        2
#> 44        S        2     26   9.7   3         3        10       3        2
#> 45        S        2     26   9.7   3         3        10       3        2
#> 46        S        2     26   9.7   3         3        10       3        2
#> 47        S        2     26   9.7   3         3        10       3        2
#> 48        S        2     26   9.7   3         3        10       3        2
#> 49        S        2     26   9.7   3         3        10       3        2
#> 50        S        2     26   9.7   3         3        10       3        2
#> 51        S        2     26   9.7   3         3        10       3        2
#> 52        S        2     26   9.7   3         3        10       3        2
#> 53        S        2     26   9.7   3         3        10       3        2
#> 54        S        2     26   9.7   3         3        10       3        2
#> 55        S        2     26   9.7   3         3        10       3        2
#> 56        S        2     26   9.7   3         3        10       3        2
#> 57        S        2     26   9.7   3         3        10       3        2
#> 58        S        2     26   9.7   3         3        10       3        2
#> 59        S        2     NA    NA  NA        NA        NA      NA       NA
#> 60        S        2     NA    NA  NA        NA        NA      NA       NA
#> 61        S        2     NA    NA   3         3        10      NA       NA
#> 62        S        2     27   9.0   3         3        10      NA       NA
#> 63        S        2     27   9.0   3         3        10       1        2
#> 64        S        2     27   9.0   3         3        10       1        2
#> 65        S        2     27   9.0   3         3        10       1        2
#> 66        S        2     27   9.0   3         3        10       1        2
#> 67        S        2     27   9.0   3         3        10       1        2
#> 68        S        2     27   9.0   3         3        10       1        2
#> 69        S        2     27   9.0   3         3        10       1        2
#> 70        S        2     27   9.0   2         3         8       1        2
#> 71        S        2     27   9.0   2         3         8       1        2
#> 72        S        2     27   9.0   2         3         8       1        2
#> 73        S        2     27   9.0   2         3         6       1        2
#> 74        S        2     23  10.0   2         3         6       1        2
#> 75        S        2     23  10.0   2         3         6       3       NA
#> 76        S        2     23  10.0   2         3         6       3       NA
#> 77        S        2     23  10.0   2         3         6       3       NA
#> 78        S        2     23  10.0   2         3         6       3       NA
#> 79        S        2     23  10.0   2         3         6       3       NA
#> 80        S        2    352   9.3   2         3         6       3       NA
#> 81        S        2    352   9.3   2         3         6       3       NA
#> 82        S        2    352   9.3   2         3         6       3       NA
#> 83        S        2    352   9.3   2         3         6       3       NA
#> 84        S        2    352   9.3   2         3         6       3       NA
#> 85        S        2    352   9.3   2         3         6       3       NA
#> 86        S        2    352   9.3   2         3         6       3       NA
#> 87        S        2    335  10.1   2         3         6       3       NA
#> 88        S        2    335  10.1   2         3         6       3       NA
#> 89        S        2    335  10.1   2         3         6       3       NA
#> 90        S        2    335  10.1   2         3         6       3       NA
#> 91        S        2    335  10.1   2         3         6       3       NA
#> 92        S        2    335  10.1   2         3         6       3       NA
#> 93        S        2    335  10.1   2         3         6       3       NA
#> 94        S        2    335  10.1   2         3         6       3       NA
#> 95        S        2    335  10.1   2         3         6       3       NA
#> 96        S        2    335  10.1   2         3         6       3       NA
#> 97        S        2    335  10.1   2         3         6       3       NA
#> 98        S        2     NA    NA  NA        NA        NA      NA       NA
#> 99        S        2     NA    NA  NA        NA        NA      NA       NA
#> 100       S        2     NA    NA   3         3         9      NA       NA
#> 101       S        2     32   9.5   3         3         9      NA       NA
#> 102       S        2     32   9.5   3         3         9       3       NA
#> 103       S        2     32   9.5   3         3         9       3       NA
#> 104       S        2     32   9.5   3         3         9       3       NA
#> 105       S        2     32   9.5   3         3         9       3       NA
#> 106       S        2     35   9.6   3         3         9       3       NA
#> 107       S        2     35   9.6   3         3         9       1       NA
#> 108       S        2     35   9.6   3         3         9       1       NA
#> 109       S        2     35   9.6   3         3         9       1       12
#> 110       S        2     35   9.6   3         3         9       1       12
#> 111       S        2     35   9.6   3         3         9       1       12
#> 112       S        2     35   9.6   3         3         9       1       12
#> 113       S        2     35   9.6   3         3         9       1       12
#> 114       S        2     35   9.6   3         3         9       1       12
#> 115       S        2     35   9.2   3         3         9       1       12
#> 116       S        2     35   9.2   3         3         9       1       12
#> 117       S        2     35   9.2   3         3         9       1       12
#> 118       S        2     35   9.2   3         3         9       1       12
#> 119       S        2     35   9.2   3         3         9       1       12
#> 120       S        2     35   9.2   3         3         9       1       12
#> 121       S        2     35   9.2   3         3         9       1       12
#> 122       S        2     35   9.2   3         3         9       1       12
#> 123       S        2     35   9.2   3         3         9       1       12
#> 124       S        2     35   9.2   3         3         9       1       12
#> 125       S        2     35   9.2   3         3         9       1       12
#> 126       S        2     NA    NA  NA        NA        NA      NA       NA
#> 127       S        2     NA    NA  NA        NA        NA      NA       NA
#> 128       S        2     NA    NA   3         3         9      NA       NA
#> 129       S        2     22   9.5   3         3         9      NA       NA
#> 130       S        2     22   9.5   3         3         9       1        8
#> 131       S        2     22   9.5   3         3         9       1        8
#> 132       S        2     22   9.5   3         3         9       1        8
#> 133       S        2     22   9.5   3         3         6       1        8
#> 134       S        2     20   9.3   3         3         6       1        8
#> 135       S        2     20   9.3   3         3         6       1        8
#> 136       S        2     20   9.3   3         3         6       1        8
#> 137       S        2     20   9.3   3         3         6       1        8
#> 138       S        2     20   9.3   3         3         6       1        8
#> 139       S        2     20   9.3   3         3         6       1        8
#> 140       S        2     20   9.3   3         3         6       1        8
#> 141       S        2     20   9.3   3         3         6       1        8
#> 142       S        2     20   9.3   3         3         6       1        8
#> 143       S        2     20   9.3   3         3         6       1        8
#> 144       S        2     20   9.3   3         3         6       1        8
#> 145       S        2     20   9.3   3         3         6       1        8
#> 146       S        2     20   9.3   3         3         6       1        8
#> 147       S        2     20   9.3   3         3         6       1        8
#> 148       S        2     20   9.3   3         3         6       1        8
#> 149       S        2     NA    NA  NA        NA        NA      NA       NA
#> 150       S        2     NA    NA  NA        NA        NA      NA       NA
#> 151       S        2     NA    NA   3         3         6      NA       NA
#> 152       S        2     17   9.3   3         3         6      NA       NA
#> 153       S        2     17   9.3   3         3         6       1        8
#> 154       S        2     17   9.3   3         3         6       1        8
#> 155       S        2     17   9.3   3         3         6       1        8
#> 156       S        2     17   9.3   3         3         6       1        8
#> 157       S        2     17   9.3   2         3         6       1        8
#> 158       S        2     16   8.9   2         3         6       1        8
#> 159       S        2     16   8.9   2         3         6       1        9
#> 160       S        2     16   8.9   2         3         6       1        9
#> 161       S        2     16   8.9   2         3         6       1        9
#> 162       S        2     16   8.9   2         3         6       1        9
#> 163       S        2     16   8.9   2         3         6       1        9
#> 164       S        2     16   8.9   2         3         6       1        9
#> 165       S        2     16   8.9   2         3         6       1        9
#> 166       S        2     NA    NA  NA        NA        NA      NA       NA
#> 167       S        2     NA    NA  NA        NA        NA      NA       NA
#> 168       S        2     NA    NA   3         3         8      NA       NA
#> 169       S        2     25   8.9   3         3         8      NA       NA
#> 170       S        2     25   8.9   3         3         8       1        8
#> 171       S        2     25   8.9   3         3         8       1        8
#> 172       S        2     25   8.9   3         3         8       1        8
#> 173       S        2     25   8.9   2         3         6       1        8
#> 174       S        2     25   8.9   2         3         6       1        8
#> 175       S        2     25   8.9   2         3         6       1        8
#> 176       S        2     25   8.9   2         3         6       1        8
#> 177       S        2     25   8.9   2         3         6       1        8
#> 178       S        2     25   8.9   2         3         6       1        8
#> 179       S        2     25   8.9   2         3         6       1        8
#> 180       S        2     25   8.9   2         3         6       1        8
#> 181       S        2     25   8.9   2         3         6       1        8
#> 182       S        2     25   8.9   2         3         6       1        8
#> 183       S        2     25   8.9   2         3         6       1        8
#> 184       S        2     25   8.9   2         3         6       1        8
#> 185       S        2     25   8.9   2         3         6       1        8
#> 186       S        2     NA    NA  NA        NA        NA      NA       NA
#> 187       S        2     NA    NA  NA        NA        NA      NA       NA
#> 188       S        2     NA    NA   2         3         6      NA       NA
#> 189       S        2     30   9.5   2         3         6      NA       NA
#> 190       S        2     30   9.5   2         3         6       1        8
#> 191       S        2     30   9.5   2         3         6       1        8
#> 192       S        2     30   9.5   2         3         6       1        8
#> 193       S        2     30   9.5   2         3         6       1        8
#> 194       S        2     30   9.5   2         3         6       1        8
#> 195       S        2     30   9.5   2         3         6       1        8
#> 196       S        2     30   9.5   2         3         6       1        8
#> 197       S        2     30   9.5   2         3         6       1        8
#> 198       S        2     30   9.5   2         3         6       1        8
#> 199       S        2     30   9.5   2         3         6       1        8
#> 200       S        2     30   9.5   2         3         6       1        8
#> 201       S        2     30   9.5   2         3         6       1        8
#> 202       S        2     30   9.5   2         3         6       1        8
#> 203       S        2     30   9.5   2         3         6       1        8
#> 204       S        2     30   9.5   2         3         6       1        8
#> 205       S        2     30   9.5   2         3         6       1        8
#> 206       S        2     30   9.5   2         3         6       1        8
#> 207       S        2     30   9.5   2         3         6       1        8
#> 208    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 209    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 210    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 211    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 212    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 213    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 214    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 215    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 216    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 217    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 218    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 219    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 220    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 221    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 222    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 223    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 224    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 225    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 226    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 227    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 228    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 229       S        2     NA    NA  NA        NA        NA      NA       NA
#> 230       S        2     NA    NA  NA        NA        NA      NA       NA
#> 231       S        2     NA    NA   2         1         5      NA       NA
#> 232       S        2     35   9.5   2         1         5      NA       NA
#> 233       S        2     35   9.5   2         1         5       3       NA
#> 234       S        2     35   9.5   2         1         5       3       NA
#> 235       S        2     35   9.5   2         1         5       3       NA
#> 236       S        2     35   9.5   2         1         5       3       NA
#> 237       S        2     35   9.5   2         1         5       3       NA
#> 238       S        2     35   9.5   2         1         5       3       NA
#> 239       S        2     NA    NA  NA        NA        NA      NA       NA
#> 240       S        2     NA    NA  NA        NA        NA      NA       NA
#> 241       S        2     NA    NA   2         1         5      NA       NA
#> 242       S        2     23   9.6   2         1         5      NA       NA
#> 243       S        2     23   9.6   2         1         5       3       NA
#> 244       S        2     23   9.6   2         1         5       3       NA
#> 245       S        2     23   9.6   2         1         5       3       NA
#> 246       S        2     23   9.6   2         1         5       3       NA
#> 247       S        2     23   9.6   2         1         5       3       NA
#> 248       S        2     23   9.6   2         1         5       3       NA
#> 249       S        2     23   9.6   2         1         5       3       NA
#> 250       S        2     23   9.6   2         1         5       3       NA
#> 251       S        2     23   9.6   2         1         5       3       NA
#> 252       S        2     23   9.6   2         1         5       3       NA
#> 253       S        2     23   9.6   2         1         5       3       NA
#> 254       S        2     23   9.6   2         1         5       3       NA
#> 255       S        2     23   9.6   2         1         5       3       NA
#> 256       S        2     23   9.6   2         1         5       3       NA
#>     VertSun Glare Vis ObsL  Rec ObsR ObsInd Data1 Data2 Data3 Data4 Data5 Data6
#> 1        NA    NA  NA <NA> <NA> <NA>   <NA>  1000     c     5     Y  <NA>  <NA>
#> 2        NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 3        NA    NA  NA  280  001  126   <NA>   280   001   126  <NA>  <NA>  <NA>
#> 4        NA    NA  NA  280  001  126   <NA>     3    03   230  <NA>  10.0  <NA>
#> 5        NA    NA  NA  280  001  126   <NA>   023  09.8  <NA>  <NA>  <NA>  <NA>
#> 6        NA    NA 6.0  280  001  126   <NA>     1  <NA>  <NA>   250   6.0  <NA>
#> 7        NA    NA 6.0  280  001  126   <NA>     3    03   230  <NA>  10.0  <NA>
#> 8         3 FALSE 6.0  280  001  126   <NA>     1    02    03   257   6.0  <NA>
#> 9        NA    NA 6.0  280  001  126   <NA>     1  <NA>  <NA>   257   6.0  <NA>
#> 10       NA    NA 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 11       NA    NA 6.0  208  280  001   <NA>   208   280   001  <NA>  <NA>  <NA>
#> 12       NA    NA 6.0  208  280  001   <NA>     3    03   230  <NA>  10.0  <NA>
#> 13       NA    NA 6.0  208  280  001   <NA>   025  10.2  <NA>  <NA>  <NA>  <NA>
#> 14       NA    NA 6.0  208  280  001   <NA>     1  <NA>  <NA>   257   6.0  <NA>
#> 15       NA    NA 6.0  208  280  001   <NA>  1406   208     3     4   309   2.8
#> 16       NA    NA 6.0  208  280  001   <NA>  1406  <NA>     N     N   018  <NA>
#> 17       NA    NA 6.0  208  280  001   <NA>   280  <NA>  <NA>    43   100  <NA>
#> 18       NA    NA 6.0  208  280  001   <NA>   001  <NA>  <NA>    36   100  <NA>
#> 19       NA    NA 6.0  208  280  001   <NA>   208  <NA>  <NA>    48   100  <NA>
#> 20       NA    NA 6.0  208  280  001   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 21       NA    NA 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 22       NA    NA 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 23       NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 24       NA    NA  NA  208  280  001   <NA>   208   280   001  <NA>  <NA>  <NA>
#> 25       NA    NA  NA  208  280  001   <NA>     3    03   230  <NA>  10.0  <NA>
#> 26       NA    NA  NA  208  280  001   <NA>   029  09.1  <NA>  <NA>  <NA>  <NA>
#> 27       NA    NA 6.0  208  280  001   <NA>     1  <NA>  <NA>   257   6.0  <NA>
#> 28       NA    NA 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 29       NA    NA 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 30       NA    NA 6.0  125  208  280   <NA>   125   208   280  <NA>  <NA>  <NA>
#> 31       NA    NA 6.0  125  208  280   <NA>     3    03   230  <NA>  10.0  <NA>
#> 32       NA    NA 6.0  125  208  280   <NA>   026  09.7  <NA>  <NA>  <NA>  <NA>
#> 33       NA    NA 5.5  125  208  280   <NA>     3  <NA>  <NA>   257   5.5  <NA>
#> 34       NA    NA 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 35       NA    NA 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 36        2 FALSE 5.5  125  208  280   <NA>     3    02    02   257   5.5  <NA>
#> 37        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 38        2 FALSE 5.5  125  208  280   <NA>  1407   125     3     4   326   0.4
#> 39        2 FALSE 5.5  125  208  280   <NA>  1407  <NA>     Y     N   076  <NA>
#> 40        2 FALSE 5.5  125  208  280   <NA>   280     6    10     6   100  <NA>
#> 41        2 FALSE 5.5  125  208  280   <NA>   001     9    10     2   100  <NA>
#> 42        2 FALSE 5.5  125  208  280   <NA>   125     9    22     9   100  <NA>
#> 43        2 FALSE 5.5  125  208  280   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 44        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 45        2 FALSE 5.5  125  208  280   <NA>  1407   011   2.0   1.3  <NA>  <NA>
#> 46        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 47        2 FALSE 5.5  125  208  280   <NA>  1407   005   3.5   0.9  <NA>  <NA>
#> 48        2 FALSE 5.5  125  208  280   <NA>  1407   050  <NA>   0.5  <NA>  <NA>
#> 49        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 50        2 FALSE 5.5  125  208  280   <NA>  1407   071   4.5   0.7   100  <NA>
#> 51        2 FALSE 5.5  125  208  280   <NA>  1407   104   4.5   0.7   100  <NA>
#> 52        2 FALSE 5.5  125  208  280   <NA>  1407   002   2.2   1.3  <NA>  <NA>
#> 53        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 54        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 55        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 56        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 57        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 58        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 59       NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 60       NA    NA  NA  001  126  149   <NA>   001   126   149  <NA>  <NA>  <NA>
#> 61       NA    NA  NA  001  126  149   <NA>     3    03   230  <NA>  10.0  <NA>
#> 62       NA    NA  NA  001  126  149   <NA>   027  09.0  <NA>  <NA>  <NA>  <NA>
#> 63        2 FALSE 5.5  001  126  149   <NA>     1    02    02   257   5.5  <NA>
#> 64        2 FALSE 5.5  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 65        2 FALSE 5.5  001  126  149   <NA>   280    LV   120  0.03     1  <NA>
#> 66        2 FALSE 5.5  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 67        2 FALSE 5.5  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 68        2 FALSE 5.5  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 69        2 FALSE 6.0  001  126  149   <NA>     1    02    02   257   6.0  <NA>
#> 70        2 FALSE 6.0  001  126  149   <NA>     2    03   230  <NA>  08.0  <NA>
#> 71        2 FALSE 6.0  001  126  149   <NA>  Well ?      <NA>  <NA>  <NA>  <NA>
#> 72        2 FALSE 6.0  280  001  126   <NA>   280   001   126  <NA>  <NA>  <NA>
#> 73        2 FALSE 6.0  280  001  126   <NA>     2    03   230  <NA>  06.0  <NA>
#> 74        2 FALSE 6.0  280  001  126   <NA>   023  10.0  <NA>  <NA>  <NA>  <NA>
#> 75       NA    NA 5.5  280  001  126   <NA>     3  <NA>  <NA>   257   5.5  <NA>
#> 76       NA    NA 5.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 77       NA    NA 5.5  280  001  126   <NA>     2    03   230  <NA>  06.0  <NA>
#> 78       NA    NA 4.5  280  001  126   <NA>     3  <NA>  <NA>   257   4.5  <NA>
#> 79       NA    NA 4.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 80       NA    NA 4.5  280  001  126   <NA>   352  09.3  <NA>  <NA>  <NA>  <NA>
#> 81       NA    NA 4.5  280  001  126   <NA>  goin g lef t to  avoid  <NA>  <NA>
#> 82       NA    NA 4.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 83       NA    NA 3.5  280  001  126   <NA>     3  <NA>  <NA>   257   3.5  <NA>
#> 84       NA    NA 2.5  280  001  126   <NA>     3  <NA>  <NA>   257   2.5  <NA>
#> 85       NA    NA 2.5  280  001  126   <NA>   280   001   126  <NA>  <NA>  <NA>
#> 86       NA    NA 2.5  280  001  126   <NA>     2    03   230  <NA>  06.0  <NA>
#> 87       NA    NA 2.5  280  001  126   <NA>   335  10.1  <NA>  <NA>  <NA>  <NA>
#> 88       NA    NA 2.5  280  001  126   <NA>     3  <NA>  <NA>   257   2.5  <NA>
#> 89       NA    NA 2.5  280  001  126   <NA>     W  <NA>  <NA>  <NA>  <NA>  <NA>
#> 90       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 91       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 92       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 93       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 94       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 95       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 96       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 97       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 98       NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 99       NA    NA  NA  125  208  280   <NA>   125   208   280  <NA>  <NA>  <NA>
#> 100      NA    NA  NA  125  208  280   <NA>     3    03   230  <NA>  09.0  <NA>
#> 101      NA    NA  NA  125  208  280   <NA>   032  09.5  <NA>  <NA>  <NA>  <NA>
#> 102      NA    NA 5.8  125  208  280   <NA>     3  <NA>  <NA>   257   5.8  <NA>
#> 103      NA    NA 5.8  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 104      NA    NA 5.8  149  125  208   <NA>   149   125   208  <NA>  <NA>  <NA>
#> 105      NA    NA 5.8  149  125  208   <NA>     3    03   230  <NA>  09.0  <NA>
#> 106      NA    NA 5.8  149  125  208   <NA>   035  09.6  <NA>  <NA>  <NA>  <NA>
#> 107      NA    NA 6.0  149  125  208   <NA>     1  <NA>  <NA>   257   6.0  <NA>
#> 108      NA    NA 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 109      12 FALSE 6.0  149  125  208   <NA>     1    12    12   257   6.0  <NA>
#> 110      12 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 111      12 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 112      12 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 113      12 FALSE 6.0  126  149  125   <NA>   126   149   125  <NA>  <NA>  <NA>
#> 114      12 FALSE 6.0  126  149  125   <NA>     3    03   230  <NA>  09.0  <NA>
#> 115      12 FALSE 6.0  126  149  125   <NA>   035  09.2  <NA>  <NA>  <NA>  <NA>
#> 116      12 FALSE 6.0  126  149  125   <NA>     1    12    12   257   6.0  <NA>
#> 117      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 118      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 119      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 120      12 FALSE 6.0  126  149  125   <NA>     U  <NA>  <NA>  <NA>  <NA>  <NA>
#> 121      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 122      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 123      12 FALSE 6.0  126  149  125   <NA>   149    DC   270  0.03     2  <NA>
#> 124      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 125      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 126      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 127      NA    NA  NA  001  126  149   <NA>   001   126   149  <NA>  <NA>  <NA>
#> 128      NA    NA  NA  001  126  149   <NA>     3    03   230  <NA>  09.0  <NA>
#> 129      NA    NA  NA  001  126  149   <NA>   022  09.5  <NA>  <NA>  <NA>  <NA>
#> 130       1 FALSE 6.0  001  126  149   <NA>     1    08    01   257   6.0  <NA>
#> 131       1 FALSE 6.0  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 132       1 FALSE 6.0  280  001  126   <NA>   280   001   126  <NA>  <NA>  <NA>
#> 133       1 FALSE 6.0  280  001  126   <NA>     3    03   230  <NA>  06.0  <NA>
#> 134       1 FALSE 6.0  280  001  126   <NA>   020  09.3  <NA>  <NA>  <NA>  <NA>
#> 135       1 FALSE 6.0  280  001  126   <NA>     1    08    01   257   6.0  <NA>
#> 136       1 FALSE 6.0  280  001  126   <NA>   228    DC   300  0.02     1  <NA>
#> 137       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 138       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 139       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 140       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 141       1 FALSE 6.0  280  001  126   <NA>  1408   280     3     4   270  14.0
#> 142       1 FALSE 6.0  280  001  126   <NA>  1408  <NA>     N     N   037  <NA>
#> 143       1 FALSE 6.0  280  001  126   <NA>   280    11    24    11   100  <NA>
#> 144       1 FALSE 6.0  280  001  126   <NA>   001    12    23    12   100  <NA>
#> 145       1 FALSE 6.0  280  001  126   <NA>   126     9    13     9   100  <NA>
#> 146       1 FALSE 6.0  280  001  126   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 147       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 148       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 149      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 150      NA    NA  NA  208  280  001   <NA>   208   280   001  <NA>  <NA>  <NA>
#> 151      NA    NA  NA  208  280  001   <NA>     3    03   230  <NA>  06.0  <NA>
#> 152      NA    NA  NA  208  280  001   <NA>   017  09.3  <NA>  <NA>  <NA>  <NA>
#> 153       1 FALSE 6.0  208  280  001   <NA>     1    08    01   257   6.0  <NA>
#> 154       1 FALSE 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 155       1 FALSE 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 156       1 FALSE 6.0  125  208  280   <NA>   125   208   280  <NA>  <NA>  <NA>
#> 157       1 FALSE 6.0  125  208  280   <NA>     2    03   230  <NA>  06.0  <NA>
#> 158       1 FALSE 6.0  125  208  280   <NA>   016  08.9  <NA>  <NA>  <NA>  <NA>
#> 159       1 FALSE 6.0  125  208  280   <NA>     1    09    01   243   6.0  <NA>
#> 160       1 FALSE 6.0  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 161       1 FALSE 6.0  125  208  280   <NA>   231    DC   045  0.05     1  <NA>
#> 162       1 FALSE 6.0  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 163       1 FALSE 6.0  125  208  280   <NA>     X  <NA>  <NA>  <NA>  <NA>  <NA>
#> 164       1 FALSE 6.0  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 165       1 FALSE 6.0  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 166      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 167      NA    NA  NA  149  125  208   <NA>   149   125   208  <NA>  <NA>  <NA>
#> 168      NA    NA  NA  149  125  208   <NA>     3    03   230  <NA>  08.0  <NA>
#> 169      NA    NA  NA  149  125  208   <NA>   025  08.9  <NA>  <NA>  <NA>  <NA>
#> 170       2 FALSE 6.0  149  125  208   <NA>     1    08    02   243   6.0  <NA>
#> 171       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 172       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 173       2 FALSE 6.0  149  125  208   <NA>     2    03   230  <NA>  06.0  <NA>
#> 174       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 175       2 FALSE 6.0  149  125  208   <NA>  1409   149     3     4   344   0.2
#> 176       2 FALSE 6.0  149  125  208   <NA>  1409  <NA>     Y     Y   016  <NA>
#> 177       2 FALSE 6.0  149  125  208   <NA>   125    46    90    46   100  <NA>
#> 178       2 FALSE 6.0  149  125  208   <NA>   149    28    65    28   100  <NA>
#> 179       2 FALSE 6.0  149  125  208   <NA>   208    66    82    66   100  <NA>
#> 180       2 FALSE 6.0  149  125  208   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 181       2 FALSE 6.0  149  125  208   <NA>  1409   356   0.4   3.0  <NA>  <NA>
#> 182       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 183       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 184       2 FALSE 6.0  149  125  208   <NA>  off  effor t aft er th e sig hting
#> 185       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 186      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 187      NA    NA  NA  126  149  125   <NA>   126   149   125  <NA>  <NA>  <NA>
#> 188      NA    NA  NA  126  149  125   <NA>     2    03   230  <NA>  06.0  <NA>
#> 189      NA    NA  NA  126  149  125   <NA>   030  09.5  <NA>  <NA>  <NA>  <NA>
#> 190       2 FALSE 6.0  126  149  125   <NA>     1    08    02   243   6.0  <NA>
#> 191       2 FALSE 6.0  126  149  125   <NA>  1410   125     3     4   070   1.4
#> 192       2 FALSE 6.0  126  149  125   <NA>  1410  <NA>     Y     N   013   016
#> 193       2 FALSE 6.0  126  149  125   <NA>   280    37    72    37    68    32
#> 194       2 FALSE 6.0  126  149  125   <NA>   125    35    74    35    75    25
#> 195       2 FALSE 6.0  126  149  125   <NA>   149    29    52    29    65    35
#> 196       2 FALSE 6.0  126  149  125   <NA>   126    66    93    66    80    20
#> 197       2 FALSE 6.0  126  149  125   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 198       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 199       2 FALSE 6.0  126  149  125   <NA>   280    DC   042  0.23     2     F
#> 200       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 201       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 202       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 203       2 FALSE 6.0  126  149  125   <NA>   099    LV   180  0.01     1  <NA>
#> 204       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 205       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 206       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 207       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 208      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 209      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 210      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 211      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 212      NA    NA  NA <NA> <NA> <NA>   <NA>  1411   280     3     1   000  <NA>
#> 213      NA    NA  NA <NA> <NA> <NA>   <NA>  1411  <NA>     N     N   075  <NA>
#> 214      NA    NA  NA <NA> <NA> <NA>   <NA>   280  <NA>  <NA>  <NA>   100  <NA>
#> 215      NA    NA  NA <NA> <NA> <NA>   <NA>  off  effor t, fi rst s een b y CO 
#> 216      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 217      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 218      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 219      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 220      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 221      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 222      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 223      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 224      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 225      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 226      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 227      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 228      NA    NA  NA <NA> <NA> <NA>   <NA>  1000     c     5     Y  <NA>  <NA>
#> 229      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 230      NA    NA  NA  149  125  208   <NA>   149   125   208  <NA>  <NA>  <NA>
#> 231      NA    NA  NA  149  125  208   <NA>     2    01   035  <NA>  05.0  <NA>
#> 232      NA    NA  NA  149  125  208   <NA>   035  09.5  <NA>  <NA>  <NA>  <NA>
#> 233      NA    NA 4.0  149  125  208   <NA>     3  <NA>  <NA>   040   4.0  <NA>
#> 234      NA    NA 4.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 235      NA    NA 4.0  149  125  208   <NA>   149   309  1.47   1.7  <NA>  <NA>
#> 236      NA    NA 4.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 237      NA    NA 4.0  149  125  208   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 238      NA    NA 4.0  149  125  208   <NA>  cros sing  fishi ng ge ar     <NA>
#> 239      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 240      NA    NA  NA  149  125  208   <NA>   149   125   208  <NA>  <NA>  <NA>
#> 241      NA    NA  NA  149  125  208   <NA>     2    01   035  <NA>  05.0  <NA>
#> 242      NA    NA  NA  149  125  208   <NA>   023  09.6  <NA>  <NA>  <NA>  <NA>
#> 243      NA    NA 4.0  149  125  208   <NA>     3  <NA>  <NA>   040   4.0  <NA>
#> 244      NA    NA 4.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 245      NA    NA 4.0  149  125  208   <NA>  1412   149     2     4   359   0.3
#> 246      NA    NA 4.0  149  125  208   <NA>  1412  <NA>     Y     N   018   277
#> 247      NA    NA 4.0  149  125  208   <NA>   149   183   328   183    80    20
#> 248      NA    NA 4.0  149  125  208   <NA>   126   120   170   120    90    10
#> 249      NA    NA 4.0  149  125  208   <NA>  1413   208     3     4   038   0.8
#> 250      NA    NA 4.0  149  125  208   <NA>  1413  <NA>     Y     N   016   277
#> 251      NA    NA 4.0  149  125  208   <NA>  1413  <NA>  <NA>  <NA>   016   016
#> 252      NA    NA 4.0  149  125  208   <NA>   125    21    60    21    60    40
#> 253      NA    NA 4.0  149  125  208   <NA>   208    16    20    16    56    44
#> 254      NA    NA 4.0  149  125  208   <NA>   149    12    18    12    70    30
#> 255      NA    NA 4.0  149  125  208   <NA>   126    36    53    36    98     2
#> 256      NA    NA 4.0  149  125  208   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#>     Data7 Data8      Data9 Data10 Data11 Data12 EffortDot EventNum
#> 1    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        1
#> 2    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        2
#> 3    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        3
#> 4    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        4
#> 5    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        5
#> 6    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        6
#> 7    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        7
#> 8    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        8
#> 9    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        9
#> 10   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       10
#> 11   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       11
#> 12   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       12
#> 13   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       13
#> 14   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       14
#> 15   1.06   013       <NA>   <NA>   <NA>   <NA>      TRUE       15
#> 16   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       16
#> 17   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 18   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 19   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 20   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       17
#> 21   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       18
#> 22   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       19
#> 23   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       20
#> 24   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       21
#> 25   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       22
#> 26   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       23
#> 27   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       24
#> 28   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       25
#> 29   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       26
#> 30   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       27
#> 31   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       28
#> 32   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       29
#> 33   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       30
#> 34   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       31
#> 35   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       32
#> 36   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       33
#> 37   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       34
#> 38   2.97   037       <NA>   <NA>   <NA>   <NA>      TRUE       35
#> 39   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       36
#> 40   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 41   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 42   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 43   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       37
#> 44   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       38
#> 45   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       39
#> 46   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       40
#> 47   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       41
#> 48   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       42
#> 49   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       43
#> 50   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       44
#> 51   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       45
#> 52   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       46
#> 53   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       47
#> 54   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       48
#> 55   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       49
#> 56   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       50
#> 57   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       51
#> 58   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       52
#> 59   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       53
#> 60   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       54
#> 61   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       55
#> 62   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       56
#> 63   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       57
#> 64   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       58
#> 65   <NA>     a          n   <NA>   <NA>   <NA>      TRUE       59
#> 66   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       60
#> 67   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       61
#> 68   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       62
#> 69   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       63
#> 70   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       64
#> 71   <NA>  <NA>              <NA>   <NA>   <NA>      TRUE       65
#> 72   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       66
#> 73   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       67
#> 74   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       68
#> 75   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       69
#> 76   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       70
#> 77   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       71
#> 78   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       72
#> 79   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       73
#> 80   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       74
#> 81   <NA>  <NA>              <NA>   <NA>   <NA>      TRUE       75
#> 82   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       77
#> 83   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       78
#> 84   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       79
#> 85   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       80
#> 86   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       81
#> 87   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       82
#> 88   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       83
#> 89   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       84
#> 90   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       85
#> 91   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       86
#> 92   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       87
#> 93   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       88
#> 94   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       89
#> 95   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       90
#> 96   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       91
#> 97   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       92
#> 98   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       93
#> 99   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       94
#> 100  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       95
#> 101  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       96
#> 102  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       97
#> 103  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       98
#> 104  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       99
#> 105  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      100
#> 106  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      101
#> 107  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      102
#> 108  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      103
#> 109  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      104
#> 110  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      105
#> 111  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      106
#> 112  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      107
#> 113  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      108
#> 114  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      109
#> 115  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      110
#> 116  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      111
#> 117  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      112
#> 118  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      113
#> 119  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      114
#> 120  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      115
#> 121  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      116
#> 122  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      117
#> 123  <NA>     a          n   <NA>   <NA>   <NA>     FALSE      118
#> 124  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      119
#> 125  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      120
#> 126  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      121
#> 127  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      122
#> 128  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      123
#> 129  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      124
#> 130  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      125
#> 131  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      126
#> 132  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      127
#> 133  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      128
#> 134  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      129
#> 135  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      130
#> 136  <NA>     j          n   <NA>   <NA>   <NA>      TRUE      131
#> 137  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      132
#> 138  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      133
#> 139  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      134
#> 140  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      135
#> 141  0.28   015       <NA>   <NA>   <NA>   <NA>      TRUE      136
#> 142  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      137
#> 143  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 144  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 145  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 146  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      138
#> 147  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      139
#> 148  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      140
#> 149  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      141
#> 150  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      142
#> 151  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      143
#> 152  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      144
#> 153  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      145
#> 154  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      146
#> 155  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      147
#> 156  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      148
#> 157  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      149
#> 158  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      150
#> 159  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      151
#> 160  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      152
#> 161  <NA>     a       <NA>   <NA>   <NA>   <NA>      TRUE      153
#> 162  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      154
#> 163  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      155
#> 164  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      156
#> 165  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      157
#> 166  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      158
#> 167  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      159
#> 168  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      160
#> 169  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      161
#> 170  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      162
#> 171  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      163
#> 172  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      164
#> 173  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      165
#> 174  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      166
#> 175  3.68   002       <NA>   <NA>   <NA>   <NA>      TRUE      167
#> 176  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      168
#> 177  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 178  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 179  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 180  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      169
#> 181  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      170
#> 182  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      171
#> 183  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      172
#> 184  <NA>  <NA>              <NA>   <NA>   <NA>     FALSE      174
#> 185  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      175
#> 186  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      176
#> 187  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      177
#> 188  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      178
#> 189  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      179
#> 190  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      180
#> 191  1.66   036       <NA>   <NA>   <NA>   <NA>      TRUE      181
#> 192  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      182
#> 193  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 194  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 195  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 196  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 197  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      183
#> 198  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      185
#> 199  17.0     A          Y   <NA>   <NA>   <NA>     FALSE      186
#> 200  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      187
#> 201  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      188
#> 202  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      189
#> 203  <NA>     A          N   <NA>   <NA>   <NA>     FALSE      190
#> 204  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      191
#> 205  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      192
#> 206  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      193
#> 207  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      194
#> 208  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      195
#> 209  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      196
#> 210  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      197
#> 211  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      198
#> 212  0.00   018       <NA>   <NA>   <NA>   <NA>     FALSE      199
#> 213  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      200
#> 214  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 215 while  ridi ng bow      w       <NA>   <NA>     FALSE      201
#> 216  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      202
#> 217  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      203
#> 218  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      204
#> 219  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      205
#> 220  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      206
#> 221  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      207
#> 222  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      208
#> 223  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      209
#> 224  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      210
#> 225  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      211
#> 226  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      212
#> 227  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      213
#> 228  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        1
#> 229  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        2
#> 230  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        3
#> 231  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        4
#> 232  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        5
#> 233  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        6
#> 234  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        7
#> 235  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        8
#> 236  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        9
#> 237  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       10
#> 238  <NA>  <NA>              <NA>   <NA>   <NA>     FALSE       11
#> 239  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       12
#> 240  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       13
#> 241  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       14
#> 242  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       15
#> 243  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       16
#> 244  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       17
#> 245  3.28   018       <NA>   <NA>   <NA>   <NA>      TRUE       18
#> 246  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       19
#> 247  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 248  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 249  2.23   018       <NA>   <NA>   <NA>   <NA>      TRUE       20
#> 250  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       21
#> 251  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 252  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 253  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 254  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 255  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 256  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       22
#>           file_das line_num
#> 1   das_sample.das        1
#> 2   das_sample.das        2
#> 3   das_sample.das        3
#> 4   das_sample.das        4
#> 5   das_sample.das        5
#> 6   das_sample.das        6
#> 7   das_sample.das        7
#> 8   das_sample.das        8
#> 9   das_sample.das        9
#> 10  das_sample.das       10
#> 11  das_sample.das       11
#> 12  das_sample.das       12
#> 13  das_sample.das       13
#> 14  das_sample.das       14
#> 15  das_sample.das       15
#> 16  das_sample.das       16
#> 17  das_sample.das       17
#> 18  das_sample.das       18
#> 19  das_sample.das       19
#> 20  das_sample.das       20
#> 21  das_sample.das       21
#> 22  das_sample.das       22
#> 23  das_sample.das       23
#> 24  das_sample.das       24
#> 25  das_sample.das       25
#> 26  das_sample.das       26
#> 27  das_sample.das       27
#> 28  das_sample.das       28
#> 29  das_sample.das       29
#> 30  das_sample.das       30
#> 31  das_sample.das       31
#> 32  das_sample.das       32
#> 33  das_sample.das       33
#> 34  das_sample.das       34
#> 35  das_sample.das       35
#> 36  das_sample.das       36
#> 37  das_sample.das       37
#> 38  das_sample.das       38
#> 39  das_sample.das       39
#> 40  das_sample.das       40
#> 41  das_sample.das       41
#> 42  das_sample.das       42
#> 43  das_sample.das       43
#> 44  das_sample.das       44
#> 45  das_sample.das       45
#> 46  das_sample.das       46
#> 47  das_sample.das       47
#> 48  das_sample.das       48
#> 49  das_sample.das       49
#> 50  das_sample.das       50
#> 51  das_sample.das       51
#> 52  das_sample.das       52
#> 53  das_sample.das       53
#> 54  das_sample.das       54
#> 55  das_sample.das       55
#> 56  das_sample.das       56
#> 57  das_sample.das       57
#> 58  das_sample.das       58
#> 59  das_sample.das       59
#> 60  das_sample.das       60
#> 61  das_sample.das       61
#> 62  das_sample.das       62
#> 63  das_sample.das       63
#> 64  das_sample.das       64
#> 65  das_sample.das       65
#> 66  das_sample.das       66
#> 67  das_sample.das       67
#> 68  das_sample.das       68
#> 69  das_sample.das       69
#> 70  das_sample.das       70
#> 71  das_sample.das       71
#> 72  das_sample.das       72
#> 73  das_sample.das       73
#> 74  das_sample.das       74
#> 75  das_sample.das       75
#> 76  das_sample.das       76
#> 77  das_sample.das       77
#> 78  das_sample.das       78
#> 79  das_sample.das       79
#> 80  das_sample.das       80
#> 81  das_sample.das       81
#> 82  das_sample.das       83
#> 83  das_sample.das       84
#> 84  das_sample.das       85
#> 85  das_sample.das       86
#> 86  das_sample.das       87
#> 87  das_sample.das       88
#> 88  das_sample.das       89
#> 89  das_sample.das       90
#> 90  das_sample.das       91
#> 91  das_sample.das       92
#> 92  das_sample.das       93
#> 93  das_sample.das       94
#> 94  das_sample.das       95
#> 95  das_sample.das       96
#> 96  das_sample.das       97
#> 97  das_sample.das       98
#> 98  das_sample.das       99
#> 99  das_sample.das      100
#> 100 das_sample.das      101
#> 101 das_sample.das      102
#> 102 das_sample.das      103
#> 103 das_sample.das      104
#> 104 das_sample.das      105
#> 105 das_sample.das      106
#> 106 das_sample.das      107
#> 107 das_sample.das      108
#> 108 das_sample.das      109
#> 109 das_sample.das      110
#> 110 das_sample.das      111
#> 111 das_sample.das      112
#> 112 das_sample.das      113
#> 113 das_sample.das      114
#> 114 das_sample.das      115
#> 115 das_sample.das      116
#> 116 das_sample.das      117
#> 117 das_sample.das      118
#> 118 das_sample.das      119
#> 119 das_sample.das      120
#> 120 das_sample.das      121
#> 121 das_sample.das      122
#> 122 das_sample.das      123
#> 123 das_sample.das      124
#> 124 das_sample.das      125
#> 125 das_sample.das      126
#> 126 das_sample.das      127
#> 127 das_sample.das      128
#> 128 das_sample.das      129
#> 129 das_sample.das      130
#> 130 das_sample.das      131
#> 131 das_sample.das      132
#> 132 das_sample.das      133
#> 133 das_sample.das      134
#> 134 das_sample.das      135
#> 135 das_sample.das      136
#> 136 das_sample.das      137
#> 137 das_sample.das      138
#> 138 das_sample.das      139
#> 139 das_sample.das      140
#> 140 das_sample.das      141
#> 141 das_sample.das      142
#> 142 das_sample.das      143
#> 143 das_sample.das      144
#> 144 das_sample.das      145
#> 145 das_sample.das      146
#> 146 das_sample.das      147
#> 147 das_sample.das      148
#> 148 das_sample.das      149
#> 149 das_sample.das      150
#> 150 das_sample.das      151
#> 151 das_sample.das      152
#> 152 das_sample.das      153
#> 153 das_sample.das      154
#> 154 das_sample.das      155
#> 155 das_sample.das      156
#> 156 das_sample.das      157
#> 157 das_sample.das      158
#> 158 das_sample.das      159
#> 159 das_sample.das      160
#> 160 das_sample.das      161
#> 161 das_sample.das      162
#> 162 das_sample.das      163
#> 163 das_sample.das      164
#> 164 das_sample.das      165
#> 165 das_sample.das      166
#> 166 das_sample.das      167
#> 167 das_sample.das      168
#> 168 das_sample.das      169
#> 169 das_sample.das      170
#> 170 das_sample.das      171
#> 171 das_sample.das      172
#> 172 das_sample.das      173
#> 173 das_sample.das      174
#> 174 das_sample.das      175
#> 175 das_sample.das      176
#> 176 das_sample.das      177
#> 177 das_sample.das      178
#> 178 das_sample.das      179
#> 179 das_sample.das      180
#> 180 das_sample.das      181
#> 181 das_sample.das      182
#> 182 das_sample.das      183
#> 183 das_sample.das      184
#> 184 das_sample.das      186
#> 185 das_sample.das      187
#> 186 das_sample.das      188
#> 187 das_sample.das      189
#> 188 das_sample.das      190
#> 189 das_sample.das      191
#> 190 das_sample.das      192
#> 191 das_sample.das      193
#> 192 das_sample.das      194
#> 193 das_sample.das      195
#> 194 das_sample.das      196
#> 195 das_sample.das      197
#> 196 das_sample.das      198
#> 197 das_sample.das      199
#> 198 das_sample.das      201
#> 199 das_sample.das      202
#> 200 das_sample.das      203
#> 201 das_sample.das      204
#> 202 das_sample.das      205
#> 203 das_sample.das      206
#> 204 das_sample.das      207
#> 205 das_sample.das      208
#> 206 das_sample.das      209
#> 207 das_sample.das      210
#> 208 das_sample.das      211
#> 209 das_sample.das      212
#> 210 das_sample.das      213
#> 211 das_sample.das      214
#> 212 das_sample.das      215
#> 213 das_sample.das      216
#> 214 das_sample.das      217
#> 215 das_sample.das      218
#> 216 das_sample.das      219
#> 217 das_sample.das      220
#> 218 das_sample.das      221
#> 219 das_sample.das      222
#> 220 das_sample.das      223
#> 221 das_sample.das      224
#> 222 das_sample.das      225
#> 223 das_sample.das      226
#> 224 das_sample.das      227
#> 225 das_sample.das      228
#> 226 das_sample.das      229
#> 227 das_sample.das      230
#> 228 das_sample.das      231
#> 229 das_sample.das      232
#> 230 das_sample.das      233
#> 231 das_sample.das      234
#> 232 das_sample.das      235
#> 233 das_sample.das      236
#> 234 das_sample.das      237
#> 235 das_sample.das      238
#> 236 das_sample.das      239
#> 237 das_sample.das      240
#> 238 das_sample.das      241
#> 239 das_sample.das      242
#> 240 das_sample.das      243
#> 241 das_sample.das      244
#> 242 das_sample.das      245
#> 243 das_sample.das      246
#> 244 das_sample.das      247
#> 245 das_sample.das      248
#> 246 das_sample.das      249
#> 247 das_sample.das      250
#> 248 das_sample.das      251
#> 249 das_sample.das      252
#> 250 das_sample.das      253
#> 251 das_sample.das      254
#> 252 das_sample.das      255
#> 253 das_sample.das      256
#> 254 das_sample.das      257
#> 255 das_sample.das      258
#> 256 das_sample.das      259

y.read <- das_read(y)
das_process(y.read)
#>     Event            DateTime      Lat       Lon OnEffort Cruise Mode OffsetGMT
#> 1       B 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 2       R 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 3       P 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 4       V 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 5       N 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 6       W 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 7       V 2013-01-13 06:29:56 39.32583 -137.6018     TRUE   1000    C         5
#> 8       W 2013-01-13 06:30:10 39.32650 -137.6015     TRUE   1000    C         5
#> 9       W 2013-01-13 06:34:01 39.33600 -137.5970     TRUE   1000    C         5
#> 10      * 2013-01-13 06:37:25 39.34450 -137.5927     TRUE   1000    C         5
#> 11      P 2013-01-13 06:41:08 39.35400 -137.5880     TRUE   1000    C         5
#> 12      V 2013-01-13 06:41:08 39.35400 -137.5880     TRUE   1000    C         5
#> 13      N 2013-01-13 06:41:08 39.35400 -137.5880     TRUE   1000    C         5
#> 14      W 2013-01-13 06:41:08 39.35400 -137.5880     TRUE   1000    C         5
#> 15      S 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 16      A 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 17      1 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 18      2 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 19      3 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 20      E 2013-01-13 06:46:25 39.36717 -137.5817    FALSE   1000    C         5
#> 21      * 2013-01-13 06:47:25 39.36967 -137.5807    FALSE   1000    C         5
#> 22      * 2013-01-13 06:57:25 39.37467 -137.5987    FALSE   1000    C         5
#> 23      R 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 24      P 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 25      V 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 26      N 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 27      W 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 28      * 2013-01-13 07:07:25 39.39883 -137.5868     TRUE   1000    C         5
#> 29      * 2013-01-13 07:17:25 39.42317 -137.5747     TRUE   1000    C         5
#> 30      P 2013-01-13 07:20:02 39.42950 -137.5715     TRUE   1000    C         5
#> 31      V 2013-01-13 07:20:02 39.42950 -137.5715     TRUE   1000    C         5
#> 32      N 2013-01-13 07:20:02 39.42950 -137.5715     TRUE   1000    C         5
#> 33      W 2013-01-13 07:20:02 39.42950 -137.5715     TRUE   1000    C         5
#> 34      * 2013-01-13 07:27:25 39.44733 -137.5627     TRUE   1000    C         5
#> 35      * 2013-01-13 07:37:25 39.47133 -137.5507     TRUE   1000    C         5
#> 36      W 2013-01-13 07:38:06 39.47300 -137.5500     TRUE   1000    C         5
#> 37      * 2013-01-13 07:47:25 39.49567 -137.5390     TRUE   1000    C         5
#> 38      S 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 39      A 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 40      1 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 41      2 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 42      3 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 43      E 2013-01-13 07:57:05 39.51933 -137.5277    FALSE   1000    C         5
#> 44      * 2013-01-13 07:57:25 39.52017 -137.5272    FALSE   1000    C         5
#> 45      s 2013-01-13 08:06:00 39.54217 -137.5263    FALSE   1000    C         5
#> 46      * 2013-01-13 08:07:25 39.54583 -137.5262    FALSE   1000    C         5
#> 47      s 2013-01-13 08:08:58 39.55000 -137.5255    FALSE   1000    C         5
#> 48      s 2013-01-13 08:15:32 39.56000 -137.5218    FALSE   1000    C         5
#> 49      * 2013-01-13 08:17:25 39.56233 -137.5210    FALSE   1000    C         5
#> 50      s 2013-01-13 08:17:28 39.56233 -137.5210    FALSE   1000    C         5
#> 51      s 2013-01-13 08:19:39 39.56483 -137.5197    FALSE   1000    C         5
#> 52      s 2013-01-13 08:26:45 39.56400 -137.5140    FALSE   1000    C         5
#> 53      * 2013-01-13 08:27:25 39.56333 -137.5133    FALSE   1000    C         5
#> 54      * 2013-01-13 08:37:25 39.54867 -137.5007    FALSE   1000    C         5
#> 55      * 2013-01-13 08:47:25 39.54533 -137.4807    FALSE   1000    C         5
#> 56      * 2013-01-13 08:57:25 39.54433 -137.4717    FALSE   1000    C         5
#> 57      * 2013-01-13 09:07:25 39.54333 -137.4627    FALSE   1000    C         5
#> 58      * 2013-01-13 09:17:25 39.55833 -137.4573    FALSE   1000    C         5
#> 59      R 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 60      P 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 61      V 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 62      N 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 63      W 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 64      * 2013-01-13 09:27:25 39.57983 -137.4475     TRUE   1000    C         5
#> 65      t 2013-01-13 09:34:27 39.59733 -137.4400     TRUE   1000    C         5
#> 66      * 2013-01-13 09:37:25 39.60467 -137.4368     TRUE   1000    C         5
#> 67      * 2013-01-13 09:47:25 39.62983 -137.4262     TRUE   1000    C         5
#> 68      * 2013-01-13 09:57:25 39.65517 -137.4155     TRUE   1000    C         5
#> 69      W 2013-01-13 09:59:38 39.66083 -137.4132     TRUE   1000    C         5
#> 70      V 2013-01-13 09:59:50 39.66133 -137.4130     TRUE   1000    C         5
#> 71      C 2013-01-13 10:04:23 39.67300 -137.4083     TRUE   1000    C         5
#> 72      P 2013-01-13 10:04:35 39.67350 -137.4080     TRUE   1000    C         5
#> 73      V 2013-01-13 10:04:35 39.67350 -137.4080     TRUE   1000    C         5
#> 74      N 2013-01-13 10:04:35 39.67350 -137.4080     TRUE   1000    C         5
#> 75      W 2013-01-13 10:04:35 39.67350 -137.4080     TRUE   1000    C         5
#> 76      * 2013-01-13 10:07:25 39.68083 -137.4050     TRUE   1000    C         5
#> 77      V 2013-01-13 10:11:00 39.69000 -137.4012     TRUE   1000    C         5
#> 78      W 2013-01-13 10:11:09 39.69050 -137.4010     TRUE   1000    C         5
#> 79      * 2013-01-13 10:17:25 39.70650 -137.3943     TRUE   1000    C         5
#> 80      N 2013-01-13 10:20:38 39.71483 -137.3920     TRUE   1000    C         5
#> 81      C 2013-01-13 10:20:43 39.71483 -137.3920     TRUE   1000    C         5
#> 82      * 2013-01-13 10:27:25 39.73150 -137.3993     TRUE   1000    C         5
#> 83      W 2013-01-13 10:30:28 39.73917 -137.4032     TRUE   1000    C         5
#> 84      W 2013-01-13 10:35:14 39.75117 -137.4092     TRUE   1000    C         5
#> 85      P 2013-01-13 10:36:06 39.75350 -137.4103     TRUE   1000    C         5
#> 86      V 2013-01-13 10:36:06 39.75350 -137.4103     TRUE   1000    C         5
#> 87      N 2013-01-13 10:36:06 39.75350 -137.4103     TRUE   1000    C         5
#> 88      W 2013-01-13 10:36:06 39.75350 -137.4103     TRUE   1000    C         5
#> 89      E 2013-01-13 10:36:27 39.75433 -137.4107    FALSE   1000    C         5
#> 90      * 2013-01-13 10:37:25 39.75683 -137.4118    FALSE   1000    C         5
#> 91      * 2013-01-13 10:47:25 39.78450 -137.4093    FALSE   1000    C         5
#> 92      * 2013-01-13 10:57:25 39.81250 -137.4087    FALSE   1000    C         5
#> 93      * 2013-01-13 11:07:25 39.83983 -137.4100    FALSE   1000    C         5
#> 94      * 2013-01-13 11:17:25 39.86700 -137.4110    FALSE   1000    C         5
#> 95      * 2013-01-13 11:27:25 39.89000 -137.4000    FALSE   1000    C         5
#> 96      * 2013-01-13 11:37:25 39.91233 -137.3875    FALSE   1000    C         5
#> 97      * 2013-01-13 11:47:25 39.93500 -137.3750    FALSE   1000    C         5
#> 98      R 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 99      P 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 100     V 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 101     N 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 102     W 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 103     * 2013-01-13 11:57:25 39.95767 -137.3613     TRUE   1000    C         5
#> 104     P 2013-01-13 12:02:29 39.96900 -137.3542     TRUE   1000    C         5
#> 105     V 2013-01-13 12:02:29 39.96900 -137.3542     TRUE   1000    C         5
#> 106     N 2013-01-13 12:02:29 39.96900 -137.3542     TRUE   1000    C         5
#> 107     W 2013-01-13 12:02:29 39.96900 -137.3542     TRUE   1000    C         5
#> 108     * 2013-01-13 12:07:25 39.97983 -137.3472     TRUE   1000    C         5
#> 109     W 2013-01-13 12:11:06 39.98767 -137.3418     TRUE   1000    C         5
#> 110     * 2013-01-13 12:17:25 40.00117 -137.3332     TRUE   1000    C         5
#> 111     * 2013-01-13 12:27:25 40.02183 -137.3197     TRUE   1000    C         5
#> 112     * 2013-01-13 12:37:25 40.04317 -137.3060     TRUE   1000    C         5
#> 113     P 2013-01-13 12:43:14 40.05567 -137.2978     TRUE   1000    C         5
#> 114     V 2013-01-13 12:43:14 40.05567 -137.2978     TRUE   1000    C         5
#> 115     N 2013-01-13 12:43:14 40.05567 -137.2978     TRUE   1000    C         5
#> 116     W 2013-01-13 12:43:14 40.05567 -137.2978     TRUE   1000    C         5
#> 117     * 2013-01-13 12:47:25 40.06450 -137.2920     TRUE   1000    C         5
#> 118     * 2013-01-13 12:57:25 40.08583 -137.2777     TRUE   1000    C         5
#> 119     * 2013-01-13 13:07:25 40.10750 -137.2627     TRUE   1000    C         5
#> 120     E 2013-01-13 13:16:38 40.12750 -137.2487    FALSE   1000    C         5
#> 121     * 2013-01-13 13:17:25 40.12917 -137.2477    FALSE   1000    C         5
#> 122     * 2013-01-13 13:27:25 40.13817 -137.2248    FALSE   1000    C         5
#> 123     t 2013-01-13 13:35:18 40.14000 -137.2048    FALSE   1000    C         5
#> 124     * 2013-01-13 13:37:25 40.14100 -137.1993    FALSE   1000    C         5
#> 125     * 2013-01-13 13:47:25 40.14717 -137.1782    FALSE   1000    C         5
#> 126     R 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 127     P 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 128     V 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 129     N 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 130     W 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 131     * 2013-01-13 13:57:25 40.16967 -137.1670     TRUE   1000    C         5
#> 132     P 2013-01-13 14:00:31 40.17700 -137.1642     TRUE   1000    C         5
#> 133     V 2013-01-13 14:00:31 40.17700 -137.1642     TRUE   1000    C         5
#> 134     N 2013-01-13 14:00:31 40.17700 -137.1642     TRUE   1000    C         5
#> 135     W 2013-01-13 14:00:31 40.17700 -137.1642     TRUE   1000    C         5
#> 136     t 2013-01-13 14:02:55 40.18283 -137.1622     TRUE   1000    C         5
#> 137     * 2013-01-13 14:07:25 40.19333 -137.1583     TRUE   1000    C         5
#> 138     * 2013-01-13 14:17:25 40.21667 -137.1505     TRUE   1000    C         5
#> 139     * 2013-01-13 14:27:25 40.24017 -137.1432     TRUE   1000    C         5
#> 140     * 2013-01-13 14:37:25 40.26417 -137.1353     TRUE   1000    C         5
#> 141     S 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 142     A 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 143     1 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 144     2 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 145     3 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 146     E 2013-01-13 14:38:13 40.26617 -137.1348    FALSE   1000    C         5
#> 147     * 2013-01-13 14:47:25 40.26317 -137.1353    FALSE   1000    C         5
#> 148     * 2013-01-13 14:57:25 40.26383 -137.1282    FALSE   1000    C         5
#> 149     R 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 150     P 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 151     V 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 152     N 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 153     W 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 154     * 2013-01-13 15:07:25 40.28850 -137.1205     TRUE   1000    C         5
#> 155     * 2013-01-13 15:17:25 40.31300 -137.1132     TRUE   1000    C         5
#> 156     P 2013-01-13 15:20:26 40.32033 -137.1108     TRUE   1000    C         5
#> 157     V 2013-01-13 15:20:26 40.32033 -137.1108     TRUE   1000    C         5
#> 158     N 2013-01-13 15:20:26 40.32033 -137.1108     TRUE   1000    C         5
#> 159     W 2013-01-13 15:20:26 40.32033 -137.1108     TRUE   1000    C         5
#> 160     * 2013-01-13 15:27:25 40.33700 -137.1057     TRUE   1000    C         5
#> 161     t 2013-01-13 15:36:47 40.36050 -137.0978     TRUE   1000    C         5
#> 162     * 2013-01-13 15:37:25 40.36217 -137.0973     TRUE   1000    C         5
#> 163     E 2013-01-13 15:43:08 40.37600 -137.0915    FALSE   1000    C         5
#> 164     * 2013-01-13 15:47:25 40.37933 -137.0927    FALSE   1000    C         5
#> 165     * 2013-01-13 15:57:25 40.37950 -137.0990    FALSE   1000    C         5
#> 166     R 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 167     P 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 168     V 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 169     N 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 170     W 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 171     * 2013-01-13 16:07:25 40.40200 -137.0885     TRUE   1000    C         5
#> 172     * 2013-01-13 16:17:25 40.42383 -137.0777     TRUE   1000    C         5
#> 173     V 2013-01-13 16:20:02 40.42967 -137.0745     TRUE   1000    C         5
#> 174     * 2013-01-13 16:27:25 40.44600 -137.0658     TRUE   1000    C         5
#> 175     S 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 176     A 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 177     1 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 178     2 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 179     3 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 180     E 2013-01-13 16:29:50 40.45133 -137.0628    FALSE   1000    C         5
#> 181     s 2013-01-13 16:36:34 40.46667 -137.0570    FALSE   1000    C         5
#> 182     * 2013-01-13 16:37:25 40.46867 -137.0567    FALSE   1000    C         5
#> 183     * 2013-01-13 16:47:25 40.49250 -137.0563    FALSE   1000    C         5
#> 184     C 2013-01-13 16:55:27 40.51250 -137.0593    FALSE   1000    C         5
#> 185     * 2013-01-13 16:57:25 40.51650 -137.0567    FALSE   1000    C         5
#> 186     R 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 187     P 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 188     V 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 189     N 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 190     W 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 191     S 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 192     A 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 193     1 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 194     2 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 195     3 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 196     4 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 197     E 2013-01-13 17:01:21 40.52533 -137.0515    FALSE   1000    C         5
#> 198     * 2013-01-13 17:07:25 40.52167 -137.0392    FALSE   1000    C         5
#> 199     t 2013-01-13 17:08:11 40.52083 -137.0383    FALSE   1000    C         5
#> 200     * 2013-01-13 17:17:25 40.51983 -137.0377    FALSE   1000    C         5
#> 201     * 2013-01-13 17:27:25 40.51750 -137.0427    FALSE   1000    C         5
#> 202     * 2013-01-13 17:37:25 40.50750 -137.0418    FALSE   1000    C         5
#> 203     t 2013-01-13 17:45:10 40.50450 -137.0437    FALSE   1000    C         5
#> 204     * 2013-01-13 17:47:32 40.50383 -137.0443    FALSE   1000    C         5
#> 205     * 2013-01-13 17:57:32 40.50033 -137.0472    FALSE   1000    C         5
#> 206     * 2013-01-13 18:07:32 40.50183 -137.0530    FALSE   1000    C         5
#> 207     * 2013-01-13 18:17:32 40.50567 -137.0432    FALSE   1000    C         5
#> 208     * 2013-01-14 08:45:06 40.70300 -135.8103    FALSE   1000 <NA>         5
#> 209     * 2013-01-14 08:55:06 40.71917 -135.7988    FALSE   1000 <NA>         5
#> 210     * 2013-01-14 09:05:06 40.73533 -135.7875    FALSE   1000 <NA>         5
#> 211     * 2013-01-14 09:15:06 40.74917 -135.7770    FALSE   1000 <NA>         5
#> 212     S 2013-01-14 09:17:21 40.75183 -135.7748    FALSE   1000 <NA>         5
#> 213     A 2013-01-14 09:17:21 40.75183 -135.7748    FALSE   1000 <NA>         5
#> 214     1 2013-01-14 09:17:21 40.75183 -135.7748    FALSE   1000 <NA>         5
#> 215     C 2013-01-14 09:18:04 40.75283 -135.7742    FALSE   1000 <NA>         5
#> 216     * 2013-01-14 09:25:06 40.76233 -135.7655    FALSE   1000 <NA>         5
#> 217     * 2013-01-14 09:35:06 40.76700 -135.7470    FALSE   1000 <NA>         5
#> 218     * 2013-01-14 09:45:06 40.77450 -135.7313    FALSE   1000 <NA>         5
#> 219     * 2013-01-14 09:55:06 40.78867 -135.7203    FALSE   1000 <NA>         5
#> 220     * 2013-01-14 10:05:06 40.80750 -135.7092    FALSE   1000 <NA>         5
#> 221     * 2013-01-14 10:15:06 40.82933 -135.6968    FALSE   1000 <NA>         5
#> 222     * 2013-01-14 10:25:06 40.85200 -135.6843    FALSE   1000 <NA>         5
#> 223     * 2013-01-14 10:35:06 40.87550 -135.6703    FALSE   1000 <NA>         5
#> 224     * 2013-01-14 10:45:06 40.89883 -135.6565    FALSE   1000 <NA>         5
#> 225     * 2013-01-14 10:55:06 40.92183 -135.6417    FALSE   1000 <NA>         5
#> 226     * 2013-01-14 11:05:06 40.94400 -135.6273    FALSE   1000 <NA>         5
#> 227     * 2013-01-14 11:15:06 40.96633 -135.6123    FALSE   1000 <NA>         5
#> 228     B 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 229     R 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 230     P 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 231     V 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 232     N 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 233     W 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 234     * 2013-01-14 11:25:06 40.98833 -135.5972     TRUE   1000    C         5
#> 235     F 2013-01-14 11:25:32 40.98950 -135.5965     TRUE   1000    C         5
#> 236     * 2013-01-14 11:35:06 41.01067 -135.5817     TRUE   1000    C         5
#> 237     E 2013-01-14 11:37:24 41.01583 -135.5782    FALSE   1000    C         5
#> 238     C 2013-01-14 11:37:27 41.01600 -135.5780    FALSE   1000    C         5
#> 239     R 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 240     P 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 241     V 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 242     N 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 243     W 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 244     * 2013-01-14 11:45:06 41.03400 -135.5678     TRUE   1000    C         5
#> 245     S 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 246     A 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 247     1 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 248     2 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 249     S 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 250     A 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 251     ? 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 252     1 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 253     2 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 254     3 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 255     4 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 256     E 2013-01-14 11:50:29 41.04600 -135.5595    FALSE   1000    C         5
#>     EffType ESWsides Course SpdKt Bft SwellHght WindSpdKt RainFog HorizSun
#> 1      <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 2         S        2     NA    NA  NA        NA        NA      NA       NA
#> 3         S        2     NA    NA  NA        NA        NA      NA       NA
#> 4         S        2     NA    NA   3         3        10      NA       NA
#> 5         S        2     23   9.8   3         3        10      NA       NA
#> 6         S        2     23   9.8   3         3        10       1       NA
#> 7         S        2     23   9.8   3         3        10       1       NA
#> 8         S        2     23   9.8   3         3        10       1        2
#> 9         S        2     23   9.8   3         3        10       1       NA
#> 10        S        2     23   9.8   3         3        10       1       NA
#> 11        S        2     23   9.8   3         3        10       1       NA
#> 12        S        2     23   9.8   3         3        10       1       NA
#> 13        S        2     25  10.2   3         3        10       1       NA
#> 14        S        2     25  10.2   3         3        10       1       NA
#> 15        S        2     25  10.2   3         3        10       1       NA
#> 16        S        2     25  10.2   3         3        10       1       NA
#> 17        S        2     25  10.2   3         3        10       1       NA
#> 18        S        2     25  10.2   3         3        10       1       NA
#> 19        S        2     25  10.2   3         3        10       1       NA
#> 20        S        2     25  10.2   3         3        10       1       NA
#> 21        S        2     25  10.2   3         3        10       1       NA
#> 22        S        2     25  10.2   3         3        10       1       NA
#> 23        S        2     NA    NA  NA        NA        NA      NA       NA
#> 24        S        2     NA    NA  NA        NA        NA      NA       NA
#> 25        S        2     NA    NA   3         3        10      NA       NA
#> 26        S        2     29   9.1   3         3        10      NA       NA
#> 27        S        2     29   9.1   3         3        10       1       NA
#> 28        S        2     29   9.1   3         3        10       1       NA
#> 29        S        2     29   9.1   3         3        10       1       NA
#> 30        S        2     29   9.1   3         3        10       1       NA
#> 31        S        2     29   9.1   3         3        10       1       NA
#> 32        S        2     26   9.7   3         3        10       1       NA
#> 33        S        2     26   9.7   3         3        10       3       NA
#> 34        S        2     26   9.7   3         3        10       3       NA
#> 35        S        2     26   9.7   3         3        10       3       NA
#> 36        S        2     26   9.7   3         3        10       3        2
#> 37        S        2     26   9.7   3         3        10       3        2
#> 38        S        2     26   9.7   3         3        10       3        2
#> 39        S        2     26   9.7   3         3        10       3        2
#> 40        S        2     26   9.7   3         3        10       3        2
#> 41        S        2     26   9.7   3         3        10       3        2
#> 42        S        2     26   9.7   3         3        10       3        2
#> 43        S        2     26   9.7   3         3        10       3        2
#> 44        S        2     26   9.7   3         3        10       3        2
#> 45        S        2     26   9.7   3         3        10       3        2
#> 46        S        2     26   9.7   3         3        10       3        2
#> 47        S        2     26   9.7   3         3        10       3        2
#> 48        S        2     26   9.7   3         3        10       3        2
#> 49        S        2     26   9.7   3         3        10       3        2
#> 50        S        2     26   9.7   3         3        10       3        2
#> 51        S        2     26   9.7   3         3        10       3        2
#> 52        S        2     26   9.7   3         3        10       3        2
#> 53        S        2     26   9.7   3         3        10       3        2
#> 54        S        2     26   9.7   3         3        10       3        2
#> 55        S        2     26   9.7   3         3        10       3        2
#> 56        S        2     26   9.7   3         3        10       3        2
#> 57        S        2     26   9.7   3         3        10       3        2
#> 58        S        2     26   9.7   3         3        10       3        2
#> 59        S        2     NA    NA  NA        NA        NA      NA       NA
#> 60        S        2     NA    NA  NA        NA        NA      NA       NA
#> 61        S        2     NA    NA   3         3        10      NA       NA
#> 62        S        2     27   9.0   3         3        10      NA       NA
#> 63        S        2     27   9.0   3         3        10       1        2
#> 64        S        2     27   9.0   3         3        10       1        2
#> 65        S        2     27   9.0   3         3        10       1        2
#> 66        S        2     27   9.0   3         3        10       1        2
#> 67        S        2     27   9.0   3         3        10       1        2
#> 68        S        2     27   9.0   3         3        10       1        2
#> 69        S        2     27   9.0   3         3        10       1        2
#> 70        S        2     27   9.0   2         3         8       1        2
#> 71        S        2     27   9.0   2         3         8       1        2
#> 72        S        2     27   9.0   2         3         8       1        2
#> 73        S        2     27   9.0   2         3         6       1        2
#> 74        S        2     23  10.0   2         3         6       1        2
#> 75        S        2     23  10.0   2         3         6       3       NA
#> 76        S        2     23  10.0   2         3         6       3       NA
#> 77        S        2     23  10.0   2         3         6       3       NA
#> 78        S        2     23  10.0   2         3         6       3       NA
#> 79        S        2     23  10.0   2         3         6       3       NA
#> 80        S        2    352   9.3   2         3         6       3       NA
#> 81        S        2    352   9.3   2         3         6       3       NA
#> 82        S        2    352   9.3   2         3         6       3       NA
#> 83        S        2    352   9.3   2         3         6       3       NA
#> 84        S        2    352   9.3   2         3         6       3       NA
#> 85        S        2    352   9.3   2         3         6       3       NA
#> 86        S        2    352   9.3   2         3         6       3       NA
#> 87        S        2    335  10.1   2         3         6       3       NA
#> 88        S        2    335  10.1   2         3         6       3       NA
#> 89        S        2    335  10.1   2         3         6       3       NA
#> 90        S        2    335  10.1   2         3         6       3       NA
#> 91        S        2    335  10.1   2         3         6       3       NA
#> 92        S        2    335  10.1   2         3         6       3       NA
#> 93        S        2    335  10.1   2         3         6       3       NA
#> 94        S        2    335  10.1   2         3         6       3       NA
#> 95        S        2    335  10.1   2         3         6       3       NA
#> 96        S        2    335  10.1   2         3         6       3       NA
#> 97        S        2    335  10.1   2         3         6       3       NA
#> 98        S        2     NA    NA  NA        NA        NA      NA       NA
#> 99        S        2     NA    NA  NA        NA        NA      NA       NA
#> 100       S        2     NA    NA   3         3         9      NA       NA
#> 101       S        2     32   9.5   3         3         9      NA       NA
#> 102       S        2     32   9.5   3         3         9       3       NA
#> 103       S        2     32   9.5   3         3         9       3       NA
#> 104       S        2     32   9.5   3         3         9       3       NA
#> 105       S        2     32   9.5   3         3         9       3       NA
#> 106       S        2     35   9.6   3         3         9       3       NA
#> 107       S        2     35   9.6   3         3         9       1       NA
#> 108       S        2     35   9.6   3         3         9       1       NA
#> 109       S        2     35   9.6   3         3         9       1       12
#> 110       S        2     35   9.6   3         3         9       1       12
#> 111       S        2     35   9.6   3         3         9       1       12
#> 112       S        2     35   9.6   3         3         9       1       12
#> 113       S        2     35   9.6   3         3         9       1       12
#> 114       S        2     35   9.6   3         3         9       1       12
#> 115       S        2     35   9.2   3         3         9       1       12
#> 116       S        2     35   9.2   3         3         9       1       12
#> 117       S        2     35   9.2   3         3         9       1       12
#> 118       S        2     35   9.2   3         3         9       1       12
#> 119       S        2     35   9.2   3         3         9       1       12
#> 120       S        2     35   9.2   3         3         9       1       12
#> 121       S        2     35   9.2   3         3         9       1       12
#> 122       S        2     35   9.2   3         3         9       1       12
#> 123       S        2     35   9.2   3         3         9       1       12
#> 124       S        2     35   9.2   3         3         9       1       12
#> 125       S        2     35   9.2   3         3         9       1       12
#> 126       S        2     NA    NA  NA        NA        NA      NA       NA
#> 127       S        2     NA    NA  NA        NA        NA      NA       NA
#> 128       S        2     NA    NA   3         3         9      NA       NA
#> 129       S        2     22   9.5   3         3         9      NA       NA
#> 130       S        2     22   9.5   3         3         9       1        8
#> 131       S        2     22   9.5   3         3         9       1        8
#> 132       S        2     22   9.5   3         3         9       1        8
#> 133       S        2     22   9.5   3         3         6       1        8
#> 134       S        2     20   9.3   3         3         6       1        8
#> 135       S        2     20   9.3   3         3         6       1        8
#> 136       S        2     20   9.3   3         3         6       1        8
#> 137       S        2     20   9.3   3         3         6       1        8
#> 138       S        2     20   9.3   3         3         6       1        8
#> 139       S        2     20   9.3   3         3         6       1        8
#> 140       S        2     20   9.3   3         3         6       1        8
#> 141       S        2     20   9.3   3         3         6       1        8
#> 142       S        2     20   9.3   3         3         6       1        8
#> 143       S        2     20   9.3   3         3         6       1        8
#> 144       S        2     20   9.3   3         3         6       1        8
#> 145       S        2     20   9.3   3         3         6       1        8
#> 146       S        2     20   9.3   3         3         6       1        8
#> 147       S        2     20   9.3   3         3         6       1        8
#> 148       S        2     20   9.3   3         3         6       1        8
#> 149       S        2     NA    NA  NA        NA        NA      NA       NA
#> 150       S        2     NA    NA  NA        NA        NA      NA       NA
#> 151       S        2     NA    NA   3         3         6      NA       NA
#> 152       S        2     17   9.3   3         3         6      NA       NA
#> 153       S        2     17   9.3   3         3         6       1        8
#> 154       S        2     17   9.3   3         3         6       1        8
#> 155       S        2     17   9.3   3         3         6       1        8
#> 156       S        2     17   9.3   3         3         6       1        8
#> 157       S        2     17   9.3   2         3         6       1        8
#> 158       S        2     16   8.9   2         3         6       1        8
#> 159       S        2     16   8.9   2         3         6       1        9
#> 160       S        2     16   8.9   2         3         6       1        9
#> 161       S        2     16   8.9   2         3         6       1        9
#> 162       S        2     16   8.9   2         3         6       1        9
#> 163       S        2     16   8.9   2         3         6       1        9
#> 164       S        2     16   8.9   2         3         6       1        9
#> 165       S        2     16   8.9   2         3         6       1        9
#> 166       S        2     NA    NA  NA        NA        NA      NA       NA
#> 167       S        2     NA    NA  NA        NA        NA      NA       NA
#> 168       S        2     NA    NA   3         3         8      NA       NA
#> 169       S        2     25   8.9   3         3         8      NA       NA
#> 170       S        2     25   8.9   3         3         8       1        8
#> 171       S        2     25   8.9   3         3         8       1        8
#> 172       S        2     25   8.9   3         3         8       1        8
#> 173       S        2     25   8.9   2         3         6       1        8
#> 174       S        2     25   8.9   2         3         6       1        8
#> 175       S        2     25   8.9   2         3         6       1        8
#> 176       S        2     25   8.9   2         3         6       1        8
#> 177       S        2     25   8.9   2         3         6       1        8
#> 178       S        2     25   8.9   2         3         6       1        8
#> 179       S        2     25   8.9   2         3         6       1        8
#> 180       S        2     25   8.9   2         3         6       1        8
#> 181       S        2     25   8.9   2         3         6       1        8
#> 182       S        2     25   8.9   2         3         6       1        8
#> 183       S        2     25   8.9   2         3         6       1        8
#> 184       S        2     25   8.9   2         3         6       1        8
#> 185       S        2     25   8.9   2         3         6       1        8
#> 186       S        2     NA    NA  NA        NA        NA      NA       NA
#> 187       S        2     NA    NA  NA        NA        NA      NA       NA
#> 188       S        2     NA    NA   2         3         6      NA       NA
#> 189       S        2     30   9.5   2         3         6      NA       NA
#> 190       S        2     30   9.5   2         3         6       1        8
#> 191       S        2     30   9.5   2         3         6       1        8
#> 192       S        2     30   9.5   2         3         6       1        8
#> 193       S        2     30   9.5   2         3         6       1        8
#> 194       S        2     30   9.5   2         3         6       1        8
#> 195       S        2     30   9.5   2         3         6       1        8
#> 196       S        2     30   9.5   2         3         6       1        8
#> 197       S        2     30   9.5   2         3         6       1        8
#> 198       S        2     30   9.5   2         3         6       1        8
#> 199       S        2     30   9.5   2         3         6       1        8
#> 200       S        2     30   9.5   2         3         6       1        8
#> 201       S        2     30   9.5   2         3         6       1        8
#> 202       S        2     30   9.5   2         3         6       1        8
#> 203       S        2     30   9.5   2         3         6       1        8
#> 204       S        2     30   9.5   2         3         6       1        8
#> 205       S        2     30   9.5   2         3         6       1        8
#> 206       S        2     30   9.5   2         3         6       1        8
#> 207       S        2     30   9.5   2         3         6       1        8
#> 208    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 209    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 210    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 211    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 212    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 213    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 214    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 215    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 216    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 217    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 218    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 219    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 220    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 221    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 222    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 223    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 224    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 225    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 226    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 227    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 228    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 229       S        2     NA    NA  NA        NA        NA      NA       NA
#> 230       S        2     NA    NA  NA        NA        NA      NA       NA
#> 231       S        2     NA    NA   2         1         5      NA       NA
#> 232       S        2     35   9.5   2         1         5      NA       NA
#> 233       S        2     35   9.5   2         1         5       3       NA
#> 234       S        2     35   9.5   2         1         5       3       NA
#> 235       S        2     35   9.5   2         1         5       3       NA
#> 236       S        2     35   9.5   2         1         5       3       NA
#> 237       S        2     35   9.5   2         1         5       3       NA
#> 238       S        2     35   9.5   2         1         5       3       NA
#> 239       S        2     NA    NA  NA        NA        NA      NA       NA
#> 240       S        2     NA    NA  NA        NA        NA      NA       NA
#> 241       S        2     NA    NA   2         1         5      NA       NA
#> 242       S        2     23   9.6   2         1         5      NA       NA
#> 243       S        2     23   9.6   2         1         5       3       NA
#> 244       S        2     23   9.6   2         1         5       3       NA
#> 245       S        2     23   9.6   2         1         5       3       NA
#> 246       S        2     23   9.6   2         1         5       3       NA
#> 247       S        2     23   9.6   2         1         5       3       NA
#> 248       S        2     23   9.6   2         1         5       3       NA
#> 249       S        2     23   9.6   2         1         5       3       NA
#> 250       S        2     23   9.6   2         1         5       3       NA
#> 251       S        2     23   9.6   2         1         5       3       NA
#> 252       S        2     23   9.6   2         1         5       3       NA
#> 253       S        2     23   9.6   2         1         5       3       NA
#> 254       S        2     23   9.6   2         1         5       3       NA
#> 255       S        2     23   9.6   2         1         5       3       NA
#> 256       S        2     23   9.6   2         1         5       3       NA
#>     VertSun Glare Vis ObsL  Rec ObsR ObsInd Data1 Data2 Data3 Data4 Data5 Data6
#> 1        NA    NA  NA <NA> <NA> <NA>   <NA>  1000     c     5     Y  <NA>  <NA>
#> 2        NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 3        NA    NA  NA  280  001  126   <NA>   280   001   126  <NA>  <NA>  <NA>
#> 4        NA    NA  NA  280  001  126   <NA>     3    03   230  <NA>  10.0  <NA>
#> 5        NA    NA  NA  280  001  126   <NA>   023  09.8  <NA>  <NA>  <NA>  <NA>
#> 6        NA    NA 6.0  280  001  126   <NA>     1  <NA>  <NA>   250   6.0  <NA>
#> 7        NA    NA 6.0  280  001  126   <NA>     3    03   230  <NA>  10.0  <NA>
#> 8         3 FALSE 6.0  280  001  126   <NA>     1    02    03   257   6.0  <NA>
#> 9        NA    NA 6.0  280  001  126   <NA>     1  <NA>  <NA>   257   6.0  <NA>
#> 10       NA    NA 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 11       NA    NA 6.0  208  280  001   <NA>   208   280   001  <NA>  <NA>  <NA>
#> 12       NA    NA 6.0  208  280  001   <NA>     3    03   230  <NA>  10.0  <NA>
#> 13       NA    NA 6.0  208  280  001   <NA>   025  10.2  <NA>  <NA>  <NA>  <NA>
#> 14       NA    NA 6.0  208  280  001   <NA>     1  <NA>  <NA>   257   6.0  <NA>
#> 15       NA    NA 6.0  208  280  001   <NA>  1406   208     3     4   309   2.8
#> 16       NA    NA 6.0  208  280  001   <NA>  1406  <NA>     N     N   018  <NA>
#> 17       NA    NA 6.0  208  280  001   <NA>   280  <NA>  <NA>    43   100  <NA>
#> 18       NA    NA 6.0  208  280  001   <NA>   001  <NA>  <NA>    36   100  <NA>
#> 19       NA    NA 6.0  208  280  001   <NA>   208  <NA>  <NA>    48   100  <NA>
#> 20       NA    NA 6.0  208  280  001   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 21       NA    NA 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 22       NA    NA 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 23       NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 24       NA    NA  NA  208  280  001   <NA>   208   280   001  <NA>  <NA>  <NA>
#> 25       NA    NA  NA  208  280  001   <NA>     3    03   230  <NA>  10.0  <NA>
#> 26       NA    NA  NA  208  280  001   <NA>   029  09.1  <NA>  <NA>  <NA>  <NA>
#> 27       NA    NA 6.0  208  280  001   <NA>     1  <NA>  <NA>   257   6.0  <NA>
#> 28       NA    NA 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 29       NA    NA 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 30       NA    NA 6.0  125  208  280   <NA>   125   208   280  <NA>  <NA>  <NA>
#> 31       NA    NA 6.0  125  208  280   <NA>     3    03   230  <NA>  10.0  <NA>
#> 32       NA    NA 6.0  125  208  280   <NA>   026  09.7  <NA>  <NA>  <NA>  <NA>
#> 33       NA    NA 5.5  125  208  280   <NA>     3  <NA>  <NA>   257   5.5  <NA>
#> 34       NA    NA 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 35       NA    NA 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 36        2 FALSE 5.5  125  208  280   <NA>     3    02    02   257   5.5  <NA>
#> 37        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 38        2 FALSE 5.5  125  208  280   <NA>  1407   125     3     4   326   0.4
#> 39        2 FALSE 5.5  125  208  280   <NA>  1407  <NA>     Y     N   076  <NA>
#> 40        2 FALSE 5.5  125  208  280   <NA>   280     6    10     6   100  <NA>
#> 41        2 FALSE 5.5  125  208  280   <NA>   001     9    10     2   100  <NA>
#> 42        2 FALSE 5.5  125  208  280   <NA>   125     9    22     9   100  <NA>
#> 43        2 FALSE 5.5  125  208  280   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 44        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 45        2 FALSE 5.5  125  208  280   <NA>  1407   011   2.0   1.3  <NA>  <NA>
#> 46        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 47        2 FALSE 5.5  125  208  280   <NA>  1407   005   3.5   0.9  <NA>  <NA>
#> 48        2 FALSE 5.5  125  208  280   <NA>  1407   050  <NA>   0.5  <NA>  <NA>
#> 49        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 50        2 FALSE 5.5  125  208  280   <NA>  1407   071   4.5   0.7   100  <NA>
#> 51        2 FALSE 5.5  125  208  280   <NA>  1407   104   4.5   0.7   100  <NA>
#> 52        2 FALSE 5.5  125  208  280   <NA>  1407   002   2.2   1.3  <NA>  <NA>
#> 53        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 54        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 55        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 56        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 57        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 58        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 59       NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 60       NA    NA  NA  001  126  149   <NA>   001   126   149  <NA>  <NA>  <NA>
#> 61       NA    NA  NA  001  126  149   <NA>     3    03   230  <NA>  10.0  <NA>
#> 62       NA    NA  NA  001  126  149   <NA>   027  09.0  <NA>  <NA>  <NA>  <NA>
#> 63        2 FALSE 5.5  001  126  149   <NA>     1    02    02   257   5.5  <NA>
#> 64        2 FALSE 5.5  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 65        2 FALSE 5.5  001  126  149   <NA>   280    LV   120  0.03     1  <NA>
#> 66        2 FALSE 5.5  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 67        2 FALSE 5.5  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 68        2 FALSE 5.5  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 69        2 FALSE 6.0  001  126  149   <NA>     1    02    02   257   6.0  <NA>
#> 70        2 FALSE 6.0  001  126  149   <NA>     2    03   230  <NA>  08.0  <NA>
#> 71        2 FALSE 6.0  001  126  149   <NA>  Well ?      <NA>  <NA>  <NA>  <NA>
#> 72        2 FALSE 6.0  280  001  126   <NA>   280   001   126  <NA>  <NA>  <NA>
#> 73        2 FALSE 6.0  280  001  126   <NA>     2    03   230  <NA>  06.0  <NA>
#> 74        2 FALSE 6.0  280  001  126   <NA>   023  10.0  <NA>  <NA>  <NA>  <NA>
#> 75       NA    NA 5.5  280  001  126   <NA>     3  <NA>  <NA>   257   5.5  <NA>
#> 76       NA    NA 5.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 77       NA    NA 5.5  280  001  126   <NA>     2    03   230  <NA>  06.0  <NA>
#> 78       NA    NA 4.5  280  001  126   <NA>     3  <NA>  <NA>   257   4.5  <NA>
#> 79       NA    NA 4.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 80       NA    NA 4.5  280  001  126   <NA>   352  09.3  <NA>  <NA>  <NA>  <NA>
#> 81       NA    NA 4.5  280  001  126   <NA>  goin g lef t to  avoid  <NA>  <NA>
#> 82       NA    NA 4.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 83       NA    NA 3.5  280  001  126   <NA>     3  <NA>  <NA>   257   3.5  <NA>
#> 84       NA    NA 2.5  280  001  126   <NA>     3  <NA>  <NA>   257   2.5  <NA>
#> 85       NA    NA 2.5  280  001  126   <NA>   280   001   126  <NA>  <NA>  <NA>
#> 86       NA    NA 2.5  280  001  126   <NA>     2    03   230  <NA>  06.0  <NA>
#> 87       NA    NA 2.5  280  001  126   <NA>   335  10.1  <NA>  <NA>  <NA>  <NA>
#> 88       NA    NA 2.5  280  001  126   <NA>     3  <NA>  <NA>   257   2.5  <NA>
#> 89       NA    NA 2.5  280  001  126   <NA>     W  <NA>  <NA>  <NA>  <NA>  <NA>
#> 90       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 91       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 92       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 93       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 94       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 95       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 96       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 97       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 98       NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 99       NA    NA  NA  125  208  280   <NA>   125   208   280  <NA>  <NA>  <NA>
#> 100      NA    NA  NA  125  208  280   <NA>     3    03   230  <NA>  09.0  <NA>
#> 101      NA    NA  NA  125  208  280   <NA>   032  09.5  <NA>  <NA>  <NA>  <NA>
#> 102      NA    NA 5.8  125  208  280   <NA>     3  <NA>  <NA>   257   5.8  <NA>
#> 103      NA    NA 5.8  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 104      NA    NA 5.8  149  125  208   <NA>   149   125   208  <NA>  <NA>  <NA>
#> 105      NA    NA 5.8  149  125  208   <NA>     3    03   230  <NA>  09.0  <NA>
#> 106      NA    NA 5.8  149  125  208   <NA>   035  09.6  <NA>  <NA>  <NA>  <NA>
#> 107      NA    NA 6.0  149  125  208   <NA>     1  <NA>  <NA>   257   6.0  <NA>
#> 108      NA    NA 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 109      12 FALSE 6.0  149  125  208   <NA>     1    12    12   257   6.0  <NA>
#> 110      12 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 111      12 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 112      12 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 113      12 FALSE 6.0  126  149  125   <NA>   126   149   125  <NA>  <NA>  <NA>
#> 114      12 FALSE 6.0  126  149  125   <NA>     3    03   230  <NA>  09.0  <NA>
#> 115      12 FALSE 6.0  126  149  125   <NA>   035  09.2  <NA>  <NA>  <NA>  <NA>
#> 116      12 FALSE 6.0  126  149  125   <NA>     1    12    12   257   6.0  <NA>
#> 117      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 118      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 119      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 120      12 FALSE 6.0  126  149  125   <NA>     U  <NA>  <NA>  <NA>  <NA>  <NA>
#> 121      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 122      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 123      12 FALSE 6.0  126  149  125   <NA>   149    DC   270  0.03     2  <NA>
#> 124      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 125      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 126      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 127      NA    NA  NA  001  126  149   <NA>   001   126   149  <NA>  <NA>  <NA>
#> 128      NA    NA  NA  001  126  149   <NA>     3    03   230  <NA>  09.0  <NA>
#> 129      NA    NA  NA  001  126  149   <NA>   022  09.5  <NA>  <NA>  <NA>  <NA>
#> 130       1 FALSE 6.0  001  126  149   <NA>     1    08    01   257   6.0  <NA>
#> 131       1 FALSE 6.0  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 132       1 FALSE 6.0  280  001  126   <NA>   280   001   126  <NA>  <NA>  <NA>
#> 133       1 FALSE 6.0  280  001  126   <NA>     3    03   230  <NA>  06.0  <NA>
#> 134       1 FALSE 6.0  280  001  126   <NA>   020  09.3  <NA>  <NA>  <NA>  <NA>
#> 135       1 FALSE 6.0  280  001  126   <NA>     1    08    01   257   6.0  <NA>
#> 136       1 FALSE 6.0  280  001  126   <NA>   228    DC   300  0.02     1  <NA>
#> 137       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 138       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 139       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 140       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 141       1 FALSE 6.0  280  001  126   <NA>  1408   280     3     4   270  14.0
#> 142       1 FALSE 6.0  280  001  126   <NA>  1408  <NA>     N     N   037  <NA>
#> 143       1 FALSE 6.0  280  001  126   <NA>   280    11    24    11   100  <NA>
#> 144       1 FALSE 6.0  280  001  126   <NA>   001    12    23    12   100  <NA>
#> 145       1 FALSE 6.0  280  001  126   <NA>   126     9    13     9   100  <NA>
#> 146       1 FALSE 6.0  280  001  126   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 147       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 148       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 149      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 150      NA    NA  NA  208  280  001   <NA>   208   280   001  <NA>  <NA>  <NA>
#> 151      NA    NA  NA  208  280  001   <NA>     3    03   230  <NA>  06.0  <NA>
#> 152      NA    NA  NA  208  280  001   <NA>   017  09.3  <NA>  <NA>  <NA>  <NA>
#> 153       1 FALSE 6.0  208  280  001   <NA>     1    08    01   257   6.0  <NA>
#> 154       1 FALSE 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 155       1 FALSE 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 156       1 FALSE 6.0  125  208  280   <NA>   125   208   280  <NA>  <NA>  <NA>
#> 157       1 FALSE 6.0  125  208  280   <NA>     2    03   230  <NA>  06.0  <NA>
#> 158       1 FALSE 6.0  125  208  280   <NA>   016  08.9  <NA>  <NA>  <NA>  <NA>
#> 159       1 FALSE 6.0  125  208  280   <NA>     1    09    01   243   6.0  <NA>
#> 160       1 FALSE 6.0  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 161       1 FALSE 6.0  125  208  280   <NA>   231    DC   045  0.05     1  <NA>
#> 162       1 FALSE 6.0  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 163       1 FALSE 6.0  125  208  280   <NA>     X  <NA>  <NA>  <NA>  <NA>  <NA>
#> 164       1 FALSE 6.0  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 165       1 FALSE 6.0  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 166      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 167      NA    NA  NA  149  125  208   <NA>   149   125   208  <NA>  <NA>  <NA>
#> 168      NA    NA  NA  149  125  208   <NA>     3    03   230  <NA>  08.0  <NA>
#> 169      NA    NA  NA  149  125  208   <NA>   025  08.9  <NA>  <NA>  <NA>  <NA>
#> 170       2 FALSE 6.0  149  125  208   <NA>     1    08    02   243   6.0  <NA>
#> 171       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 172       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 173       2 FALSE 6.0  149  125  208   <NA>     2    03   230  <NA>  06.0  <NA>
#> 174       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 175       2 FALSE 6.0  149  125  208   <NA>  1409   149     3     4   344   0.2
#> 176       2 FALSE 6.0  149  125  208   <NA>  1409  <NA>     Y     Y   016  <NA>
#> 177       2 FALSE 6.0  149  125  208   <NA>   125    46    90    46   100  <NA>
#> 178       2 FALSE 6.0  149  125  208   <NA>   149    28    65    28   100  <NA>
#> 179       2 FALSE 6.0  149  125  208   <NA>   208    66    82    66   100  <NA>
#> 180       2 FALSE 6.0  149  125  208   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 181       2 FALSE 6.0  149  125  208   <NA>  1409   356   0.4   3.0  <NA>  <NA>
#> 182       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 183       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 184       2 FALSE 6.0  149  125  208   <NA>  off  effor t aft er th e sig hting
#> 185       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 186      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 187      NA    NA  NA  126  149  125   <NA>   126   149   125  <NA>  <NA>  <NA>
#> 188      NA    NA  NA  126  149  125   <NA>     2    03   230  <NA>  06.0  <NA>
#> 189      NA    NA  NA  126  149  125   <NA>   030  09.5  <NA>  <NA>  <NA>  <NA>
#> 190       2 FALSE 6.0  126  149  125   <NA>     1    08    02   243   6.0  <NA>
#> 191       2 FALSE 6.0  126  149  125   <NA>  1410   125     3     4   070   1.4
#> 192       2 FALSE 6.0  126  149  125   <NA>  1410  <NA>     Y     N   013   016
#> 193       2 FALSE 6.0  126  149  125   <NA>   280    37    72    37    68    32
#> 194       2 FALSE 6.0  126  149  125   <NA>   125    35    74    35    75    25
#> 195       2 FALSE 6.0  126  149  125   <NA>   149    29    52    29    65    35
#> 196       2 FALSE 6.0  126  149  125   <NA>   126    66    93    66    80    20
#> 197       2 FALSE 6.0  126  149  125   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 198       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 199       2 FALSE 6.0  126  149  125   <NA>   280    DC   042  0.23     2     F
#> 200       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 201       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 202       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 203       2 FALSE 6.0  126  149  125   <NA>   099    LV   180  0.01     1  <NA>
#> 204       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 205       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 206       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 207       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 208      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 209      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 210      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 211      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 212      NA    NA  NA <NA> <NA> <NA>   <NA>  1411   280     3     1   000  <NA>
#> 213      NA    NA  NA <NA> <NA> <NA>   <NA>  1411  <NA>     N     N   075  <NA>
#> 214      NA    NA  NA <NA> <NA> <NA>   <NA>   280  <NA>  <NA>  <NA>   100  <NA>
#> 215      NA    NA  NA <NA> <NA> <NA>   <NA>  off  effor t, fi rst s een b y CO 
#> 216      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 217      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 218      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 219      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 220      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 221      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 222      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 223      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 224      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 225      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 226      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 227      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 228      NA    NA  NA <NA> <NA> <NA>   <NA>  1000     c     5     Y  <NA>  <NA>
#> 229      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 230      NA    NA  NA  149  125  208   <NA>   149   125   208  <NA>  <NA>  <NA>
#> 231      NA    NA  NA  149  125  208   <NA>     2    01   035  <NA>  05.0  <NA>
#> 232      NA    NA  NA  149  125  208   <NA>   035  09.5  <NA>  <NA>  <NA>  <NA>
#> 233      NA    NA 4.0  149  125  208   <NA>     3  <NA>  <NA>   040   4.0  <NA>
#> 234      NA    NA 4.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 235      NA    NA 4.0  149  125  208   <NA>   149   309  1.47   1.7  <NA>  <NA>
#> 236      NA    NA 4.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 237      NA    NA 4.0  149  125  208   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 238      NA    NA 4.0  149  125  208   <NA>  cros sing  fishi ng ge ar     <NA>
#> 239      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 240      NA    NA  NA  149  125  208   <NA>   149   125   208  <NA>  <NA>  <NA>
#> 241      NA    NA  NA  149  125  208   <NA>     2    01   035  <NA>  05.0  <NA>
#> 242      NA    NA  NA  149  125  208   <NA>   023  09.6  <NA>  <NA>  <NA>  <NA>
#> 243      NA    NA 4.0  149  125  208   <NA>     3  <NA>  <NA>   040   4.0  <NA>
#> 244      NA    NA 4.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 245      NA    NA 4.0  149  125  208   <NA>  1412   149     2     4   359   0.3
#> 246      NA    NA 4.0  149  125  208   <NA>  1412  <NA>     Y     N   018   277
#> 247      NA    NA 4.0  149  125  208   <NA>   149   183   328   183    80    20
#> 248      NA    NA 4.0  149  125  208   <NA>   126   120   170   120    90    10
#> 249      NA    NA 4.0  149  125  208   <NA>  1413   208     3     4   038   0.8
#> 250      NA    NA 4.0  149  125  208   <NA>  1413  <NA>     Y     N   016   277
#> 251      NA    NA 4.0  149  125  208   <NA>  1413  <NA>  <NA>  <NA>   016   016
#> 252      NA    NA 4.0  149  125  208   <NA>   125    21    60    21    60    40
#> 253      NA    NA 4.0  149  125  208   <NA>   208    16    20    16    56    44
#> 254      NA    NA 4.0  149  125  208   <NA>   149    12    18    12    70    30
#> 255      NA    NA 4.0  149  125  208   <NA>   126    36    53    36    98     2
#> 256      NA    NA 4.0  149  125  208   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#>     Data7 Data8      Data9 Data10 Data11 Data12 EffortDot EventNum
#> 1    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        1
#> 2    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        2
#> 3    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        3
#> 4    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        4
#> 5    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        5
#> 6    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        6
#> 7    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        7
#> 8    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        8
#> 9    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        9
#> 10   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       10
#> 11   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       11
#> 12   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       12
#> 13   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       13
#> 14   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       14
#> 15   1.06   013       <NA>   <NA>   <NA>   <NA>      TRUE       15
#> 16   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       16
#> 17   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 18   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 19   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 20   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       17
#> 21   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       18
#> 22   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       19
#> 23   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       20
#> 24   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       21
#> 25   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       22
#> 26   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       23
#> 27   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       24
#> 28   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       25
#> 29   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       26
#> 30   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       27
#> 31   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       28
#> 32   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       29
#> 33   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       30
#> 34   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       31
#> 35   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       32
#> 36   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       33
#> 37   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       34
#> 38   2.97   037       <NA>   <NA>   <NA>   <NA>      TRUE       35
#> 39   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       36
#> 40   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 41   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 42   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 43   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       37
#> 44   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       38
#> 45   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       39
#> 46   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       40
#> 47   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       41
#> 48   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       42
#> 49   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       43
#> 50   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       44
#> 51   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       45
#> 52   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       46
#> 53   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       47
#> 54   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       48
#> 55   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       49
#> 56   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       50
#> 57   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       51
#> 58   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       52
#> 59   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       53
#> 60   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       54
#> 61   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       55
#> 62   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       56
#> 63   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       57
#> 64   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       58
#> 65   <NA>     a          n   <NA>   <NA>   <NA>      TRUE       59
#> 66   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       60
#> 67   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       61
#> 68   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       62
#> 69   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       63
#> 70   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       64
#> 71   <NA>  <NA>              <NA>   <NA>   <NA>      TRUE       65
#> 72   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       66
#> 73   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       67
#> 74   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       68
#> 75   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       69
#> 76   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       70
#> 77   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       71
#> 78   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       72
#> 79   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       73
#> 80   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       74
#> 81   <NA>  <NA>              <NA>   <NA>   <NA>      TRUE       75
#> 82   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       77
#> 83   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       78
#> 84   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       79
#> 85   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       80
#> 86   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       81
#> 87   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       82
#> 88   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       83
#> 89   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       84
#> 90   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       85
#> 91   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       86
#> 92   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       87
#> 93   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       88
#> 94   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       89
#> 95   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       90
#> 96   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       91
#> 97   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       92
#> 98   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       93
#> 99   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       94
#> 100  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       95
#> 101  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       96
#> 102  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       97
#> 103  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       98
#> 104  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       99
#> 105  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      100
#> 106  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      101
#> 107  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      102
#> 108  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      103
#> 109  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      104
#> 110  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      105
#> 111  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      106
#> 112  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      107
#> 113  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      108
#> 114  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      109
#> 115  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      110
#> 116  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      111
#> 117  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      112
#> 118  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      113
#> 119  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      114
#> 120  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      115
#> 121  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      116
#> 122  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      117
#> 123  <NA>     a          n   <NA>   <NA>   <NA>     FALSE      118
#> 124  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      119
#> 125  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      120
#> 126  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      121
#> 127  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      122
#> 128  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      123
#> 129  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      124
#> 130  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      125
#> 131  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      126
#> 132  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      127
#> 133  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      128
#> 134  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      129
#> 135  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      130
#> 136  <NA>     j          n   <NA>   <NA>   <NA>      TRUE      131
#> 137  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      132
#> 138  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      133
#> 139  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      134
#> 140  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      135
#> 141  0.28   015       <NA>   <NA>   <NA>   <NA>      TRUE      136
#> 142  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      137
#> 143  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 144  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 145  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 146  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      138
#> 147  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      139
#> 148  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      140
#> 149  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      141
#> 150  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      142
#> 151  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      143
#> 152  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      144
#> 153  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      145
#> 154  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      146
#> 155  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      147
#> 156  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      148
#> 157  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      149
#> 158  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      150
#> 159  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      151
#> 160  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      152
#> 161  <NA>     a       <NA>   <NA>   <NA>   <NA>      TRUE      153
#> 162  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      154
#> 163  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      155
#> 164  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      156
#> 165  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      157
#> 166  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      158
#> 167  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      159
#> 168  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      160
#> 169  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      161
#> 170  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      162
#> 171  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      163
#> 172  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      164
#> 173  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      165
#> 174  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      166
#> 175  3.68   002       <NA>   <NA>   <NA>   <NA>      TRUE      167
#> 176  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      168
#> 177  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 178  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 179  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 180  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      169
#> 181  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      170
#> 182  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      171
#> 183  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      172
#> 184  <NA>  <NA>              <NA>   <NA>   <NA>     FALSE      174
#> 185  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      175
#> 186  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      176
#> 187  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      177
#> 188  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      178
#> 189  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      179
#> 190  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      180
#> 191  1.66   036       <NA>   <NA>   <NA>   <NA>      TRUE      181
#> 192  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      182
#> 193  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 194  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 195  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 196  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 197  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      183
#> 198  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      185
#> 199  17.0     A          Y   <NA>   <NA>   <NA>     FALSE      186
#> 200  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      187
#> 201  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      188
#> 202  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      189
#> 203  <NA>     A          N   <NA>   <NA>   <NA>     FALSE      190
#> 204  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      191
#> 205  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      192
#> 206  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      193
#> 207  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      194
#> 208  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      195
#> 209  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      196
#> 210  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      197
#> 211  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      198
#> 212  0.00   018       <NA>   <NA>   <NA>   <NA>     FALSE      199
#> 213  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      200
#> 214  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 215 while  ridi ng bow      w       <NA>   <NA>     FALSE      201
#> 216  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      202
#> 217  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      203
#> 218  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      204
#> 219  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      205
#> 220  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      206
#> 221  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      207
#> 222  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      208
#> 223  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      209
#> 224  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      210
#> 225  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      211
#> 226  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      212
#> 227  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      213
#> 228  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        1
#> 229  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        2
#> 230  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        3
#> 231  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        4
#> 232  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        5
#> 233  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        6
#> 234  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        7
#> 235  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        8
#> 236  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        9
#> 237  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       10
#> 238  <NA>  <NA>              <NA>   <NA>   <NA>     FALSE       11
#> 239  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       12
#> 240  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       13
#> 241  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       14
#> 242  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       15
#> 243  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       16
#> 244  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       17
#> 245  3.28   018       <NA>   <NA>   <NA>   <NA>      TRUE       18
#> 246  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       19
#> 247  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 248  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 249  2.23   018       <NA>   <NA>   <NA>   <NA>      TRUE       20
#> 250  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       21
#> 251  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 252  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 253  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 254  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 255  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 256  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       22
#>           file_das line_num
#> 1   das_sample.das        1
#> 2   das_sample.das        2
#> 3   das_sample.das        3
#> 4   das_sample.das        4
#> 5   das_sample.das        5
#> 6   das_sample.das        6
#> 7   das_sample.das        7
#> 8   das_sample.das        8
#> 9   das_sample.das        9
#> 10  das_sample.das       10
#> 11  das_sample.das       11
#> 12  das_sample.das       12
#> 13  das_sample.das       13
#> 14  das_sample.das       14
#> 15  das_sample.das       15
#> 16  das_sample.das       16
#> 17  das_sample.das       17
#> 18  das_sample.das       18
#> 19  das_sample.das       19
#> 20  das_sample.das       20
#> 21  das_sample.das       21
#> 22  das_sample.das       22
#> 23  das_sample.das       23
#> 24  das_sample.das       24
#> 25  das_sample.das       25
#> 26  das_sample.das       26
#> 27  das_sample.das       27
#> 28  das_sample.das       28
#> 29  das_sample.das       29
#> 30  das_sample.das       30
#> 31  das_sample.das       31
#> 32  das_sample.das       32
#> 33  das_sample.das       33
#> 34  das_sample.das       34
#> 35  das_sample.das       35
#> 36  das_sample.das       36
#> 37  das_sample.das       37
#> 38  das_sample.das       38
#> 39  das_sample.das       39
#> 40  das_sample.das       40
#> 41  das_sample.das       41
#> 42  das_sample.das       42
#> 43  das_sample.das       43
#> 44  das_sample.das       44
#> 45  das_sample.das       45
#> 46  das_sample.das       46
#> 47  das_sample.das       47
#> 48  das_sample.das       48
#> 49  das_sample.das       49
#> 50  das_sample.das       50
#> 51  das_sample.das       51
#> 52  das_sample.das       52
#> 53  das_sample.das       53
#> 54  das_sample.das       54
#> 55  das_sample.das       55
#> 56  das_sample.das       56
#> 57  das_sample.das       57
#> 58  das_sample.das       58
#> 59  das_sample.das       59
#> 60  das_sample.das       60
#> 61  das_sample.das       61
#> 62  das_sample.das       62
#> 63  das_sample.das       63
#> 64  das_sample.das       64
#> 65  das_sample.das       65
#> 66  das_sample.das       66
#> 67  das_sample.das       67
#> 68  das_sample.das       68
#> 69  das_sample.das       69
#> 70  das_sample.das       70
#> 71  das_sample.das       71
#> 72  das_sample.das       72
#> 73  das_sample.das       73
#> 74  das_sample.das       74
#> 75  das_sample.das       75
#> 76  das_sample.das       76
#> 77  das_sample.das       77
#> 78  das_sample.das       78
#> 79  das_sample.das       79
#> 80  das_sample.das       80
#> 81  das_sample.das       81
#> 82  das_sample.das       83
#> 83  das_sample.das       84
#> 84  das_sample.das       85
#> 85  das_sample.das       86
#> 86  das_sample.das       87
#> 87  das_sample.das       88
#> 88  das_sample.das       89
#> 89  das_sample.das       90
#> 90  das_sample.das       91
#> 91  das_sample.das       92
#> 92  das_sample.das       93
#> 93  das_sample.das       94
#> 94  das_sample.das       95
#> 95  das_sample.das       96
#> 96  das_sample.das       97
#> 97  das_sample.das       98
#> 98  das_sample.das       99
#> 99  das_sample.das      100
#> 100 das_sample.das      101
#> 101 das_sample.das      102
#> 102 das_sample.das      103
#> 103 das_sample.das      104
#> 104 das_sample.das      105
#> 105 das_sample.das      106
#> 106 das_sample.das      107
#> 107 das_sample.das      108
#> 108 das_sample.das      109
#> 109 das_sample.das      110
#> 110 das_sample.das      111
#> 111 das_sample.das      112
#> 112 das_sample.das      113
#> 113 das_sample.das      114
#> 114 das_sample.das      115
#> 115 das_sample.das      116
#> 116 das_sample.das      117
#> 117 das_sample.das      118
#> 118 das_sample.das      119
#> 119 das_sample.das      120
#> 120 das_sample.das      121
#> 121 das_sample.das      122
#> 122 das_sample.das      123
#> 123 das_sample.das      124
#> 124 das_sample.das      125
#> 125 das_sample.das      126
#> 126 das_sample.das      127
#> 127 das_sample.das      128
#> 128 das_sample.das      129
#> 129 das_sample.das      130
#> 130 das_sample.das      131
#> 131 das_sample.das      132
#> 132 das_sample.das      133
#> 133 das_sample.das      134
#> 134 das_sample.das      135
#> 135 das_sample.das      136
#> 136 das_sample.das      137
#> 137 das_sample.das      138
#> 138 das_sample.das      139
#> 139 das_sample.das      140
#> 140 das_sample.das      141
#> 141 das_sample.das      142
#> 142 das_sample.das      143
#> 143 das_sample.das      144
#> 144 das_sample.das      145
#> 145 das_sample.das      146
#> 146 das_sample.das      147
#> 147 das_sample.das      148
#> 148 das_sample.das      149
#> 149 das_sample.das      150
#> 150 das_sample.das      151
#> 151 das_sample.das      152
#> 152 das_sample.das      153
#> 153 das_sample.das      154
#> 154 das_sample.das      155
#> 155 das_sample.das      156
#> 156 das_sample.das      157
#> 157 das_sample.das      158
#> 158 das_sample.das      159
#> 159 das_sample.das      160
#> 160 das_sample.das      161
#> 161 das_sample.das      162
#> 162 das_sample.das      163
#> 163 das_sample.das      164
#> 164 das_sample.das      165
#> 165 das_sample.das      166
#> 166 das_sample.das      167
#> 167 das_sample.das      168
#> 168 das_sample.das      169
#> 169 das_sample.das      170
#> 170 das_sample.das      171
#> 171 das_sample.das      172
#> 172 das_sample.das      173
#> 173 das_sample.das      174
#> 174 das_sample.das      175
#> 175 das_sample.das      176
#> 176 das_sample.das      177
#> 177 das_sample.das      178
#> 178 das_sample.das      179
#> 179 das_sample.das      180
#> 180 das_sample.das      181
#> 181 das_sample.das      182
#> 182 das_sample.das      183
#> 183 das_sample.das      184
#> 184 das_sample.das      186
#> 185 das_sample.das      187
#> 186 das_sample.das      188
#> 187 das_sample.das      189
#> 188 das_sample.das      190
#> 189 das_sample.das      191
#> 190 das_sample.das      192
#> 191 das_sample.das      193
#> 192 das_sample.das      194
#> 193 das_sample.das      195
#> 194 das_sample.das      196
#> 195 das_sample.das      197
#> 196 das_sample.das      198
#> 197 das_sample.das      199
#> 198 das_sample.das      201
#> 199 das_sample.das      202
#> 200 das_sample.das      203
#> 201 das_sample.das      204
#> 202 das_sample.das      205
#> 203 das_sample.das      206
#> 204 das_sample.das      207
#> 205 das_sample.das      208
#> 206 das_sample.das      209
#> 207 das_sample.das      210
#> 208 das_sample.das      211
#> 209 das_sample.das      212
#> 210 das_sample.das      213
#> 211 das_sample.das      214
#> 212 das_sample.das      215
#> 213 das_sample.das      216
#> 214 das_sample.das      217
#> 215 das_sample.das      218
#> 216 das_sample.das      219
#> 217 das_sample.das      220
#> 218 das_sample.das      221
#> 219 das_sample.das      222
#> 220 das_sample.das      223
#> 221 das_sample.das      224
#> 222 das_sample.das      225
#> 223 das_sample.das      226
#> 224 das_sample.das      227
#> 225 das_sample.das      228
#> 226 das_sample.das      229
#> 227 das_sample.das      230
#> 228 das_sample.das      231
#> 229 das_sample.das      232
#> 230 das_sample.das      233
#> 231 das_sample.das      234
#> 232 das_sample.das      235
#> 233 das_sample.das      236
#> 234 das_sample.das      237
#> 235 das_sample.das      238
#> 236 das_sample.das      239
#> 237 das_sample.das      240
#> 238 das_sample.das      241
#> 239 das_sample.das      242
#> 240 das_sample.das      243
#> 241 das_sample.das      244
#> 242 das_sample.das      245
#> 243 das_sample.das      246
#> 244 das_sample.das      247
#> 245 das_sample.das      248
#> 246 das_sample.das      249
#> 247 das_sample.das      250
#> 248 das_sample.das      251
#> 249 das_sample.das      252
#> 250 das_sample.das      253
#> 251 das_sample.das      254
#> 252 das_sample.das      255
#> 253 das_sample.das      256
#> 254 das_sample.das      257
#> 255 das_sample.das      258
#> 256 das_sample.das      259
das_process(y.read, reset.effort = FALSE)
#>     Event            DateTime      Lat       Lon OnEffort Cruise Mode OffsetGMT
#> 1       B 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 2       R 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 3       P 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 4       V 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 5       N 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 6       W 2013-01-13 06:27:39 39.32033 -137.6043     TRUE   1000    C         5
#> 7       V 2013-01-13 06:29:56 39.32583 -137.6018     TRUE   1000    C         5
#> 8       W 2013-01-13 06:30:10 39.32650 -137.6015     TRUE   1000    C         5
#> 9       W 2013-01-13 06:34:01 39.33600 -137.5970     TRUE   1000    C         5
#> 10      * 2013-01-13 06:37:25 39.34450 -137.5927     TRUE   1000    C         5
#> 11      P 2013-01-13 06:41:08 39.35400 -137.5880     TRUE   1000    C         5
#> 12      V 2013-01-13 06:41:08 39.35400 -137.5880     TRUE   1000    C         5
#> 13      N 2013-01-13 06:41:08 39.35400 -137.5880     TRUE   1000    C         5
#> 14      W 2013-01-13 06:41:08 39.35400 -137.5880     TRUE   1000    C         5
#> 15      S 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 16      A 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 17      1 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 18      2 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 19      3 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 20      E 2013-01-13 06:46:25 39.36717 -137.5817    FALSE   1000    C         5
#> 21      * 2013-01-13 06:47:25 39.36967 -137.5807    FALSE   1000    C         5
#> 22      * 2013-01-13 06:57:25 39.37467 -137.5987    FALSE   1000    C         5
#> 23      R 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 24      P 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 25      V 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 26      N 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 27      W 2013-01-13 06:58:04 39.37617 -137.5978     TRUE   1000    C         5
#> 28      * 2013-01-13 07:07:25 39.39883 -137.5868     TRUE   1000    C         5
#> 29      * 2013-01-13 07:17:25 39.42317 -137.5747     TRUE   1000    C         5
#> 30      P 2013-01-13 07:20:02 39.42950 -137.5715     TRUE   1000    C         5
#> 31      V 2013-01-13 07:20:02 39.42950 -137.5715     TRUE   1000    C         5
#> 32      N 2013-01-13 07:20:02 39.42950 -137.5715     TRUE   1000    C         5
#> 33      W 2013-01-13 07:20:02 39.42950 -137.5715     TRUE   1000    C         5
#> 34      * 2013-01-13 07:27:25 39.44733 -137.5627     TRUE   1000    C         5
#> 35      * 2013-01-13 07:37:25 39.47133 -137.5507     TRUE   1000    C         5
#> 36      W 2013-01-13 07:38:06 39.47300 -137.5500     TRUE   1000    C         5
#> 37      * 2013-01-13 07:47:25 39.49567 -137.5390     TRUE   1000    C         5
#> 38      S 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 39      A 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 40      1 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 41      2 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 42      3 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 43      E 2013-01-13 07:57:05 39.51933 -137.5277    FALSE   1000    C         5
#> 44      * 2013-01-13 07:57:25 39.52017 -137.5272    FALSE   1000    C         5
#> 45      s 2013-01-13 08:06:00 39.54217 -137.5263    FALSE   1000    C         5
#> 46      * 2013-01-13 08:07:25 39.54583 -137.5262    FALSE   1000    C         5
#> 47      s 2013-01-13 08:08:58 39.55000 -137.5255    FALSE   1000    C         5
#> 48      s 2013-01-13 08:15:32 39.56000 -137.5218    FALSE   1000    C         5
#> 49      * 2013-01-13 08:17:25 39.56233 -137.5210    FALSE   1000    C         5
#> 50      s 2013-01-13 08:17:28 39.56233 -137.5210    FALSE   1000    C         5
#> 51      s 2013-01-13 08:19:39 39.56483 -137.5197    FALSE   1000    C         5
#> 52      s 2013-01-13 08:26:45 39.56400 -137.5140    FALSE   1000    C         5
#> 53      * 2013-01-13 08:27:25 39.56333 -137.5133    FALSE   1000    C         5
#> 54      * 2013-01-13 08:37:25 39.54867 -137.5007    FALSE   1000    C         5
#> 55      * 2013-01-13 08:47:25 39.54533 -137.4807    FALSE   1000    C         5
#> 56      * 2013-01-13 08:57:25 39.54433 -137.4717    FALSE   1000    C         5
#> 57      * 2013-01-13 09:07:25 39.54333 -137.4627    FALSE   1000    C         5
#> 58      * 2013-01-13 09:17:25 39.55833 -137.4573    FALSE   1000    C         5
#> 59      R 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 60      P 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 61      V 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 62      N 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 63      W 2013-01-13 09:22:13 39.56800 -137.4530     TRUE   1000    C         5
#> 64      * 2013-01-13 09:27:25 39.57983 -137.4475     TRUE   1000    C         5
#> 65      t 2013-01-13 09:34:27 39.59733 -137.4400     TRUE   1000    C         5
#> 66      * 2013-01-13 09:37:25 39.60467 -137.4368     TRUE   1000    C         5
#> 67      * 2013-01-13 09:47:25 39.62983 -137.4262     TRUE   1000    C         5
#> 68      * 2013-01-13 09:57:25 39.65517 -137.4155     TRUE   1000    C         5
#> 69      W 2013-01-13 09:59:38 39.66083 -137.4132     TRUE   1000    C         5
#> 70      V 2013-01-13 09:59:50 39.66133 -137.4130     TRUE   1000    C         5
#> 71      C 2013-01-13 10:04:23 39.67300 -137.4083     TRUE   1000    C         5
#> 72      P 2013-01-13 10:04:35 39.67350 -137.4080     TRUE   1000    C         5
#> 73      V 2013-01-13 10:04:35 39.67350 -137.4080     TRUE   1000    C         5
#> 74      N 2013-01-13 10:04:35 39.67350 -137.4080     TRUE   1000    C         5
#> 75      W 2013-01-13 10:04:35 39.67350 -137.4080     TRUE   1000    C         5
#> 76      * 2013-01-13 10:07:25 39.68083 -137.4050     TRUE   1000    C         5
#> 77      V 2013-01-13 10:11:00 39.69000 -137.4012     TRUE   1000    C         5
#> 78      W 2013-01-13 10:11:09 39.69050 -137.4010     TRUE   1000    C         5
#> 79      * 2013-01-13 10:17:25 39.70650 -137.3943     TRUE   1000    C         5
#> 80      N 2013-01-13 10:20:38 39.71483 -137.3920     TRUE   1000    C         5
#> 81      C 2013-01-13 10:20:43 39.71483 -137.3920     TRUE   1000    C         5
#> 82      * 2013-01-13 10:27:25 39.73150 -137.3993     TRUE   1000    C         5
#> 83      W 2013-01-13 10:30:28 39.73917 -137.4032     TRUE   1000    C         5
#> 84      W 2013-01-13 10:35:14 39.75117 -137.4092     TRUE   1000    C         5
#> 85      P 2013-01-13 10:36:06 39.75350 -137.4103     TRUE   1000    C         5
#> 86      V 2013-01-13 10:36:06 39.75350 -137.4103     TRUE   1000    C         5
#> 87      N 2013-01-13 10:36:06 39.75350 -137.4103     TRUE   1000    C         5
#> 88      W 2013-01-13 10:36:06 39.75350 -137.4103     TRUE   1000    C         5
#> 89      E 2013-01-13 10:36:27 39.75433 -137.4107    FALSE   1000    C         5
#> 90      * 2013-01-13 10:37:25 39.75683 -137.4118    FALSE   1000    C         5
#> 91      * 2013-01-13 10:47:25 39.78450 -137.4093    FALSE   1000    C         5
#> 92      * 2013-01-13 10:57:25 39.81250 -137.4087    FALSE   1000    C         5
#> 93      * 2013-01-13 11:07:25 39.83983 -137.4100    FALSE   1000    C         5
#> 94      * 2013-01-13 11:17:25 39.86700 -137.4110    FALSE   1000    C         5
#> 95      * 2013-01-13 11:27:25 39.89000 -137.4000    FALSE   1000    C         5
#> 96      * 2013-01-13 11:37:25 39.91233 -137.3875    FALSE   1000    C         5
#> 97      * 2013-01-13 11:47:25 39.93500 -137.3750    FALSE   1000    C         5
#> 98      R 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 99      P 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 100     V 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 101     N 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 102     W 2013-01-13 11:51:51 39.94517 -137.3692     TRUE   1000    C         5
#> 103     * 2013-01-13 11:57:25 39.95767 -137.3613     TRUE   1000    C         5
#> 104     P 2013-01-13 12:02:29 39.96900 -137.3542     TRUE   1000    C         5
#> 105     V 2013-01-13 12:02:29 39.96900 -137.3542     TRUE   1000    C         5
#> 106     N 2013-01-13 12:02:29 39.96900 -137.3542     TRUE   1000    C         5
#> 107     W 2013-01-13 12:02:29 39.96900 -137.3542     TRUE   1000    C         5
#> 108     * 2013-01-13 12:07:25 39.97983 -137.3472     TRUE   1000    C         5
#> 109     W 2013-01-13 12:11:06 39.98767 -137.3418     TRUE   1000    C         5
#> 110     * 2013-01-13 12:17:25 40.00117 -137.3332     TRUE   1000    C         5
#> 111     * 2013-01-13 12:27:25 40.02183 -137.3197     TRUE   1000    C         5
#> 112     * 2013-01-13 12:37:25 40.04317 -137.3060     TRUE   1000    C         5
#> 113     P 2013-01-13 12:43:14 40.05567 -137.2978     TRUE   1000    C         5
#> 114     V 2013-01-13 12:43:14 40.05567 -137.2978     TRUE   1000    C         5
#> 115     N 2013-01-13 12:43:14 40.05567 -137.2978     TRUE   1000    C         5
#> 116     W 2013-01-13 12:43:14 40.05567 -137.2978     TRUE   1000    C         5
#> 117     * 2013-01-13 12:47:25 40.06450 -137.2920     TRUE   1000    C         5
#> 118     * 2013-01-13 12:57:25 40.08583 -137.2777     TRUE   1000    C         5
#> 119     * 2013-01-13 13:07:25 40.10750 -137.2627     TRUE   1000    C         5
#> 120     E 2013-01-13 13:16:38 40.12750 -137.2487    FALSE   1000    C         5
#> 121     * 2013-01-13 13:17:25 40.12917 -137.2477    FALSE   1000    C         5
#> 122     * 2013-01-13 13:27:25 40.13817 -137.2248    FALSE   1000    C         5
#> 123     t 2013-01-13 13:35:18 40.14000 -137.2048    FALSE   1000    C         5
#> 124     * 2013-01-13 13:37:25 40.14100 -137.1993    FALSE   1000    C         5
#> 125     * 2013-01-13 13:47:25 40.14717 -137.1782    FALSE   1000    C         5
#> 126     R 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 127     P 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 128     V 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 129     N 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 130     W 2013-01-13 13:50:07 40.15217 -137.1737     TRUE   1000    C         5
#> 131     * 2013-01-13 13:57:25 40.16967 -137.1670     TRUE   1000    C         5
#> 132     P 2013-01-13 14:00:31 40.17700 -137.1642     TRUE   1000    C         5
#> 133     V 2013-01-13 14:00:31 40.17700 -137.1642     TRUE   1000    C         5
#> 134     N 2013-01-13 14:00:31 40.17700 -137.1642     TRUE   1000    C         5
#> 135     W 2013-01-13 14:00:31 40.17700 -137.1642     TRUE   1000    C         5
#> 136     t 2013-01-13 14:02:55 40.18283 -137.1622     TRUE   1000    C         5
#> 137     * 2013-01-13 14:07:25 40.19333 -137.1583     TRUE   1000    C         5
#> 138     * 2013-01-13 14:17:25 40.21667 -137.1505     TRUE   1000    C         5
#> 139     * 2013-01-13 14:27:25 40.24017 -137.1432     TRUE   1000    C         5
#> 140     * 2013-01-13 14:37:25 40.26417 -137.1353     TRUE   1000    C         5
#> 141     S 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 142     A 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 143     1 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 144     2 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 145     3 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 146     E 2013-01-13 14:38:13 40.26617 -137.1348    FALSE   1000    C         5
#> 147     * 2013-01-13 14:47:25 40.26317 -137.1353    FALSE   1000    C         5
#> 148     * 2013-01-13 14:57:25 40.26383 -137.1282    FALSE   1000    C         5
#> 149     R 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 150     P 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 151     V 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 152     N 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 153     W 2013-01-13 14:59:19 40.26867 -137.1268     TRUE   1000    C         5
#> 154     * 2013-01-13 15:07:25 40.28850 -137.1205     TRUE   1000    C         5
#> 155     * 2013-01-13 15:17:25 40.31300 -137.1132     TRUE   1000    C         5
#> 156     P 2013-01-13 15:20:26 40.32033 -137.1108     TRUE   1000    C         5
#> 157     V 2013-01-13 15:20:26 40.32033 -137.1108     TRUE   1000    C         5
#> 158     N 2013-01-13 15:20:26 40.32033 -137.1108     TRUE   1000    C         5
#> 159     W 2013-01-13 15:20:26 40.32033 -137.1108     TRUE   1000    C         5
#> 160     * 2013-01-13 15:27:25 40.33700 -137.1057     TRUE   1000    C         5
#> 161     t 2013-01-13 15:36:47 40.36050 -137.0978     TRUE   1000    C         5
#> 162     * 2013-01-13 15:37:25 40.36217 -137.0973     TRUE   1000    C         5
#> 163     E 2013-01-13 15:43:08 40.37600 -137.0915    FALSE   1000    C         5
#> 164     * 2013-01-13 15:47:25 40.37933 -137.0927    FALSE   1000    C         5
#> 165     * 2013-01-13 15:57:25 40.37950 -137.0990    FALSE   1000    C         5
#> 166     R 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 167     P 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 168     V 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 169     N 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 170     W 2013-01-13 15:58:41 40.38250 -137.0977     TRUE   1000    C         5
#> 171     * 2013-01-13 16:07:25 40.40200 -137.0885     TRUE   1000    C         5
#> 172     * 2013-01-13 16:17:25 40.42383 -137.0777     TRUE   1000    C         5
#> 173     V 2013-01-13 16:20:02 40.42967 -137.0745     TRUE   1000    C         5
#> 174     * 2013-01-13 16:27:25 40.44600 -137.0658     TRUE   1000    C         5
#> 175     S 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 176     A 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 177     1 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 178     2 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 179     3 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 180     E 2013-01-13 16:29:50 40.45133 -137.0628    FALSE   1000    C         5
#> 181     s 2013-01-13 16:36:34 40.46667 -137.0570    FALSE   1000    C         5
#> 182     * 2013-01-13 16:37:25 40.46867 -137.0567    FALSE   1000    C         5
#> 183     * 2013-01-13 16:47:25 40.49250 -137.0563    FALSE   1000    C         5
#> 184     C 2013-01-13 16:55:27 40.51250 -137.0593    FALSE   1000    C         5
#> 185     * 2013-01-13 16:57:25 40.51650 -137.0567    FALSE   1000    C         5
#> 186     R 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 187     P 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 188     V 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 189     N 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 190     W 2013-01-13 16:59:54 40.52200 -137.0533     TRUE   1000    C         5
#> 191     S 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 192     A 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 193     1 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 194     2 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 195     3 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 196     4 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 197     E 2013-01-13 17:01:21 40.52533 -137.0515    FALSE   1000    C         5
#> 198     * 2013-01-13 17:07:25 40.52167 -137.0392    FALSE   1000    C         5
#> 199     t 2013-01-13 17:08:11 40.52083 -137.0383    FALSE   1000    C         5
#> 200     * 2013-01-13 17:17:25 40.51983 -137.0377    FALSE   1000    C         5
#> 201     * 2013-01-13 17:27:25 40.51750 -137.0427    FALSE   1000    C         5
#> 202     * 2013-01-13 17:37:25 40.50750 -137.0418    FALSE   1000    C         5
#> 203     t 2013-01-13 17:45:10 40.50450 -137.0437    FALSE   1000    C         5
#> 204     * 2013-01-13 17:47:32 40.50383 -137.0443    FALSE   1000    C         5
#> 205     * 2013-01-13 17:57:32 40.50033 -137.0472    FALSE   1000    C         5
#> 206     * 2013-01-13 18:07:32 40.50183 -137.0530    FALSE   1000    C         5
#> 207     * 2013-01-13 18:17:32 40.50567 -137.0432    FALSE   1000    C         5
#> 208     * 2013-01-14 08:45:06 40.70300 -135.8103    FALSE   1000 <NA>         5
#> 209     * 2013-01-14 08:55:06 40.71917 -135.7988    FALSE   1000 <NA>         5
#> 210     * 2013-01-14 09:05:06 40.73533 -135.7875    FALSE   1000 <NA>         5
#> 211     * 2013-01-14 09:15:06 40.74917 -135.7770    FALSE   1000 <NA>         5
#> 212     S 2013-01-14 09:17:21 40.75183 -135.7748    FALSE   1000 <NA>         5
#> 213     A 2013-01-14 09:17:21 40.75183 -135.7748    FALSE   1000 <NA>         5
#> 214     1 2013-01-14 09:17:21 40.75183 -135.7748    FALSE   1000 <NA>         5
#> 215     C 2013-01-14 09:18:04 40.75283 -135.7742    FALSE   1000 <NA>         5
#> 216     * 2013-01-14 09:25:06 40.76233 -135.7655    FALSE   1000 <NA>         5
#> 217     * 2013-01-14 09:35:06 40.76700 -135.7470    FALSE   1000 <NA>         5
#> 218     * 2013-01-14 09:45:06 40.77450 -135.7313    FALSE   1000 <NA>         5
#> 219     * 2013-01-14 09:55:06 40.78867 -135.7203    FALSE   1000 <NA>         5
#> 220     * 2013-01-14 10:05:06 40.80750 -135.7092    FALSE   1000 <NA>         5
#> 221     * 2013-01-14 10:15:06 40.82933 -135.6968    FALSE   1000 <NA>         5
#> 222     * 2013-01-14 10:25:06 40.85200 -135.6843    FALSE   1000 <NA>         5
#> 223     * 2013-01-14 10:35:06 40.87550 -135.6703    FALSE   1000 <NA>         5
#> 224     * 2013-01-14 10:45:06 40.89883 -135.6565    FALSE   1000 <NA>         5
#> 225     * 2013-01-14 10:55:06 40.92183 -135.6417    FALSE   1000 <NA>         5
#> 226     * 2013-01-14 11:05:06 40.94400 -135.6273    FALSE   1000 <NA>         5
#> 227     * 2013-01-14 11:15:06 40.96633 -135.6123    FALSE   1000 <NA>         5
#> 228     B 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 229     R 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 230     P 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 231     V 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 232     N 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 233     W 2013-01-14 11:24:32 40.98717 -135.5980     TRUE   1000    C         5
#> 234     * 2013-01-14 11:25:06 40.98833 -135.5972     TRUE   1000    C         5
#> 235     F 2013-01-14 11:25:32 40.98950 -135.5965     TRUE   1000    C         5
#> 236     * 2013-01-14 11:35:06 41.01067 -135.5817     TRUE   1000    C         5
#> 237     E 2013-01-14 11:37:24 41.01583 -135.5782    FALSE   1000    C         5
#> 238     C 2013-01-14 11:37:27 41.01600 -135.5780    FALSE   1000    C         5
#> 239     R 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 240     P 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 241     V 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 242     N 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 243     W 2013-01-14 11:40:38 41.02383 -135.5743     TRUE   1000    C         5
#> 244     * 2013-01-14 11:45:06 41.03400 -135.5678     TRUE   1000    C         5
#> 245     S 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 246     A 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 247     1 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 248     2 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 249     S 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 250     A 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 251     ? 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 252     1 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 253     2 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 254     3 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 255     4 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 256     E 2013-01-14 11:50:29 41.04600 -135.5595    FALSE   1000    C         5
#>     EffType ESWsides Course SpdKt Bft SwellHght WindSpdKt RainFog HorizSun
#> 1      <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 2         S        2     NA    NA  NA        NA        NA      NA       NA
#> 3         S        2     NA    NA  NA        NA        NA      NA       NA
#> 4         S        2     NA    NA   3         3        10      NA       NA
#> 5         S        2     23   9.8   3         3        10      NA       NA
#> 6         S        2     23   9.8   3         3        10       1       NA
#> 7         S        2     23   9.8   3         3        10       1       NA
#> 8         S        2     23   9.8   3         3        10       1        2
#> 9         S        2     23   9.8   3         3        10       1       NA
#> 10        S        2     23   9.8   3         3        10       1       NA
#> 11        S        2     23   9.8   3         3        10       1       NA
#> 12        S        2     23   9.8   3         3        10       1       NA
#> 13        S        2     25  10.2   3         3        10       1       NA
#> 14        S        2     25  10.2   3         3        10       1       NA
#> 15        S        2     25  10.2   3         3        10       1       NA
#> 16        S        2     25  10.2   3         3        10       1       NA
#> 17        S        2     25  10.2   3         3        10       1       NA
#> 18        S        2     25  10.2   3         3        10       1       NA
#> 19        S        2     25  10.2   3         3        10       1       NA
#> 20        S        2     25  10.2   3         3        10       1       NA
#> 21        S        2     25  10.2   3         3        10       1       NA
#> 22        S        2     25  10.2   3         3        10       1       NA
#> 23        S        2     25  10.2   3         3        10       1       NA
#> 24        S        2     25  10.2   3         3        10       1       NA
#> 25        S        2     25  10.2   3         3        10       1       NA
#> 26        S        2     29   9.1   3         3        10       1       NA
#> 27        S        2     29   9.1   3         3        10       1       NA
#> 28        S        2     29   9.1   3         3        10       1       NA
#> 29        S        2     29   9.1   3         3        10       1       NA
#> 30        S        2     29   9.1   3         3        10       1       NA
#> 31        S        2     29   9.1   3         3        10       1       NA
#> 32        S        2     26   9.7   3         3        10       1       NA
#> 33        S        2     26   9.7   3         3        10       3       NA
#> 34        S        2     26   9.7   3         3        10       3       NA
#> 35        S        2     26   9.7   3         3        10       3       NA
#> 36        S        2     26   9.7   3         3        10       3        2
#> 37        S        2     26   9.7   3         3        10       3        2
#> 38        S        2     26   9.7   3         3        10       3        2
#> 39        S        2     26   9.7   3         3        10       3        2
#> 40        S        2     26   9.7   3         3        10       3        2
#> 41        S        2     26   9.7   3         3        10       3        2
#> 42        S        2     26   9.7   3         3        10       3        2
#> 43        S        2     26   9.7   3         3        10       3        2
#> 44        S        2     26   9.7   3         3        10       3        2
#> 45        S        2     26   9.7   3         3        10       3        2
#> 46        S        2     26   9.7   3         3        10       3        2
#> 47        S        2     26   9.7   3         3        10       3        2
#> 48        S        2     26   9.7   3         3        10       3        2
#> 49        S        2     26   9.7   3         3        10       3        2
#> 50        S        2     26   9.7   3         3        10       3        2
#> 51        S        2     26   9.7   3         3        10       3        2
#> 52        S        2     26   9.7   3         3        10       3        2
#> 53        S        2     26   9.7   3         3        10       3        2
#> 54        S        2     26   9.7   3         3        10       3        2
#> 55        S        2     26   9.7   3         3        10       3        2
#> 56        S        2     26   9.7   3         3        10       3        2
#> 57        S        2     26   9.7   3         3        10       3        2
#> 58        S        2     26   9.7   3         3        10       3        2
#> 59        S        2     26   9.7   3         3        10       3        2
#> 60        S        2     26   9.7   3         3        10       3        2
#> 61        S        2     26   9.7   3         3        10       3        2
#> 62        S        2     27   9.0   3         3        10       3        2
#> 63        S        2     27   9.0   3         3        10       1        2
#> 64        S        2     27   9.0   3         3        10       1        2
#> 65        S        2     27   9.0   3         3        10       1        2
#> 66        S        2     27   9.0   3         3        10       1        2
#> 67        S        2     27   9.0   3         3        10       1        2
#> 68        S        2     27   9.0   3         3        10       1        2
#> 69        S        2     27   9.0   3         3        10       1        2
#> 70        S        2     27   9.0   2         3         8       1        2
#> 71        S        2     27   9.0   2         3         8       1        2
#> 72        S        2     27   9.0   2         3         8       1        2
#> 73        S        2     27   9.0   2         3         6       1        2
#> 74        S        2     23  10.0   2         3         6       1        2
#> 75        S        2     23  10.0   2         3         6       3       NA
#> 76        S        2     23  10.0   2         3         6       3       NA
#> 77        S        2     23  10.0   2         3         6       3       NA
#> 78        S        2     23  10.0   2         3         6       3       NA
#> 79        S        2     23  10.0   2         3         6       3       NA
#> 80        S        2    352   9.3   2         3         6       3       NA
#> 81        S        2    352   9.3   2         3         6       3       NA
#> 82        S        2    352   9.3   2         3         6       3       NA
#> 83        S        2    352   9.3   2         3         6       3       NA
#> 84        S        2    352   9.3   2         3         6       3       NA
#> 85        S        2    352   9.3   2         3         6       3       NA
#> 86        S        2    352   9.3   2         3         6       3       NA
#> 87        S        2    335  10.1   2         3         6       3       NA
#> 88        S        2    335  10.1   2         3         6       3       NA
#> 89        S        2    335  10.1   2         3         6       3       NA
#> 90        S        2    335  10.1   2         3         6       3       NA
#> 91        S        2    335  10.1   2         3         6       3       NA
#> 92        S        2    335  10.1   2         3         6       3       NA
#> 93        S        2    335  10.1   2         3         6       3       NA
#> 94        S        2    335  10.1   2         3         6       3       NA
#> 95        S        2    335  10.1   2         3         6       3       NA
#> 96        S        2    335  10.1   2         3         6       3       NA
#> 97        S        2    335  10.1   2         3         6       3       NA
#> 98        S        2    335  10.1   2         3         6       3       NA
#> 99        S        2    335  10.1   2         3         6       3       NA
#> 100       S        2    335  10.1   3         3         9       3       NA
#> 101       S        2     32   9.5   3         3         9       3       NA
#> 102       S        2     32   9.5   3         3         9       3       NA
#> 103       S        2     32   9.5   3         3         9       3       NA
#> 104       S        2     32   9.5   3         3         9       3       NA
#> 105       S        2     32   9.5   3         3         9       3       NA
#> 106       S        2     35   9.6   3         3         9       3       NA
#> 107       S        2     35   9.6   3         3         9       1       NA
#> 108       S        2     35   9.6   3         3         9       1       NA
#> 109       S        2     35   9.6   3         3         9       1       12
#> 110       S        2     35   9.6   3         3         9       1       12
#> 111       S        2     35   9.6   3         3         9       1       12
#> 112       S        2     35   9.6   3         3         9       1       12
#> 113       S        2     35   9.6   3         3         9       1       12
#> 114       S        2     35   9.6   3         3         9       1       12
#> 115       S        2     35   9.2   3         3         9       1       12
#> 116       S        2     35   9.2   3         3         9       1       12
#> 117       S        2     35   9.2   3         3         9       1       12
#> 118       S        2     35   9.2   3         3         9       1       12
#> 119       S        2     35   9.2   3         3         9       1       12
#> 120       S        2     35   9.2   3         3         9       1       12
#> 121       S        2     35   9.2   3         3         9       1       12
#> 122       S        2     35   9.2   3         3         9       1       12
#> 123       S        2     35   9.2   3         3         9       1       12
#> 124       S        2     35   9.2   3         3         9       1       12
#> 125       S        2     35   9.2   3         3         9       1       12
#> 126       S        2     35   9.2   3         3         9       1       12
#> 127       S        2     35   9.2   3         3         9       1       12
#> 128       S        2     35   9.2   3         3         9       1       12
#> 129       S        2     22   9.5   3         3         9       1       12
#> 130       S        2     22   9.5   3         3         9       1        8
#> 131       S        2     22   9.5   3         3         9       1        8
#> 132       S        2     22   9.5   3         3         9       1        8
#> 133       S        2     22   9.5   3         3         6       1        8
#> 134       S        2     20   9.3   3         3         6       1        8
#> 135       S        2     20   9.3   3         3         6       1        8
#> 136       S        2     20   9.3   3         3         6       1        8
#> 137       S        2     20   9.3   3         3         6       1        8
#> 138       S        2     20   9.3   3         3         6       1        8
#> 139       S        2     20   9.3   3         3         6       1        8
#> 140       S        2     20   9.3   3         3         6       1        8
#> 141       S        2     20   9.3   3         3         6       1        8
#> 142       S        2     20   9.3   3         3         6       1        8
#> 143       S        2     20   9.3   3         3         6       1        8
#> 144       S        2     20   9.3   3         3         6       1        8
#> 145       S        2     20   9.3   3         3         6       1        8
#> 146       S        2     20   9.3   3         3         6       1        8
#> 147       S        2     20   9.3   3         3         6       1        8
#> 148       S        2     20   9.3   3         3         6       1        8
#> 149       S        2     20   9.3   3         3         6       1        8
#> 150       S        2     20   9.3   3         3         6       1        8
#> 151       S        2     20   9.3   3         3         6       1        8
#> 152       S        2     17   9.3   3         3         6       1        8
#> 153       S        2     17   9.3   3         3         6       1        8
#> 154       S        2     17   9.3   3         3         6       1        8
#> 155       S        2     17   9.3   3         3         6       1        8
#> 156       S        2     17   9.3   3         3         6       1        8
#> 157       S        2     17   9.3   2         3         6       1        8
#> 158       S        2     16   8.9   2         3         6       1        8
#> 159       S        2     16   8.9   2         3         6       1        9
#> 160       S        2     16   8.9   2         3         6       1        9
#> 161       S        2     16   8.9   2         3         6       1        9
#> 162       S        2     16   8.9   2         3         6       1        9
#> 163       S        2     16   8.9   2         3         6       1        9
#> 164       S        2     16   8.9   2         3         6       1        9
#> 165       S        2     16   8.9   2         3         6       1        9
#> 166       S        2     16   8.9   2         3         6       1        9
#> 167       S        2     16   8.9   2         3         6       1        9
#> 168       S        2     16   8.9   3         3         8       1        9
#> 169       S        2     25   8.9   3         3         8       1        9
#> 170       S        2     25   8.9   3         3         8       1        8
#> 171       S        2     25   8.9   3         3         8       1        8
#> 172       S        2     25   8.9   3         3         8       1        8
#> 173       S        2     25   8.9   2         3         6       1        8
#> 174       S        2     25   8.9   2         3         6       1        8
#> 175       S        2     25   8.9   2         3         6       1        8
#> 176       S        2     25   8.9   2         3         6       1        8
#> 177       S        2     25   8.9   2         3         6       1        8
#> 178       S        2     25   8.9   2         3         6       1        8
#> 179       S        2     25   8.9   2         3         6       1        8
#> 180       S        2     25   8.9   2         3         6       1        8
#> 181       S        2     25   8.9   2         3         6       1        8
#> 182       S        2     25   8.9   2         3         6       1        8
#> 183       S        2     25   8.9   2         3         6       1        8
#> 184       S        2     25   8.9   2         3         6       1        8
#> 185       S        2     25   8.9   2         3         6       1        8
#> 186       S        2     25   8.9   2         3         6       1        8
#> 187       S        2     25   8.9   2         3         6       1        8
#> 188       S        2     25   8.9   2         3         6       1        8
#> 189       S        2     30   9.5   2         3         6       1        8
#> 190       S        2     30   9.5   2         3         6       1        8
#> 191       S        2     30   9.5   2         3         6       1        8
#> 192       S        2     30   9.5   2         3         6       1        8
#> 193       S        2     30   9.5   2         3         6       1        8
#> 194       S        2     30   9.5   2         3         6       1        8
#> 195       S        2     30   9.5   2         3         6       1        8
#> 196       S        2     30   9.5   2         3         6       1        8
#> 197       S        2     30   9.5   2         3         6       1        8
#> 198       S        2     30   9.5   2         3         6       1        8
#> 199       S        2     30   9.5   2         3         6       1        8
#> 200       S        2     30   9.5   2         3         6       1        8
#> 201       S        2     30   9.5   2         3         6       1        8
#> 202       S        2     30   9.5   2         3         6       1        8
#> 203       S        2     30   9.5   2         3         6       1        8
#> 204       S        2     30   9.5   2         3         6       1        8
#> 205       S        2     30   9.5   2         3         6       1        8
#> 206       S        2     30   9.5   2         3         6       1        8
#> 207       S        2     30   9.5   2         3         6       1        8
#> 208    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 209    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 210    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 211    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 212    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 213    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 214    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 215    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 216    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 217    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 218    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 219    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 220    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 221    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 222    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 223    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 224    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 225    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 226    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 227    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 228    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 229       S        2     NA    NA  NA        NA        NA      NA       NA
#> 230       S        2     NA    NA  NA        NA        NA      NA       NA
#> 231       S        2     NA    NA   2         1         5      NA       NA
#> 232       S        2     35   9.5   2         1         5      NA       NA
#> 233       S        2     35   9.5   2         1         5       3       NA
#> 234       S        2     35   9.5   2         1         5       3       NA
#> 235       S        2     35   9.5   2         1         5       3       NA
#> 236       S        2     35   9.5   2         1         5       3       NA
#> 237       S        2     35   9.5   2         1         5       3       NA
#> 238       S        2     35   9.5   2         1         5       3       NA
#> 239       S        2     35   9.5   2         1         5       3       NA
#> 240       S        2     35   9.5   2         1         5       3       NA
#> 241       S        2     35   9.5   2         1         5       3       NA
#> 242       S        2     23   9.6   2         1         5       3       NA
#> 243       S        2     23   9.6   2         1         5       3       NA
#> 244       S        2     23   9.6   2         1         5       3       NA
#> 245       S        2     23   9.6   2         1         5       3       NA
#> 246       S        2     23   9.6   2         1         5       3       NA
#> 247       S        2     23   9.6   2         1         5       3       NA
#> 248       S        2     23   9.6   2         1         5       3       NA
#> 249       S        2     23   9.6   2         1         5       3       NA
#> 250       S        2     23   9.6   2         1         5       3       NA
#> 251       S        2     23   9.6   2         1         5       3       NA
#> 252       S        2     23   9.6   2         1         5       3       NA
#> 253       S        2     23   9.6   2         1         5       3       NA
#> 254       S        2     23   9.6   2         1         5       3       NA
#> 255       S        2     23   9.6   2         1         5       3       NA
#> 256       S        2     23   9.6   2         1         5       3       NA
#>     VertSun Glare Vis ObsL  Rec ObsR ObsInd Data1 Data2 Data3 Data4 Data5 Data6
#> 1        NA    NA  NA <NA> <NA> <NA>   <NA>  1000     c     5     Y  <NA>  <NA>
#> 2        NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 3        NA    NA  NA  280  001  126   <NA>   280   001   126  <NA>  <NA>  <NA>
#> 4        NA    NA  NA  280  001  126   <NA>     3    03   230  <NA>  10.0  <NA>
#> 5        NA    NA  NA  280  001  126   <NA>   023  09.8  <NA>  <NA>  <NA>  <NA>
#> 6        NA    NA 6.0  280  001  126   <NA>     1  <NA>  <NA>   250   6.0  <NA>
#> 7        NA    NA 6.0  280  001  126   <NA>     3    03   230  <NA>  10.0  <NA>
#> 8         3 FALSE 6.0  280  001  126   <NA>     1    02    03   257   6.0  <NA>
#> 9        NA    NA 6.0  280  001  126   <NA>     1  <NA>  <NA>   257   6.0  <NA>
#> 10       NA    NA 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 11       NA    NA 6.0  208  280  001   <NA>   208   280   001  <NA>  <NA>  <NA>
#> 12       NA    NA 6.0  208  280  001   <NA>     3    03   230  <NA>  10.0  <NA>
#> 13       NA    NA 6.0  208  280  001   <NA>   025  10.2  <NA>  <NA>  <NA>  <NA>
#> 14       NA    NA 6.0  208  280  001   <NA>     1  <NA>  <NA>   257   6.0  <NA>
#> 15       NA    NA 6.0  208  280  001   <NA>  1406   208     3     4   309   2.8
#> 16       NA    NA 6.0  208  280  001   <NA>  1406  <NA>     N     N   018  <NA>
#> 17       NA    NA 6.0  208  280  001   <NA>   280  <NA>  <NA>    43   100  <NA>
#> 18       NA    NA 6.0  208  280  001   <NA>   001  <NA>  <NA>    36   100  <NA>
#> 19       NA    NA 6.0  208  280  001   <NA>   208  <NA>  <NA>    48   100  <NA>
#> 20       NA    NA 6.0  208  280  001   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 21       NA    NA 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 22       NA    NA 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 23       NA    NA 6.0  208  280  001   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 24       NA    NA 6.0  208  280  001   <NA>   208   280   001  <NA>  <NA>  <NA>
#> 25       NA    NA 6.0  208  280  001   <NA>     3    03   230  <NA>  10.0  <NA>
#> 26       NA    NA 6.0  208  280  001   <NA>   029  09.1  <NA>  <NA>  <NA>  <NA>
#> 27       NA    NA 6.0  208  280  001   <NA>     1  <NA>  <NA>   257   6.0  <NA>
#> 28       NA    NA 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 29       NA    NA 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 30       NA    NA 6.0  125  208  280   <NA>   125   208   280  <NA>  <NA>  <NA>
#> 31       NA    NA 6.0  125  208  280   <NA>     3    03   230  <NA>  10.0  <NA>
#> 32       NA    NA 6.0  125  208  280   <NA>   026  09.7  <NA>  <NA>  <NA>  <NA>
#> 33       NA    NA 5.5  125  208  280   <NA>     3  <NA>  <NA>   257   5.5  <NA>
#> 34       NA    NA 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 35       NA    NA 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 36        2 FALSE 5.5  125  208  280   <NA>     3    02    02   257   5.5  <NA>
#> 37        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 38        2 FALSE 5.5  125  208  280   <NA>  1407   125     3     4   326   0.4
#> 39        2 FALSE 5.5  125  208  280   <NA>  1407  <NA>     Y     N   076  <NA>
#> 40        2 FALSE 5.5  125  208  280   <NA>   280     6    10     6   100  <NA>
#> 41        2 FALSE 5.5  125  208  280   <NA>   001     9    10     2   100  <NA>
#> 42        2 FALSE 5.5  125  208  280   <NA>   125     9    22     9   100  <NA>
#> 43        2 FALSE 5.5  125  208  280   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 44        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 45        2 FALSE 5.5  125  208  280   <NA>  1407   011   2.0   1.3  <NA>  <NA>
#> 46        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 47        2 FALSE 5.5  125  208  280   <NA>  1407   005   3.5   0.9  <NA>  <NA>
#> 48        2 FALSE 5.5  125  208  280   <NA>  1407   050  <NA>   0.5  <NA>  <NA>
#> 49        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 50        2 FALSE 5.5  125  208  280   <NA>  1407   071   4.5   0.7   100  <NA>
#> 51        2 FALSE 5.5  125  208  280   <NA>  1407   104   4.5   0.7   100  <NA>
#> 52        2 FALSE 5.5  125  208  280   <NA>  1407   002   2.2   1.3  <NA>  <NA>
#> 53        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 54        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 55        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 56        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 57        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 58        2 FALSE 5.5  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 59        2 FALSE 5.5  125  208  280   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 60        2 FALSE 5.5  001  126  149   <NA>   001   126   149  <NA>  <NA>  <NA>
#> 61        2 FALSE 5.5  001  126  149   <NA>     3    03   230  <NA>  10.0  <NA>
#> 62        2 FALSE 5.5  001  126  149   <NA>   027  09.0  <NA>  <NA>  <NA>  <NA>
#> 63        2 FALSE 5.5  001  126  149   <NA>     1    02    02   257   5.5  <NA>
#> 64        2 FALSE 5.5  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 65        2 FALSE 5.5  001  126  149   <NA>   280    LV   120  0.03     1  <NA>
#> 66        2 FALSE 5.5  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 67        2 FALSE 5.5  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 68        2 FALSE 5.5  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 69        2 FALSE 6.0  001  126  149   <NA>     1    02    02   257   6.0  <NA>
#> 70        2 FALSE 6.0  001  126  149   <NA>     2    03   230  <NA>  08.0  <NA>
#> 71        2 FALSE 6.0  001  126  149   <NA>  Well ?      <NA>  <NA>  <NA>  <NA>
#> 72        2 FALSE 6.0  280  001  126   <NA>   280   001   126  <NA>  <NA>  <NA>
#> 73        2 FALSE 6.0  280  001  126   <NA>     2    03   230  <NA>  06.0  <NA>
#> 74        2 FALSE 6.0  280  001  126   <NA>   023  10.0  <NA>  <NA>  <NA>  <NA>
#> 75       NA    NA 5.5  280  001  126   <NA>     3  <NA>  <NA>   257   5.5  <NA>
#> 76       NA    NA 5.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 77       NA    NA 5.5  280  001  126   <NA>     2    03   230  <NA>  06.0  <NA>
#> 78       NA    NA 4.5  280  001  126   <NA>     3  <NA>  <NA>   257   4.5  <NA>
#> 79       NA    NA 4.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 80       NA    NA 4.5  280  001  126   <NA>   352  09.3  <NA>  <NA>  <NA>  <NA>
#> 81       NA    NA 4.5  280  001  126   <NA>  goin g lef t to  avoid  <NA>  <NA>
#> 82       NA    NA 4.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 83       NA    NA 3.5  280  001  126   <NA>     3  <NA>  <NA>   257   3.5  <NA>
#> 84       NA    NA 2.5  280  001  126   <NA>     3  <NA>  <NA>   257   2.5  <NA>
#> 85       NA    NA 2.5  280  001  126   <NA>   280   001   126  <NA>  <NA>  <NA>
#> 86       NA    NA 2.5  280  001  126   <NA>     2    03   230  <NA>  06.0  <NA>
#> 87       NA    NA 2.5  280  001  126   <NA>   335  10.1  <NA>  <NA>  <NA>  <NA>
#> 88       NA    NA 2.5  280  001  126   <NA>     3  <NA>  <NA>   257   2.5  <NA>
#> 89       NA    NA 2.5  280  001  126   <NA>     W  <NA>  <NA>  <NA>  <NA>  <NA>
#> 90       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 91       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 92       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 93       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 94       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 95       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 96       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 97       NA    NA 2.5  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 98       NA    NA 2.5  280  001  126   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 99       NA    NA 2.5  125  208  280   <NA>   125   208   280  <NA>  <NA>  <NA>
#> 100      NA    NA 2.5  125  208  280   <NA>     3    03   230  <NA>  09.0  <NA>
#> 101      NA    NA 2.5  125  208  280   <NA>   032  09.5  <NA>  <NA>  <NA>  <NA>
#> 102      NA    NA 5.8  125  208  280   <NA>     3  <NA>  <NA>   257   5.8  <NA>
#> 103      NA    NA 5.8  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 104      NA    NA 5.8  149  125  208   <NA>   149   125   208  <NA>  <NA>  <NA>
#> 105      NA    NA 5.8  149  125  208   <NA>     3    03   230  <NA>  09.0  <NA>
#> 106      NA    NA 5.8  149  125  208   <NA>   035  09.6  <NA>  <NA>  <NA>  <NA>
#> 107      NA    NA 6.0  149  125  208   <NA>     1  <NA>  <NA>   257   6.0  <NA>
#> 108      NA    NA 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 109      12 FALSE 6.0  149  125  208   <NA>     1    12    12   257   6.0  <NA>
#> 110      12 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 111      12 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 112      12 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 113      12 FALSE 6.0  126  149  125   <NA>   126   149   125  <NA>  <NA>  <NA>
#> 114      12 FALSE 6.0  126  149  125   <NA>     3    03   230  <NA>  09.0  <NA>
#> 115      12 FALSE 6.0  126  149  125   <NA>   035  09.2  <NA>  <NA>  <NA>  <NA>
#> 116      12 FALSE 6.0  126  149  125   <NA>     1    12    12   257   6.0  <NA>
#> 117      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 118      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 119      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 120      12 FALSE 6.0  126  149  125   <NA>     U  <NA>  <NA>  <NA>  <NA>  <NA>
#> 121      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 122      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 123      12 FALSE 6.0  126  149  125   <NA>   149    DC   270  0.03     2  <NA>
#> 124      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 125      12 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 126      12 FALSE 6.0  126  149  125   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 127      12 FALSE 6.0  001  126  149   <NA>   001   126   149  <NA>  <NA>  <NA>
#> 128      12 FALSE 6.0  001  126  149   <NA>     3    03   230  <NA>  09.0  <NA>
#> 129      12 FALSE 6.0  001  126  149   <NA>   022  09.5  <NA>  <NA>  <NA>  <NA>
#> 130       1 FALSE 6.0  001  126  149   <NA>     1    08    01   257   6.0  <NA>
#> 131       1 FALSE 6.0  001  126  149   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 132       1 FALSE 6.0  280  001  126   <NA>   280   001   126  <NA>  <NA>  <NA>
#> 133       1 FALSE 6.0  280  001  126   <NA>     3    03   230  <NA>  06.0  <NA>
#> 134       1 FALSE 6.0  280  001  126   <NA>   020  09.3  <NA>  <NA>  <NA>  <NA>
#> 135       1 FALSE 6.0  280  001  126   <NA>     1    08    01   257   6.0  <NA>
#> 136       1 FALSE 6.0  280  001  126   <NA>   228    DC   300  0.02     1  <NA>
#> 137       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 138       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 139       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 140       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 141       1 FALSE 6.0  280  001  126   <NA>  1408   280     3     4   270  14.0
#> 142       1 FALSE 6.0  280  001  126   <NA>  1408  <NA>     N     N   037  <NA>
#> 143       1 FALSE 6.0  280  001  126   <NA>   280    11    24    11   100  <NA>
#> 144       1 FALSE 6.0  280  001  126   <NA>   001    12    23    12   100  <NA>
#> 145       1 FALSE 6.0  280  001  126   <NA>   126     9    13     9   100  <NA>
#> 146       1 FALSE 6.0  280  001  126   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 147       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 148       1 FALSE 6.0  280  001  126   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 149       1 FALSE 6.0  280  001  126   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 150       1 FALSE 6.0  208  280  001   <NA>   208   280   001  <NA>  <NA>  <NA>
#> 151       1 FALSE 6.0  208  280  001   <NA>     3    03   230  <NA>  06.0  <NA>
#> 152       1 FALSE 6.0  208  280  001   <NA>   017  09.3  <NA>  <NA>  <NA>  <NA>
#> 153       1 FALSE 6.0  208  280  001   <NA>     1    08    01   257   6.0  <NA>
#> 154       1 FALSE 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 155       1 FALSE 6.0  208  280  001   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 156       1 FALSE 6.0  125  208  280   <NA>   125   208   280  <NA>  <NA>  <NA>
#> 157       1 FALSE 6.0  125  208  280   <NA>     2    03   230  <NA>  06.0  <NA>
#> 158       1 FALSE 6.0  125  208  280   <NA>   016  08.9  <NA>  <NA>  <NA>  <NA>
#> 159       1 FALSE 6.0  125  208  280   <NA>     1    09    01   243   6.0  <NA>
#> 160       1 FALSE 6.0  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 161       1 FALSE 6.0  125  208  280   <NA>   231    DC   045  0.05     1  <NA>
#> 162       1 FALSE 6.0  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 163       1 FALSE 6.0  125  208  280   <NA>     X  <NA>  <NA>  <NA>  <NA>  <NA>
#> 164       1 FALSE 6.0  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 165       1 FALSE 6.0  125  208  280   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 166       1 FALSE 6.0  125  208  280   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 167       1 FALSE 6.0  149  125  208   <NA>   149   125   208  <NA>  <NA>  <NA>
#> 168       1 FALSE 6.0  149  125  208   <NA>     3    03   230  <NA>  08.0  <NA>
#> 169       1 FALSE 6.0  149  125  208   <NA>   025  08.9  <NA>  <NA>  <NA>  <NA>
#> 170       2 FALSE 6.0  149  125  208   <NA>     1    08    02   243   6.0  <NA>
#> 171       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 172       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 173       2 FALSE 6.0  149  125  208   <NA>     2    03   230  <NA>  06.0  <NA>
#> 174       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 175       2 FALSE 6.0  149  125  208   <NA>  1409   149     3     4   344   0.2
#> 176       2 FALSE 6.0  149  125  208   <NA>  1409  <NA>     Y     Y   016  <NA>
#> 177       2 FALSE 6.0  149  125  208   <NA>   125    46    90    46   100  <NA>
#> 178       2 FALSE 6.0  149  125  208   <NA>   149    28    65    28   100  <NA>
#> 179       2 FALSE 6.0  149  125  208   <NA>   208    66    82    66   100  <NA>
#> 180       2 FALSE 6.0  149  125  208   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 181       2 FALSE 6.0  149  125  208   <NA>  1409   356   0.4   3.0  <NA>  <NA>
#> 182       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 183       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 184       2 FALSE 6.0  149  125  208   <NA>  off  effor t aft er th e sig hting
#> 185       2 FALSE 6.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 186       2 FALSE 6.0  149  125  208   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 187       2 FALSE 6.0  126  149  125   <NA>   126   149   125  <NA>  <NA>  <NA>
#> 188       2 FALSE 6.0  126  149  125   <NA>     2    03   230  <NA>  06.0  <NA>
#> 189       2 FALSE 6.0  126  149  125   <NA>   030  09.5  <NA>  <NA>  <NA>  <NA>
#> 190       2 FALSE 6.0  126  149  125   <NA>     1    08    02   243   6.0  <NA>
#> 191       2 FALSE 6.0  126  149  125   <NA>  1410   125     3     4   070   1.4
#> 192       2 FALSE 6.0  126  149  125   <NA>  1410  <NA>     Y     N   013   016
#> 193       2 FALSE 6.0  126  149  125   <NA>   280    37    72    37    68    32
#> 194       2 FALSE 6.0  126  149  125   <NA>   125    35    74    35    75    25
#> 195       2 FALSE 6.0  126  149  125   <NA>   149    29    52    29    65    35
#> 196       2 FALSE 6.0  126  149  125   <NA>   126    66    93    66    80    20
#> 197       2 FALSE 6.0  126  149  125   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#> 198       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 199       2 FALSE 6.0  126  149  125   <NA>   280    DC   042  0.23     2     F
#> 200       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 201       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 202       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 203       2 FALSE 6.0  126  149  125   <NA>   099    LV   180  0.01     1  <NA>
#> 204       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 205       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 206       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 207       2 FALSE 6.0  126  149  125   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 208      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 209      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 210      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 211      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 212      NA    NA  NA <NA> <NA> <NA>   <NA>  1411   280     3     1   000  <NA>
#> 213      NA    NA  NA <NA> <NA> <NA>   <NA>  1411  <NA>     N     N   075  <NA>
#> 214      NA    NA  NA <NA> <NA> <NA>   <NA>   280  <NA>  <NA>  <NA>   100  <NA>
#> 215      NA    NA  NA <NA> <NA> <NA>   <NA>  off  effor t, fi rst s een b y CO 
#> 216      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 217      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 218      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 219      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 220      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 221      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 222      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 223      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 224      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 225      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 226      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 227      NA    NA  NA <NA> <NA> <NA>   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 228      NA    NA  NA <NA> <NA> <NA>   <NA>  1000     c     5     Y  <NA>  <NA>
#> 229      NA    NA  NA <NA> <NA> <NA>   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 230      NA    NA  NA  149  125  208   <NA>   149   125   208  <NA>  <NA>  <NA>
#> 231      NA    NA  NA  149  125  208   <NA>     2    01   035  <NA>  05.0  <NA>
#> 232      NA    NA  NA  149  125  208   <NA>   035  09.5  <NA>  <NA>  <NA>  <NA>
#> 233      NA    NA 4.0  149  125  208   <NA>     3  <NA>  <NA>   040   4.0  <NA>
#> 234      NA    NA 4.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 235      NA    NA 4.0  149  125  208   <NA>   149   309  1.47   1.7  <NA>  <NA>
#> 236      NA    NA 4.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 237      NA    NA 4.0  149  125  208   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 238      NA    NA 4.0  149  125  208   <NA>  cros sing  fishi ng ge ar     <NA>
#> 239      NA    NA 4.0  149  125  208   <NA>     S  <NA>  <NA>  <NA>  <NA>  <NA>
#> 240      NA    NA 4.0  149  125  208   <NA>   149   125   208  <NA>  <NA>  <NA>
#> 241      NA    NA 4.0  149  125  208   <NA>     2    01   035  <NA>  05.0  <NA>
#> 242      NA    NA 4.0  149  125  208   <NA>   023  09.6  <NA>  <NA>  <NA>  <NA>
#> 243      NA    NA 4.0  149  125  208   <NA>     3  <NA>  <NA>   040   4.0  <NA>
#> 244      NA    NA 4.0  149  125  208   <NA>  <NA>  <NA>  <NA>  <NA>  <NA>  <NA>
#> 245      NA    NA 4.0  149  125  208   <NA>  1412   149     2     4   359   0.3
#> 246      NA    NA 4.0  149  125  208   <NA>  1412  <NA>     Y     N   018   277
#> 247      NA    NA 4.0  149  125  208   <NA>   149   183   328   183    80    20
#> 248      NA    NA 4.0  149  125  208   <NA>   126   120   170   120    90    10
#> 249      NA    NA 4.0  149  125  208   <NA>  1413   208     3     4   038   0.8
#> 250      NA    NA 4.0  149  125  208   <NA>  1413  <NA>     Y     N   016   277
#> 251      NA    NA 4.0  149  125  208   <NA>  1413  <NA>  <NA>  <NA>   016   016
#> 252      NA    NA 4.0  149  125  208   <NA>   125    21    60    21    60    40
#> 253      NA    NA 4.0  149  125  208   <NA>   208    16    20    16    56    44
#> 254      NA    NA 4.0  149  125  208   <NA>   149    12    18    12    70    30
#> 255      NA    NA 4.0  149  125  208   <NA>   126    36    53    36    98     2
#> 256      NA    NA 4.0  149  125  208   <NA>     C  <NA>  <NA>  <NA>  <NA>  <NA>
#>     Data7 Data8      Data9 Data10 Data11 Data12 EffortDot EventNum
#> 1    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        1
#> 2    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        2
#> 3    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        3
#> 4    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        4
#> 5    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        5
#> 6    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        6
#> 7    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        7
#> 8    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        8
#> 9    <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        9
#> 10   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       10
#> 11   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       11
#> 12   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       12
#> 13   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       13
#> 14   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       14
#> 15   1.06   013       <NA>   <NA>   <NA>   <NA>      TRUE       15
#> 16   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       16
#> 17   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 18   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 19   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 20   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       17
#> 21   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       18
#> 22   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       19
#> 23   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       20
#> 24   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       21
#> 25   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       22
#> 26   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       23
#> 27   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       24
#> 28   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       25
#> 29   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       26
#> 30   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       27
#> 31   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       28
#> 32   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       29
#> 33   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       30
#> 34   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       31
#> 35   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       32
#> 36   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       33
#> 37   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       34
#> 38   2.97   037       <NA>   <NA>   <NA>   <NA>      TRUE       35
#> 39   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       36
#> 40   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 41   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 42   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 43   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       37
#> 44   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       38
#> 45   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       39
#> 46   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       40
#> 47   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       41
#> 48   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       42
#> 49   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       43
#> 50   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       44
#> 51   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       45
#> 52   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       46
#> 53   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       47
#> 54   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       48
#> 55   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       49
#> 56   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       50
#> 57   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       51
#> 58   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       52
#> 59   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       53
#> 60   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       54
#> 61   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       55
#> 62   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       56
#> 63   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       57
#> 64   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       58
#> 65   <NA>     a          n   <NA>   <NA>   <NA>      TRUE       59
#> 66   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       60
#> 67   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       61
#> 68   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       62
#> 69   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       63
#> 70   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       64
#> 71   <NA>  <NA>              <NA>   <NA>   <NA>      TRUE       65
#> 72   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       66
#> 73   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       67
#> 74   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       68
#> 75   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       69
#> 76   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       70
#> 77   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       71
#> 78   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       72
#> 79   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       73
#> 80   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       74
#> 81   <NA>  <NA>              <NA>   <NA>   <NA>      TRUE       75
#> 82   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       77
#> 83   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       78
#> 84   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       79
#> 85   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       80
#> 86   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       81
#> 87   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       82
#> 88   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       83
#> 89   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       84
#> 90   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       85
#> 91   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       86
#> 92   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       87
#> 93   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       88
#> 94   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       89
#> 95   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       90
#> 96   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       91
#> 97   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       92
#> 98   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       93
#> 99   <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       94
#> 100  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       95
#> 101  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       96
#> 102  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       97
#> 103  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       98
#> 104  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       99
#> 105  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      100
#> 106  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      101
#> 107  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      102
#> 108  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      103
#> 109  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      104
#> 110  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      105
#> 111  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      106
#> 112  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      107
#> 113  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      108
#> 114  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      109
#> 115  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      110
#> 116  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      111
#> 117  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      112
#> 118  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      113
#> 119  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      114
#> 120  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      115
#> 121  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      116
#> 122  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      117
#> 123  <NA>     a          n   <NA>   <NA>   <NA>     FALSE      118
#> 124  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      119
#> 125  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      120
#> 126  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      121
#> 127  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      122
#> 128  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      123
#> 129  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      124
#> 130  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      125
#> 131  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      126
#> 132  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      127
#> 133  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      128
#> 134  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      129
#> 135  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      130
#> 136  <NA>     j          n   <NA>   <NA>   <NA>      TRUE      131
#> 137  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      132
#> 138  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      133
#> 139  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      134
#> 140  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      135
#> 141  0.28   015       <NA>   <NA>   <NA>   <NA>      TRUE      136
#> 142  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      137
#> 143  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 144  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 145  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 146  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      138
#> 147  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      139
#> 148  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      140
#> 149  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      141
#> 150  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      142
#> 151  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      143
#> 152  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      144
#> 153  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      145
#> 154  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      146
#> 155  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      147
#> 156  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      148
#> 157  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      149
#> 158  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      150
#> 159  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      151
#> 160  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      152
#> 161  <NA>     a       <NA>   <NA>   <NA>   <NA>      TRUE      153
#> 162  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      154
#> 163  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      155
#> 164  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      156
#> 165  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      157
#> 166  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      158
#> 167  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      159
#> 168  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      160
#> 169  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      161
#> 170  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      162
#> 171  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      163
#> 172  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      164
#> 173  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      165
#> 174  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      166
#> 175  3.68   002       <NA>   <NA>   <NA>   <NA>      TRUE      167
#> 176  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      168
#> 177  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 178  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 179  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 180  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      169
#> 181  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      170
#> 182  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      171
#> 183  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      172
#> 184  <NA>  <NA>              <NA>   <NA>   <NA>     FALSE      174
#> 185  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      175
#> 186  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      176
#> 187  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      177
#> 188  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      178
#> 189  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      179
#> 190  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      180
#> 191  1.66   036       <NA>   <NA>   <NA>   <NA>      TRUE      181
#> 192  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE      182
#> 193  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 194  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 195  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 196  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 197  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      183
#> 198  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      185
#> 199  17.0     A          Y   <NA>   <NA>   <NA>     FALSE      186
#> 200  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      187
#> 201  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      188
#> 202  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      189
#> 203  <NA>     A          N   <NA>   <NA>   <NA>     FALSE      190
#> 204  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      191
#> 205  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      192
#> 206  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      193
#> 207  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      194
#> 208  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      195
#> 209  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      196
#> 210  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      197
#> 211  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      198
#> 212  0.00   018       <NA>   <NA>   <NA>   <NA>     FALSE      199
#> 213  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      200
#> 214  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 215 while  ridi ng bow      w       <NA>   <NA>     FALSE      201
#> 216  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      202
#> 217  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      203
#> 218  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      204
#> 219  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      205
#> 220  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      206
#> 221  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      207
#> 222  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      208
#> 223  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      209
#> 224  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      210
#> 225  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      211
#> 226  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      212
#> 227  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE      213
#> 228  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        1
#> 229  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        2
#> 230  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        3
#> 231  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        4
#> 232  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        5
#> 233  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        6
#> 234  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        7
#> 235  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        8
#> 236  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE        9
#> 237  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       10
#> 238  <NA>  <NA>              <NA>   <NA>   <NA>     FALSE       11
#> 239  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       12
#> 240  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       13
#> 241  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       14
#> 242  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       15
#> 243  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       16
#> 244  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       17
#> 245  3.28   018       <NA>   <NA>   <NA>   <NA>      TRUE       18
#> 246  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       19
#> 247  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 248  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 249  2.23   018       <NA>   <NA>   <NA>   <NA>      TRUE       20
#> 250  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>      TRUE       21
#> 251  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 252  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 253  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 254  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 255  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE     <NA>
#> 256  <NA>  <NA>       <NA>   <NA>   <NA>   <NA>     FALSE       22
#>           file_das line_num
#> 1   das_sample.das        1
#> 2   das_sample.das        2
#> 3   das_sample.das        3
#> 4   das_sample.das        4
#> 5   das_sample.das        5
#> 6   das_sample.das        6
#> 7   das_sample.das        7
#> 8   das_sample.das        8
#> 9   das_sample.das        9
#> 10  das_sample.das       10
#> 11  das_sample.das       11
#> 12  das_sample.das       12
#> 13  das_sample.das       13
#> 14  das_sample.das       14
#> 15  das_sample.das       15
#> 16  das_sample.das       16
#> 17  das_sample.das       17
#> 18  das_sample.das       18
#> 19  das_sample.das       19
#> 20  das_sample.das       20
#> 21  das_sample.das       21
#> 22  das_sample.das       22
#> 23  das_sample.das       23
#> 24  das_sample.das       24
#> 25  das_sample.das       25
#> 26  das_sample.das       26
#> 27  das_sample.das       27
#> 28  das_sample.das       28
#> 29  das_sample.das       29
#> 30  das_sample.das       30
#> 31  das_sample.das       31
#> 32  das_sample.das       32
#> 33  das_sample.das       33
#> 34  das_sample.das       34
#> 35  das_sample.das       35
#> 36  das_sample.das       36
#> 37  das_sample.das       37
#> 38  das_sample.das       38
#> 39  das_sample.das       39
#> 40  das_sample.das       40
#> 41  das_sample.das       41
#> 42  das_sample.das       42
#> 43  das_sample.das       43
#> 44  das_sample.das       44
#> 45  das_sample.das       45
#> 46  das_sample.das       46
#> 47  das_sample.das       47
#> 48  das_sample.das       48
#> 49  das_sample.das       49
#> 50  das_sample.das       50
#> 51  das_sample.das       51
#> 52  das_sample.das       52
#> 53  das_sample.das       53
#> 54  das_sample.das       54
#> 55  das_sample.das       55
#> 56  das_sample.das       56
#> 57  das_sample.das       57
#> 58  das_sample.das       58
#> 59  das_sample.das       59
#> 60  das_sample.das       60
#> 61  das_sample.das       61
#> 62  das_sample.das       62
#> 63  das_sample.das       63
#> 64  das_sample.das       64
#> 65  das_sample.das       65
#> 66  das_sample.das       66
#> 67  das_sample.das       67
#> 68  das_sample.das       68
#> 69  das_sample.das       69
#> 70  das_sample.das       70
#> 71  das_sample.das       71
#> 72  das_sample.das       72
#> 73  das_sample.das       73
#> 74  das_sample.das       74
#> 75  das_sample.das       75
#> 76  das_sample.das       76
#> 77  das_sample.das       77
#> 78  das_sample.das       78
#> 79  das_sample.das       79
#> 80  das_sample.das       80
#> 81  das_sample.das       81
#> 82  das_sample.das       83
#> 83  das_sample.das       84
#> 84  das_sample.das       85
#> 85  das_sample.das       86
#> 86  das_sample.das       87
#> 87  das_sample.das       88
#> 88  das_sample.das       89
#> 89  das_sample.das       90
#> 90  das_sample.das       91
#> 91  das_sample.das       92
#> 92  das_sample.das       93
#> 93  das_sample.das       94
#> 94  das_sample.das       95
#> 95  das_sample.das       96
#> 96  das_sample.das       97
#> 97  das_sample.das       98
#> 98  das_sample.das       99
#> 99  das_sample.das      100
#> 100 das_sample.das      101
#> 101 das_sample.das      102
#> 102 das_sample.das      103
#> 103 das_sample.das      104
#> 104 das_sample.das      105
#> 105 das_sample.das      106
#> 106 das_sample.das      107
#> 107 das_sample.das      108
#> 108 das_sample.das      109
#> 109 das_sample.das      110
#> 110 das_sample.das      111
#> 111 das_sample.das      112
#> 112 das_sample.das      113
#> 113 das_sample.das      114
#> 114 das_sample.das      115
#> 115 das_sample.das      116
#> 116 das_sample.das      117
#> 117 das_sample.das      118
#> 118 das_sample.das      119
#> 119 das_sample.das      120
#> 120 das_sample.das      121
#> 121 das_sample.das      122
#> 122 das_sample.das      123
#> 123 das_sample.das      124
#> 124 das_sample.das      125
#> 125 das_sample.das      126
#> 126 das_sample.das      127
#> 127 das_sample.das      128
#> 128 das_sample.das      129
#> 129 das_sample.das      130
#> 130 das_sample.das      131
#> 131 das_sample.das      132
#> 132 das_sample.das      133
#> 133 das_sample.das      134
#> 134 das_sample.das      135
#> 135 das_sample.das      136
#> 136 das_sample.das      137
#> 137 das_sample.das      138
#> 138 das_sample.das      139
#> 139 das_sample.das      140
#> 140 das_sample.das      141
#> 141 das_sample.das      142
#> 142 das_sample.das      143
#> 143 das_sample.das      144
#> 144 das_sample.das      145
#> 145 das_sample.das      146
#> 146 das_sample.das      147
#> 147 das_sample.das      148
#> 148 das_sample.das      149
#> 149 das_sample.das      150
#> 150 das_sample.das      151
#> 151 das_sample.das      152
#> 152 das_sample.das      153
#> 153 das_sample.das      154
#> 154 das_sample.das      155
#> 155 das_sample.das      156
#> 156 das_sample.das      157
#> 157 das_sample.das      158
#> 158 das_sample.das      159
#> 159 das_sample.das      160
#> 160 das_sample.das      161
#> 161 das_sample.das      162
#> 162 das_sample.das      163
#> 163 das_sample.das      164
#> 164 das_sample.das      165
#> 165 das_sample.das      166
#> 166 das_sample.das      167
#> 167 das_sample.das      168
#> 168 das_sample.das      169
#> 169 das_sample.das      170
#> 170 das_sample.das      171
#> 171 das_sample.das      172
#> 172 das_sample.das      173
#> 173 das_sample.das      174
#> 174 das_sample.das      175
#> 175 das_sample.das      176
#> 176 das_sample.das      177
#> 177 das_sample.das      178
#> 178 das_sample.das      179
#> 179 das_sample.das      180
#> 180 das_sample.das      181
#> 181 das_sample.das      182
#> 182 das_sample.das      183
#> 183 das_sample.das      184
#> 184 das_sample.das      186
#> 185 das_sample.das      187
#> 186 das_sample.das      188
#> 187 das_sample.das      189
#> 188 das_sample.das      190
#> 189 das_sample.das      191
#> 190 das_sample.das      192
#> 191 das_sample.das      193
#> 192 das_sample.das      194
#> 193 das_sample.das      195
#> 194 das_sample.das      196
#> 195 das_sample.das      197
#> 196 das_sample.das      198
#> 197 das_sample.das      199
#> 198 das_sample.das      201
#> 199 das_sample.das      202
#> 200 das_sample.das      203
#> 201 das_sample.das      204
#> 202 das_sample.das      205
#> 203 das_sample.das      206
#> 204 das_sample.das      207
#> 205 das_sample.das      208
#> 206 das_sample.das      209
#> 207 das_sample.das      210
#> 208 das_sample.das      211
#> 209 das_sample.das      212
#> 210 das_sample.das      213
#> 211 das_sample.das      214
#> 212 das_sample.das      215
#> 213 das_sample.das      216
#> 214 das_sample.das      217
#> 215 das_sample.das      218
#> 216 das_sample.das      219
#> 217 das_sample.das      220
#> 218 das_sample.das      221
#> 219 das_sample.das      222
#> 220 das_sample.das      223
#> 221 das_sample.das      224
#> 222 das_sample.das      225
#> 223 das_sample.das      226
#> 224 das_sample.das      227
#> 225 das_sample.das      228
#> 226 das_sample.das      229
#> 227 das_sample.das      230
#> 228 das_sample.das      231
#> 229 das_sample.das      232
#> 230 das_sample.das      233
#> 231 das_sample.das      234
#> 232 das_sample.das      235
#> 233 das_sample.das      236
#> 234 das_sample.das      237
#> 235 das_sample.das      238
#> 236 das_sample.das      239
#> 237 das_sample.das      240
#> 238 das_sample.das      241
#> 239 das_sample.das      242
#> 240 das_sample.das      243
#> 241 das_sample.das      244
#> 242 das_sample.das      245
#> 243 das_sample.das      246
#> 244 das_sample.das      247
#> 245 das_sample.das      248
#> 246 das_sample.das      249
#> 247 das_sample.das      250
#> 248 das_sample.das      251
#> 249 das_sample.das      252
#> 250 das_sample.das      253
#> 251 das_sample.das      254
#> 252 das_sample.das      255
#> 253 das_sample.das      256
#> 254 das_sample.das      257
#> 255 das_sample.das      258
#> 256 das_sample.das      259
```
