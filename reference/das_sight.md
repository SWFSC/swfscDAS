# DAS sightings

Extract sightings and associated information from processed DAS data

## Usage

``` r
das_sight(x, ...)

# S3 method for class 'data.frame'
das_sight(x, ...)

# S3 method for class 'das_df'
das_sight(
  x,
  return.format = c("default", "wide", "complete"),
  return.events = c("S", "K", "M", "G", "s", "k", "m", "g", "t", "p", "F"),
  gs.sp.min1 = FALSE,
  ...
)
```

## Arguments

- x:

  an object of class `das_df`, or a data frame that can be coerced to
  class `das_df`

- ...:

  ignored

- return.format:

  character; can be one of "default", "wide", "complete", or any partial
  match thereof (case sensitive). Formats described below

- return.events:

  character; event codes included in the output. Must be one or more of:
  "S", "K", "M", "G", "s", "k", "m", "g", "t", "p", "F"
  (case-sensitive). The default is all of these event codes

- gs.sp.min1:

  logical; flag indicating whether species group size columns will be
  set to a minimum value of 1 (TRUE) or maintain the raw values (FALSE,
  default). See the 'Details' section below

## Value

Data frame with 1) the columns from `x`, excluding the 'Data#' columns,
and 2) columns with sighting information extracted from 'Data#' columns.
See
[`das_format_pdf`](https://swfsc.github.io/swfscDAS/reference/das_format_pdf.md)
for more information the sighting information. If `return.format` is
"default", then there is one row for each species of each sighting
event; if `return.format` is "wide", then there is one row for each
sighting event; if `return.format` is "complete", then there is one row
for every group size estimate for each sighting event (excluding sperm
whale "C" events - see the Details section).

The format-specific columns are described in their respective sections.
The following sighting information columns are included in all return
formats:

|  |  |  |
|----|----|----|
| *Sighting information* | *Column name* | *Notes* |
| Sighting number | SightNo | Character |
| Subgroup code | Subgroup | Character |
| Daily sighting number | SightNoDaily | See below |
| Observer that made the sighting | Obs |  |
| Standard observer | ObsStd | Logical; `TRUE` if Obs is one of ObsL, Rec or ObsR, and `FALSE` otherwise |
| Bearing to the sighting | Bearing | Numeric; degrees, expected range 0 to 360 |
| Number of reticle marks | Reticle | Numeric |
| Distance (nautical miles) | DistNm | Numeric |
| Sighting cue | Cue |  |
| Sighting method | Method |  |
| Photos of school? | Photos |  |
| Birds present with school? | Birds |  |
| Calibration school? | CalibSchool |  |
| Aerial photos taken? | PhotosAerial |  |
| Biopsy taken? | Biopsy |  |
| Probable sighting | Prob | Logical indicating if sighting has associated ? event; `NA` for non-S/K/M/G events |
| Number of species in sighting | nSp | `NA` for non-S/K/M/G events |
| Mixed species sighting | Mixed | Logical; `TRUE` if nSp \> 1 |
| Group size of school - best estimate | GsSchoolBest | See below |
| Group size of school - high estimate | GsSchoolHigh | See below |
| Group size of school - low estimate | GsSchoolLow | See below |
| Course (true heading) of school at resight | CourseSchool | `NA` for non-s/k/m events |
| Presence of associated JFR | TurtleJFR | `NA` for non-"t" events; JFR = jellyfish, floating debris, or red tide |
| Estimated turtle maturity | TurtleAge | `NA` for non-"t" events |
| Perpendicular distance (km) to sighting | PerpDistKm | Calculated via `(abs(sin(Bearing*pi/180) * DistNm) * 1.852)` |

SightNoDaily is a running count of the number of S/K/M/G sightings that
occurred on each day. It is formatted as 'YYYYMMDD'\_'running count',
e.g. "20050101_1".

The GsSchoolBest, GsSchoolHigh, and GsSchoolLow columns are either: 1)
the arithmetic mean across observer estimates, for the "default" and
"wide" formats, or 2) the individual observer estimates, for the
"complete" format. Note that for non-"complete" formats, `na.rm = TRUE`
is used when calculating the mean, and thus blank elements of estimates
(but not the whole incomplete estimate) are ignored.

To convert the perpendicular distance back to nautical miles, one would
divide PerpDistKm by 1.852

## Details

DAS events contain specific information in the 'Data#' columns, with the
information depending on the event code for that row. The output data
frame contains columns with this specific information extracted to
dedicated columns as described below. This function recognizes the
following types of sightings: marine mammal sightings (event codes "S",
"K", or "M"), marine mammal resights (codes "s", "k", "m"), marine
mammal subgroup sightings (code "G"), marine mammal subgroup resights
(code "g"), turtle sightings (code "t"), pinniped sightings (code "p"),
and fishing vessel sightings (code "F"). Warnings are printed if all S,
K, M, and G events (and only these events) are not followed by an A
event and at least one numeric event. See
[`das_format_pdf`](https://swfsc.github.io/swfscDAS/reference/das_format_pdf.md)
for more information about events and event formats. Of specific note -
sperm whale sightings (species code 046) often contain additional
estimates recorded as "C" events immediately following the S, A, and
numeric events. Because these estimates are recorded as"C" events, they
are NOT included in the `das_sight` calculations or output for any
`return.format`

The `return.events` argument simply provides a shortcut for filtering
the output of `das_sight` by event codes

Abbreviations used in output column names: Gs = group size, Sp =
species, Nm = nautical mile, Perc = percentage, Prob = probable,
GsSchool = school-level group size info, GsSp = species-level group size
info.

Species group sizes are calculated as the product of the arithmetic mean
of the corresponding school group size and the species percentage. These
values are reported in 'GsSp...' columns for 'default' and 'wide' output
formats. Sometimes, however, these species group size estimates may be
less than 1, depending on how the means work out across different
observers. If `gs.sp.min1` is `TRUE`, any values in 'GsSp...' columns
that are less than 1 are set to 1, via e.g.
[`pmin`](https://rdrr.io/r/base/Extremes.html)`(GsSpBest, 1)`

This function makes the following assumptions, and alterations to the
raw DAS data:

- "A" events immediately following an S/K/M/G event have the same
  sighting number (Data1 value) as the S/K/M/G event

- The 'nSp' column is equivalent to the number of non-`NA` values across
  the 'Data5', 'Data6', 'Data7', and 'Data8' columns for the pertinent
  "A" event

- The following data are coerced to a numeric using
  [`as.numeric`](https://rdrr.io/r/base/numeric.html): Bearing, Reticle,
  DistNm, Cue, Method, species percentages, and group sizes (including
  for t, p, and F events). Note that if there are any formatting errors
  and these data are not numeric, the function will likely print a
  warning message

- The values for the following columns are capitalized using
  [`toupper`](https://rdrr.io/r/base/chartr.html): 'Birds', 'Photos',
  'CalibSchool', 'PhotosAerial', 'Biopsy', 'TurtleAge', and 'TurtleCapt'

## The "default" format output

This output data frame contains 'long' sighting data, meaning there is
one row for each species of each sighting event. The GsSp... columns are
calculated as follows: for each species and for each observer estimate,
the best/high/low school size estimate is multiplied by the applicable
species percent estimate. The values are grouped by species and then
averaged to get single GsSpBest, GsSpHigh, and GsSpLow values for each
species. (using [`mean`](https://rdrr.io/r/base/mean.html) with
`na.rm = TRUE`)

Sighting information columns/formats present specifically in the
"default" format output:

|  |  |  |
|----|----|----|
| *Sighting information* | *Column name* | *Notes* |
| Species code | SpCode | Boat type or mammal, turtle, or pinniped species codes |
| Probable species code | SpCodeProb | Probable mammal species codes; `NA` if none or not applicable |
| Group size of species - best estimate | GsSpBest | The product of the arithmetic means of GsSchoolBest and the corresponding species percentage |
| Group size of species - high estimate | GsSpHigh | The product of the arithmetic means of GsSchoolHigh and the corresponding species percentage |
| Group size of species - low estimate | GsSpLow | The product of the arithmetic means of GsSchoolLow and the corresponding species percentage |

Note that for the above calculations, the GsSchoolX value and
corresponding species percentages were each averaged across observers,
using `na.rm = TRUE`, before being multiplied to calculate GsSpX. For
example, in the workflow:
`GsSpBest1 = mean(.data$Data2, na.rm = TRUE) * mean(.data$Data5, na.rm = TRUE)`

## The "wide" and "complete" format outputs

The "wide" and "complete" options have very similar columns in their
output date frames. There are two main differences: 1) the "wide" format
has one row for each sighting event, while the complete format has a row
for every observer estimate for each sightings, and thus 2) in the
"wide" format, all numeric information for which there are multiple
observer estimates (school group size, species percentage, etc.) are
averaged across estimated via an arithmetic mean (using
[`mean`](https://rdrr.io/r/base/mean.html) with `na.rm = TRUE`)

With these formats, note that the species/type code and group size for
turtle, pinniped, and boat sightings are in their own column

Sighting information columns present in the "wide" and "complete" format
outputs:

|  |  |  |
|----|----|----|
| *Sighting information* | *Column name* | *Notes* |
| Observer code - estimate | ObsEstimate | See below |
| Species 1 code | SpCode1 |  |
| Species 2 code | SpCode2 |  |
| Species 3 code | SpCode3 |  |
| Species 4 code | SpCode4 |  |
| Species 1 probable code | SpCodeProb1 | Extracted from '?' event |
| Species 2 probable code | SpCodeProb2 | Extracted from '?' event |
| Species 3 probable code | SpCodeProb3 | Extracted from '?' event |
| Species 4 probable code | SpCodeProb4 | Extracted from '?' event |
| Percentage of Sp 1 in school | SpPerc1 |  |
| Percentage of Sp 2 in school | SpPerc2 |  |
| Percentage of Sp 3 in school | SpPerc3 |  |
| Percentage of Sp 4 in school | SpPerc4 |  |
| Group size of species 1 | GsSpBest1 | Present in "wide" output only; see below |
| Group size of species 2 | GsSpBest2 | Present in "wide" output only; see below |
| Group size of species 3 | GsSpBest3 | Present in "wide" output only; see below |
| Group size of species 4 | GsSpBest4 | Present in "wide" output only; see below |
| Turtle species | TurtleSp | `NA` for non-"t" events |
| Turtle group size | TurtleGs | `NA` for non-"t" events |
| Was turtle captured? | TurtleCapt | `NA` for non-"t" events |
| Pinniped species | PinnipedSp | `NA` for non-"p" events |
| Pinniped group size | PinnipedGs | `NA` for non-"p" events |
| Boat or gear type | BoatType | `NA` for non-"F" events |
| Number of boats | BoatGs | `NA` for non-"F" events |

ObsEstimate refers to the code of the observer that made the
corresponding estimate. For the "wide" format, ObsEstimate is a
list-column of all of the observer codes that provided an estimate. Also
in the "wide" format, the GsSpBest# columns are the product of the means
of GsSchoolBest and the corresponding species percentage (see the
Default section for calculation details). These numbers, 1 to 4,
correspond to the order of the data as it appears in the DAS file

## Examples

``` r
y <- system.file("extdata", "das_sample.das", package = "swfscDAS")
y.proc <- das_process(y)

das_sight(y.proc)
#>    Event            DateTime      Lat       Lon OnEffort Cruise Mode OffsetGMT
#> 1      S 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 2      S 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 3      s 2013-01-13 08:06:00 39.54217 -137.5263    FALSE   1000    C         5
#> 4      s 2013-01-13 08:08:58 39.55000 -137.5255    FALSE   1000    C         5
#> 5      s 2013-01-13 08:15:32 39.56000 -137.5218    FALSE   1000    C         5
#> 6      s 2013-01-13 08:17:28 39.56233 -137.5210    FALSE   1000    C         5
#> 7      s 2013-01-13 08:19:39 39.56483 -137.5197    FALSE   1000    C         5
#> 8      s 2013-01-13 08:26:45 39.56400 -137.5140    FALSE   1000    C         5
#> 9      t 2013-01-13 09:34:27 39.59733 -137.4400     TRUE   1000    C         5
#> 10     t 2013-01-13 13:35:18 40.14000 -137.2048    FALSE   1000    C         5
#> 11     t 2013-01-13 14:02:55 40.18283 -137.1622     TRUE   1000    C         5
#> 12     S 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 13     t 2013-01-13 15:36:47 40.36050 -137.0978     TRUE   1000    C         5
#> 14     S 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 15     s 2013-01-13 16:36:34 40.46667 -137.0570    FALSE   1000    C         5
#> 16     S 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 17     S 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 18     t 2013-01-13 17:08:11 40.52083 -137.0383    FALSE   1000    C         5
#> 19     t 2013-01-13 17:45:10 40.50450 -137.0437    FALSE   1000    C         5
#> 20     S 2013-01-14 09:17:21 40.75183 -135.7748    FALSE   1000 <NA>         5
#> 21     F 2013-01-14 11:25:32 40.98950 -135.5965     TRUE   1000    C         5
#> 22     S 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 23     S 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 24     S 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 25     S 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#>    EffType ESWsides Trackline Course SpdKt Bft SwellHght SwellDir WindSpdKt
#> 1        S        2      <NA>     25  10.2   3         3      230        10
#> 2        S        2      <NA>     26   9.7   3         3      230        10
#> 3        S        2      <NA>     26   9.7   3         3      230        10
#> 4        S        2      <NA>     26   9.7   3         3      230        10
#> 5        S        2      <NA>     26   9.7   3         3      230        10
#> 6        S        2      <NA>     26   9.7   3         3      230        10
#> 7        S        2      <NA>     26   9.7   3         3      230        10
#> 8        S        2      <NA>     26   9.7   3         3      230        10
#> 9        S        2      <NA>     27   9.0   3         3      230        10
#> 10       S        2      <NA>     35   9.2   3         3      230         9
#> 11       S        2      <NA>     20   9.3   3         3      230         6
#> 12       S        2      <NA>     20   9.3   3         3      230         6
#> 13       S        2      <NA>     16   8.9   2         3      230         6
#> 14       S        2      <NA>     25   8.9   2         3      230         6
#> 15       S        2      <NA>     25   8.9   2         3      230         6
#> 16       S        2      <NA>     30   9.5   2         3      230         6
#> 17       S        2      <NA>     30   9.5   2         3      230         6
#> 18       S        2      <NA>     30   9.5   2         3      230         6
#> 19       S        2      <NA>     30   9.5   2         3      230         6
#> 20    <NA>       NA      <NA>     NA    NA  NA        NA       NA        NA
#> 21       S        2      <NA>     35   9.5   2         1       35         5
#> 22       S        2      <NA>     23   9.6   2         1       35         5
#> 23       S        2      <NA>     23   9.6   2         1       35         5
#> 24       S        2      <NA>     23   9.6   2         1       35         5
#> 25       S        2      <NA>     23   9.6   2         1       35         5
#>    WindDir RainFog HorizSun VertSun Glare Vis ObsL  Rec ObsR ObsInd EffortDot
#> 1      257       1       NA      NA    NA 6.0  208  280  001   <NA>      TRUE
#> 2      257       3        2       2 FALSE 5.5  125  208  280   <NA>      TRUE
#> 3      257       3        2       2 FALSE 5.5  125  208  280   <NA>     FALSE
#> 4      257       3        2       2 FALSE 5.5  125  208  280   <NA>     FALSE
#> 5      257       3        2       2 FALSE 5.5  125  208  280   <NA>     FALSE
#> 6      257       3        2       2 FALSE 5.5  125  208  280   <NA>     FALSE
#> 7      257       3        2       2 FALSE 5.5  125  208  280   <NA>     FALSE
#> 8      257       3        2       2 FALSE 5.5  125  208  280   <NA>     FALSE
#> 9      257       1        2       2 FALSE 5.5  001  126  149   <NA>      TRUE
#> 10     257       1       12      12 FALSE 6.0  126  149  125   <NA>     FALSE
#> 11     257       1        8       1 FALSE 6.0  280  001  126   <NA>      TRUE
#> 12     257       1        8       1 FALSE 6.0  280  001  126   <NA>      TRUE
#> 13     243       1        9       1 FALSE 6.0  125  208  280   <NA>      TRUE
#> 14     243       1        8       2 FALSE 6.0  149  125  208   <NA>      TRUE
#> 15     243       1        8       2 FALSE 6.0  149  125  208   <NA>     FALSE
#> 16     243       1        8       2 FALSE 6.0  126  149  125   <NA>      TRUE
#> 17     243       1        8       2 FALSE 6.0  126  149  125   <NA>      TRUE
#> 18     243       1        8       2 FALSE 6.0  126  149  125   <NA>     FALSE
#> 19     243       1        8       2 FALSE 6.0  126  149  125   <NA>     FALSE
#> 20      NA      NA       NA      NA    NA  NA <NA> <NA> <NA>   <NA>     FALSE
#> 21      40       3       NA      NA    NA 4.0  149  125  208   <NA>      TRUE
#> 22      40       3       NA      NA    NA 4.0  149  125  208   <NA>      TRUE
#> 23      40       3       NA      NA    NA 4.0  149  125  208   <NA>      TRUE
#> 24      40       3       NA      NA    NA 4.0  149  125  208   <NA>      TRUE
#> 25      40       3       NA      NA    NA 4.0  149  125  208   <NA>      TRUE
#>    EventNum       file_das line_num SightNo Subgroup SightNoDaily  Obs ObsStd
#> 1        15 das_sample.das       15    1406     <NA>   20130113_1  208   TRUE
#> 2        35 das_sample.das       38    1407     <NA>   20130113_2  125   TRUE
#> 3        39 das_sample.das       45    1407     <NA>         <NA> <NA>  FALSE
#> 4        41 das_sample.das       47    1407     <NA>         <NA> <NA>  FALSE
#> 5        42 das_sample.das       48    1407     <NA>         <NA> <NA>  FALSE
#> 6        44 das_sample.das       50    1407     <NA>         <NA> <NA>  FALSE
#> 7        45 das_sample.das       51    1407     <NA>         <NA> <NA>  FALSE
#> 8        46 das_sample.das       52    1407     <NA>         <NA> <NA>  FALSE
#> 9        59 das_sample.das       65    <NA>     <NA>         <NA>  280  FALSE
#> 10      118 das_sample.das      124    <NA>     <NA>         <NA>  149   TRUE
#> 11      131 das_sample.das      137    <NA>     <NA>         <NA>  228  FALSE
#> 12      136 das_sample.das      142    1408     <NA>   20130113_3  280   TRUE
#> 13      153 das_sample.das      162    <NA>     <NA>         <NA>  231  FALSE
#> 14      167 das_sample.das      176    1409     <NA>   20130113_4  149   TRUE
#> 15      170 das_sample.das      182    1409     <NA>         <NA> <NA>  FALSE
#> 16      181 das_sample.das      193    1410     <NA>   20130113_5  125   TRUE
#> 17      181 das_sample.das      193    1410     <NA>   20130113_5  125   TRUE
#> 18      186 das_sample.das      202    <NA>     <NA>         <NA>  280  FALSE
#> 19      190 das_sample.das      206    <NA>     <NA>         <NA>  099  FALSE
#> 20      199 das_sample.das      215    1411     <NA>   20130114_1  280  FALSE
#> 21        8 das_sample.das      238    <NA>     <NA>         <NA>  149   TRUE
#> 22       18 das_sample.das      248    1412     <NA>   20130114_2  149   TRUE
#> 23       18 das_sample.das      248    1412     <NA>   20130114_2  149   TRUE
#> 24       20 das_sample.das      252    1413     <NA>   20130114_3  208   TRUE
#> 25       20 das_sample.das      252    1413     <NA>   20130114_3  208   TRUE
#>    Bearing Reticle DistNm Cue Method Photos Birds CalibSchool PhotosAerial
#> 1      309     2.8   1.06   3      4      N     N        <NA>         <NA>
#> 2      326     0.4   2.97   3      4      Y     N        <NA>         <NA>
#> 3       11     2.0   1.30  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 4        5     3.5   0.90  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 5       50      NA   0.50  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 6       71     4.5   0.70  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 7      104     4.5   0.70  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 8        2     2.2   1.30  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 9      120      NA   0.03  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 10     270      NA   0.03  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 11     300      NA   0.02  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 12     270    14.0   0.28   3      4      N     N        <NA>         <NA>
#> 13      45      NA   0.05  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 14     344     0.2   3.68   3      4      Y     Y        <NA>         <NA>
#> 15     356     0.4   3.00  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 16      70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 17      70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 18      42    17.0   0.23  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 19     180      NA   0.01  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 20       0      NA   0.00   3      1      N     N        <NA>         <NA>
#> 21     309     1.7   1.47  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 22     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 23     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 24      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#> 25      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#>    Biopsy  Prob nSp Mixed SpCode SpCodeProb GsSchoolBest GsSchoolHigh
#> 1    <NA> FALSE   1 FALSE    018       <NA>           NA           NA
#> 2    <NA> FALSE   1 FALSE    076       <NA>      8.00000        14.00
#> 3    <NA>    NA  NA    NA   <NA>       <NA>           NA           NA
#> 4    <NA>    NA  NA    NA   <NA>       <NA>           NA           NA
#> 5    <NA>    NA  NA    NA   <NA>       <NA>           NA           NA
#> 6    <NA>    NA  NA    NA   <NA>       <NA>           NA           NA
#> 7    <NA>    NA  NA    NA   <NA>       <NA>           NA           NA
#> 8    <NA>    NA  NA    NA   <NA>       <NA>           NA           NA
#> 9    <NA>    NA  NA    NA     LV       <NA>      1.00000           NA
#> 10   <NA>    NA  NA    NA     DC       <NA>      2.00000           NA
#> 11   <NA>    NA  NA    NA     DC       <NA>      1.00000           NA
#> 12   <NA> FALSE   1 FALSE    037       <NA>     10.66667        20.00
#> 13   <NA>    NA  NA    NA     DC       <NA>      1.00000           NA
#> 14   <NA> FALSE   1 FALSE    016       <NA>     46.66667        79.00
#> 15   <NA>    NA  NA    NA   <NA>       <NA>           NA           NA
#> 16   <NA> FALSE   2  TRUE    013       <NA>     41.75000        72.75
#> 17   <NA> FALSE   2  TRUE    016       <NA>     41.75000        72.75
#> 18   <NA>    NA  NA    NA     DC       <NA>      2.00000           NA
#> 19   <NA>    NA  NA    NA     LV       <NA>      1.00000           NA
#> 20   <NA> FALSE   1 FALSE    075       <NA>           NA           NA
#> 21   <NA>    NA  NA    NA   <NA>       <NA>           NA           NA
#> 22   <NA> FALSE   2  TRUE    018       <NA>    151.50000       249.00
#> 23   <NA> FALSE   2  TRUE    277       <NA>    151.50000       249.00
#> 24   <NA>  TRUE   2  TRUE    016        016     21.25000        37.75
#> 25   <NA>  TRUE   2  TRUE    277        016     21.25000        37.75
#>    GsSchoolLow  GsSpBest GsSpHigh    GsSpLow CourseSchool TurtleJFR TurtleAge
#> 1    42.333333        NA       NA  42.333333           NA      <NA>      <NA>
#> 2     5.666667   8.00000  14.0000   5.666667           NA      <NA>      <NA>
#> 3           NA        NA       NA         NA           NA      <NA>      <NA>
#> 4           NA        NA       NA         NA           NA      <NA>      <NA>
#> 5           NA        NA       NA         NA           NA      <NA>      <NA>
#> 6           NA        NA       NA         NA          100      <NA>      <NA>
#> 7           NA        NA       NA         NA          100      <NA>      <NA>
#> 8           NA        NA       NA         NA           NA      <NA>      <NA>
#> 9           NA   1.00000       NA         NA           NA      <NA>         A
#> 10          NA   2.00000       NA         NA           NA      <NA>         A
#> 11          NA   1.00000       NA         NA           NA      <NA>         J
#> 12   10.666667  10.66667  20.0000  10.666667           NA      <NA>      <NA>
#> 13          NA   1.00000       NA         NA           NA      <NA>         A
#> 14   46.666667  46.66667  79.0000  46.666667           NA      <NA>      <NA>
#> 15          NA        NA       NA         NA           NA      <NA>      <NA>
#> 16   41.750000  30.06000  52.3800  30.060000           NA      <NA>      <NA>
#> 17   41.750000  11.69000  20.3700  11.690000           NA      <NA>      <NA>
#> 18          NA   2.00000       NA         NA           NA         F         A
#> 19          NA   1.00000       NA         NA           NA      <NA>         A
#> 20          NA        NA       NA         NA           NA      <NA>      <NA>
#> 21          NA        NA       NA         NA           NA      <NA>      <NA>
#> 22  151.500000 128.77500 211.6500 128.775000           NA      <NA>      <NA>
#> 23  151.500000  22.72500  37.3500  22.725000           NA      <NA>      <NA>
#> 24   21.250000  15.08750  26.8025  15.087500           NA      <NA>      <NA>
#> 25   21.250000   6.16250  10.9475   6.162500           NA      <NA>      <NA>
#>    TurtleCapt   PerpDistKm
#> 1        <NA> 1.525631e+00
#> 2        <NA> 3.075807e+00
#> 3        <NA> 4.593917e-01
#> 4        <NA> 1.452712e-01
#> 5        <NA> 7.093572e-01
#> 6        <NA> 1.225770e+00
#> 7        <NA> 1.257891e+00
#> 8        <NA> 8.402403e-02
#> 9           N 4.811637e-02
#> 10          N 5.556000e-02
#> 11          N 3.207758e-02
#> 12       <NA> 5.185600e-01
#> 13       <NA> 6.547809e-02
#> 14       <NA> 1.878568e+00
#> 15       <NA> 3.875670e-01
#> 16       <NA> 2.888916e+00
#> 17       <NA> 2.888916e+00
#> 18          Y 2.850229e-01
#> 19          N 2.268046e-18
#> 20       <NA> 0.000000e+00
#> 21       <NA> 2.115733e+00
#> 22       <NA> 1.060157e-01
#> 23       <NA> 1.060157e-01
#> 24       <NA> 2.542657e+00
#> 25       <NA> 2.542657e+00
das_sight(y.proc, return.format = "complete")
#>    Event            DateTime      Lat       Lon OnEffort Cruise Mode OffsetGMT
#> 1      S 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 2      S 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 3      S 2013-01-13 06:46:02 39.36617 -137.5820     TRUE   1000    C         5
#> 4      S 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 5      S 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 6      S 2013-01-13 07:56:22 39.51767 -137.5285     TRUE   1000    C         5
#> 7      s 2013-01-13 08:06:00 39.54217 -137.5263    FALSE   1000    C         5
#> 8      s 2013-01-13 08:08:58 39.55000 -137.5255    FALSE   1000    C         5
#> 9      s 2013-01-13 08:15:32 39.56000 -137.5218    FALSE   1000    C         5
#> 10     s 2013-01-13 08:17:28 39.56233 -137.5210    FALSE   1000    C         5
#> 11     s 2013-01-13 08:19:39 39.56483 -137.5197    FALSE   1000    C         5
#> 12     s 2013-01-13 08:26:45 39.56400 -137.5140    FALSE   1000    C         5
#> 13     t 2013-01-13 09:34:27 39.59733 -137.4400     TRUE   1000    C         5
#> 14     t 2013-01-13 13:35:18 40.14000 -137.2048    FALSE   1000    C         5
#> 15     t 2013-01-13 14:02:55 40.18283 -137.1622     TRUE   1000    C         5
#> 16     S 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 17     S 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 18     S 2013-01-13 14:37:56 40.26567 -137.1350     TRUE   1000    C         5
#> 19     t 2013-01-13 15:36:47 40.36050 -137.0978     TRUE   1000    C         5
#> 20     S 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 21     S 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 22     S 2013-01-13 16:29:50 40.45133 -137.0628     TRUE   1000    C         5
#> 23     s 2013-01-13 16:36:34 40.46667 -137.0570    FALSE   1000    C         5
#> 24     S 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 25     S 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 26     S 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 27     S 2013-01-13 17:00:45 40.52400 -137.0522     TRUE   1000    C         5
#> 28     t 2013-01-13 17:08:11 40.52083 -137.0383    FALSE   1000    C         5
#> 29     t 2013-01-13 17:45:10 40.50450 -137.0437    FALSE   1000    C         5
#> 30     S 2013-01-14 09:17:21 40.75183 -135.7748    FALSE   1000 <NA>         5
#> 31     F 2013-01-14 11:25:32 40.98950 -135.5965     TRUE   1000    C         5
#> 32     S 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 33     S 2013-01-14 11:47:51 41.04017 -135.5635     TRUE   1000    C         5
#> 34     S 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 35     S 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 36     S 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#> 37     S 2013-01-14 11:49:14 41.04333 -135.5615     TRUE   1000    C         5
#>    EffType ESWsides Trackline Course SpdKt Bft SwellHght SwellDir WindSpdKt
#> 1        S        2      <NA>     25  10.2   3         3      230        10
#> 2        S        2      <NA>     25  10.2   3         3      230        10
#> 3        S        2      <NA>     25  10.2   3         3      230        10
#> 4        S        2      <NA>     26   9.7   3         3      230        10
#> 5        S        2      <NA>     26   9.7   3         3      230        10
#> 6        S        2      <NA>     26   9.7   3         3      230        10
#> 7        S        2      <NA>     26   9.7   3         3      230        10
#> 8        S        2      <NA>     26   9.7   3         3      230        10
#> 9        S        2      <NA>     26   9.7   3         3      230        10
#> 10       S        2      <NA>     26   9.7   3         3      230        10
#> 11       S        2      <NA>     26   9.7   3         3      230        10
#> 12       S        2      <NA>     26   9.7   3         3      230        10
#> 13       S        2      <NA>     27   9.0   3         3      230        10
#> 14       S        2      <NA>     35   9.2   3         3      230         9
#> 15       S        2      <NA>     20   9.3   3         3      230         6
#> 16       S        2      <NA>     20   9.3   3         3      230         6
#> 17       S        2      <NA>     20   9.3   3         3      230         6
#> 18       S        2      <NA>     20   9.3   3         3      230         6
#> 19       S        2      <NA>     16   8.9   2         3      230         6
#> 20       S        2      <NA>     25   8.9   2         3      230         6
#> 21       S        2      <NA>     25   8.9   2         3      230         6
#> 22       S        2      <NA>     25   8.9   2         3      230         6
#> 23       S        2      <NA>     25   8.9   2         3      230         6
#> 24       S        2      <NA>     30   9.5   2         3      230         6
#> 25       S        2      <NA>     30   9.5   2         3      230         6
#> 26       S        2      <NA>     30   9.5   2         3      230         6
#> 27       S        2      <NA>     30   9.5   2         3      230         6
#> 28       S        2      <NA>     30   9.5   2         3      230         6
#> 29       S        2      <NA>     30   9.5   2         3      230         6
#> 30    <NA>       NA      <NA>     NA    NA  NA        NA       NA        NA
#> 31       S        2      <NA>     35   9.5   2         1       35         5
#> 32       S        2      <NA>     23   9.6   2         1       35         5
#> 33       S        2      <NA>     23   9.6   2         1       35         5
#> 34       S        2      <NA>     23   9.6   2         1       35         5
#> 35       S        2      <NA>     23   9.6   2         1       35         5
#> 36       S        2      <NA>     23   9.6   2         1       35         5
#> 37       S        2      <NA>     23   9.6   2         1       35         5
#>    WindDir RainFog HorizSun VertSun Glare Vis ObsL  Rec ObsR ObsInd EffortDot
#> 1      257       1       NA      NA    NA 6.0  208  280  001   <NA>      TRUE
#> 2      257       1       NA      NA    NA 6.0  208  280  001   <NA>      TRUE
#> 3      257       1       NA      NA    NA 6.0  208  280  001   <NA>      TRUE
#> 4      257       3        2       2 FALSE 5.5  125  208  280   <NA>      TRUE
#> 5      257       3        2       2 FALSE 5.5  125  208  280   <NA>      TRUE
#> 6      257       3        2       2 FALSE 5.5  125  208  280   <NA>      TRUE
#> 7      257       3        2       2 FALSE 5.5  125  208  280   <NA>     FALSE
#> 8      257       3        2       2 FALSE 5.5  125  208  280   <NA>     FALSE
#> 9      257       3        2       2 FALSE 5.5  125  208  280   <NA>     FALSE
#> 10     257       3        2       2 FALSE 5.5  125  208  280   <NA>     FALSE
#> 11     257       3        2       2 FALSE 5.5  125  208  280   <NA>     FALSE
#> 12     257       3        2       2 FALSE 5.5  125  208  280   <NA>     FALSE
#> 13     257       1        2       2 FALSE 5.5  001  126  149   <NA>      TRUE
#> 14     257       1       12      12 FALSE 6.0  126  149  125   <NA>     FALSE
#> 15     257       1        8       1 FALSE 6.0  280  001  126   <NA>      TRUE
#> 16     257       1        8       1 FALSE 6.0  280  001  126   <NA>      TRUE
#> 17     257       1        8       1 FALSE 6.0  280  001  126   <NA>      TRUE
#> 18     257       1        8       1 FALSE 6.0  280  001  126   <NA>      TRUE
#> 19     243       1        9       1 FALSE 6.0  125  208  280   <NA>      TRUE
#> 20     243       1        8       2 FALSE 6.0  149  125  208   <NA>      TRUE
#> 21     243       1        8       2 FALSE 6.0  149  125  208   <NA>      TRUE
#> 22     243       1        8       2 FALSE 6.0  149  125  208   <NA>      TRUE
#> 23     243       1        8       2 FALSE 6.0  149  125  208   <NA>     FALSE
#> 24     243       1        8       2 FALSE 6.0  126  149  125   <NA>      TRUE
#> 25     243       1        8       2 FALSE 6.0  126  149  125   <NA>      TRUE
#> 26     243       1        8       2 FALSE 6.0  126  149  125   <NA>      TRUE
#> 27     243       1        8       2 FALSE 6.0  126  149  125   <NA>      TRUE
#> 28     243       1        8       2 FALSE 6.0  126  149  125   <NA>     FALSE
#> 29     243       1        8       2 FALSE 6.0  126  149  125   <NA>     FALSE
#> 30      NA      NA       NA      NA    NA  NA <NA> <NA> <NA>   <NA>     FALSE
#> 31      40       3       NA      NA    NA 4.0  149  125  208   <NA>      TRUE
#> 32      40       3       NA      NA    NA 4.0  149  125  208   <NA>      TRUE
#> 33      40       3       NA      NA    NA 4.0  149  125  208   <NA>      TRUE
#> 34      40       3       NA      NA    NA 4.0  149  125  208   <NA>      TRUE
#> 35      40       3       NA      NA    NA 4.0  149  125  208   <NA>      TRUE
#> 36      40       3       NA      NA    NA 4.0  149  125  208   <NA>      TRUE
#> 37      40       3       NA      NA    NA 4.0  149  125  208   <NA>      TRUE
#>    EventNum       file_das line_num SightNo Subgroup SightNoDaily  Obs ObsStd
#> 1        15 das_sample.das       15    1406     <NA>   20130113_1  208   TRUE
#> 2        15 das_sample.das       15    1406     <NA>   20130113_1  208   TRUE
#> 3        15 das_sample.das       15    1406     <NA>   20130113_1  208   TRUE
#> 4        35 das_sample.das       38    1407     <NA>   20130113_2  125   TRUE
#> 5        35 das_sample.das       38    1407     <NA>   20130113_2  125   TRUE
#> 6        35 das_sample.das       38    1407     <NA>   20130113_2  125   TRUE
#> 7        39 das_sample.das       45    1407     <NA>         <NA> <NA>  FALSE
#> 8        41 das_sample.das       47    1407     <NA>         <NA> <NA>  FALSE
#> 9        42 das_sample.das       48    1407     <NA>         <NA> <NA>  FALSE
#> 10       44 das_sample.das       50    1407     <NA>         <NA> <NA>  FALSE
#> 11       45 das_sample.das       51    1407     <NA>         <NA> <NA>  FALSE
#> 12       46 das_sample.das       52    1407     <NA>         <NA> <NA>  FALSE
#> 13       59 das_sample.das       65    <NA>     <NA>         <NA>  280  FALSE
#> 14      118 das_sample.das      124    <NA>     <NA>         <NA>  149   TRUE
#> 15      131 das_sample.das      137    <NA>     <NA>         <NA>  228  FALSE
#> 16      136 das_sample.das      142    1408     <NA>   20130113_3  280   TRUE
#> 17      136 das_sample.das      142    1408     <NA>   20130113_3  280   TRUE
#> 18      136 das_sample.das      142    1408     <NA>   20130113_3  280   TRUE
#> 19      153 das_sample.das      162    <NA>     <NA>         <NA>  231  FALSE
#> 20      167 das_sample.das      176    1409     <NA>   20130113_4  149   TRUE
#> 21      167 das_sample.das      176    1409     <NA>   20130113_4  149   TRUE
#> 22      167 das_sample.das      176    1409     <NA>   20130113_4  149   TRUE
#> 23      170 das_sample.das      182    1409     <NA>         <NA> <NA>  FALSE
#> 24      181 das_sample.das      193    1410     <NA>   20130113_5  125   TRUE
#> 25      181 das_sample.das      193    1410     <NA>   20130113_5  125   TRUE
#> 26      181 das_sample.das      193    1410     <NA>   20130113_5  125   TRUE
#> 27      181 das_sample.das      193    1410     <NA>   20130113_5  125   TRUE
#> 28      186 das_sample.das      202    <NA>     <NA>         <NA>  280  FALSE
#> 29      190 das_sample.das      206    <NA>     <NA>         <NA>  099  FALSE
#> 30      199 das_sample.das      215    1411     <NA>   20130114_1  280  FALSE
#> 31        8 das_sample.das      238    <NA>     <NA>         <NA>  149   TRUE
#> 32       18 das_sample.das      248    1412     <NA>   20130114_2  149   TRUE
#> 33       18 das_sample.das      248    1412     <NA>   20130114_2  149   TRUE
#> 34       20 das_sample.das      252    1413     <NA>   20130114_3  208   TRUE
#> 35       20 das_sample.das      252    1413     <NA>   20130114_3  208   TRUE
#> 36       20 das_sample.das      252    1413     <NA>   20130114_3  208   TRUE
#> 37       20 das_sample.das      252    1413     <NA>   20130114_3  208   TRUE
#>    Bearing Reticle DistNm Cue Method Photos Birds CalibSchool PhotosAerial
#> 1      309     2.8   1.06   3      4      N     N        <NA>         <NA>
#> 2      309     2.8   1.06   3      4      N     N        <NA>         <NA>
#> 3      309     2.8   1.06   3      4      N     N        <NA>         <NA>
#> 4      326     0.4   2.97   3      4      Y     N        <NA>         <NA>
#> 5      326     0.4   2.97   3      4      Y     N        <NA>         <NA>
#> 6      326     0.4   2.97   3      4      Y     N        <NA>         <NA>
#> 7       11     2.0   1.30  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 8        5     3.5   0.90  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 9       50      NA   0.50  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 10      71     4.5   0.70  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 11     104     4.5   0.70  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 12       2     2.2   1.30  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 13     120      NA   0.03  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 14     270      NA   0.03  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 15     300      NA   0.02  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 16     270    14.0   0.28   3      4      N     N        <NA>         <NA>
#> 17     270    14.0   0.28   3      4      N     N        <NA>         <NA>
#> 18     270    14.0   0.28   3      4      N     N        <NA>         <NA>
#> 19      45      NA   0.05  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 20     344     0.2   3.68   3      4      Y     Y        <NA>         <NA>
#> 21     344     0.2   3.68   3      4      Y     Y        <NA>         <NA>
#> 22     344     0.2   3.68   3      4      Y     Y        <NA>         <NA>
#> 23     356     0.4   3.00  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 24      70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 25      70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 26      70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 27      70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 28      42    17.0   0.23  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 29     180      NA   0.01  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 30       0      NA   0.00   3      1      N     N        <NA>         <NA>
#> 31     309     1.7   1.47  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 32     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 33     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 34      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#> 35      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#> 36      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#> 37      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#>    Biopsy  Prob nSp Mixed ObsEstimate SpCode1 SpCode2 SpCode3 SpCode4
#> 1    <NA> FALSE   1 FALSE         280     018    <NA>    <NA>    <NA>
#> 2    <NA> FALSE   1 FALSE         001     018    <NA>    <NA>    <NA>
#> 3    <NA> FALSE   1 FALSE         208     018    <NA>    <NA>    <NA>
#> 4    <NA> FALSE   1 FALSE         280     076    <NA>    <NA>    <NA>
#> 5    <NA> FALSE   1 FALSE         001     076    <NA>    <NA>    <NA>
#> 6    <NA> FALSE   1 FALSE         125     076    <NA>    <NA>    <NA>
#> 7    <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 8    <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 9    <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 10   <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 11   <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 12   <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 13   <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 14   <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 15   <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 16   <NA> FALSE   1 FALSE         280     037    <NA>    <NA>    <NA>
#> 17   <NA> FALSE   1 FALSE         001     037    <NA>    <NA>    <NA>
#> 18   <NA> FALSE   1 FALSE         126     037    <NA>    <NA>    <NA>
#> 19   <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 20   <NA> FALSE   1 FALSE         125     016    <NA>    <NA>    <NA>
#> 21   <NA> FALSE   1 FALSE         149     016    <NA>    <NA>    <NA>
#> 22   <NA> FALSE   1 FALSE         208     016    <NA>    <NA>    <NA>
#> 23   <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 24   <NA> FALSE   2  TRUE         280     013     016    <NA>    <NA>
#> 25   <NA> FALSE   2  TRUE         125     013     016    <NA>    <NA>
#> 26   <NA> FALSE   2  TRUE         149     013     016    <NA>    <NA>
#> 27   <NA> FALSE   2  TRUE         126     013     016    <NA>    <NA>
#> 28   <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 29   <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 30   <NA> FALSE   1 FALSE         280     075    <NA>    <NA>    <NA>
#> 31   <NA>    NA  NA    NA        <NA>    <NA>    <NA>    <NA>    <NA>
#> 32   <NA> FALSE   2  TRUE         149     018     277    <NA>    <NA>
#> 33   <NA> FALSE   2  TRUE         126     018     277    <NA>    <NA>
#> 34   <NA>  TRUE   2  TRUE         125     016     277    <NA>    <NA>
#> 35   <NA>  TRUE   2  TRUE         208     016     277    <NA>    <NA>
#> 36   <NA>  TRUE   2  TRUE         149     016     277    <NA>    <NA>
#> 37   <NA>  TRUE   2  TRUE         126     016     277    <NA>    <NA>
#>    SpCodeProb1 SpCodeProb2 SpCodeProb3 SpCodeProb4 SpPerc1 SpPerc2 SpPerc3
#> 1         <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 2         <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 3         <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 4         <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 5         <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 6         <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 7         <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 8         <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 9         <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 10        <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 11        <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 12        <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 13        <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 14        <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 15        <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 16        <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 17        <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 18        <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 19        <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 20        <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 21        <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 22        <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 23        <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 24        <NA>        <NA>        <NA>        <NA>      68      32      NA
#> 25        <NA>        <NA>        <NA>        <NA>      75      25      NA
#> 26        <NA>        <NA>        <NA>        <NA>      65      35      NA
#> 27        <NA>        <NA>        <NA>        <NA>      80      20      NA
#> 28        <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 29        <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 30        <NA>        <NA>        <NA>        <NA>     100      NA      NA
#> 31        <NA>        <NA>        <NA>        <NA>      NA      NA      NA
#> 32        <NA>        <NA>        <NA>        <NA>      80      20      NA
#> 33        <NA>        <NA>        <NA>        <NA>      90      10      NA
#> 34         016         016        <NA>        <NA>      60      40      NA
#> 35         016         016        <NA>        <NA>      56      44      NA
#> 36         016         016        <NA>        <NA>      70      30      NA
#> 37         016         016        <NA>        <NA>      98       2      NA
#>    SpPerc4 GsSchoolBest GsSchoolHigh GsSchoolLow CourseSchool TurtleSp TurtleGs
#> 1       NA           NA           NA          43           NA     <NA>       NA
#> 2       NA           NA           NA          36           NA     <NA>       NA
#> 3       NA           NA           NA          48           NA     <NA>       NA
#> 4       NA            6           10           6           NA     <NA>       NA
#> 5       NA            9           10           2           NA     <NA>       NA
#> 6       NA            9           22           9           NA     <NA>       NA
#> 7       NA           NA           NA          NA           NA     <NA>       NA
#> 8       NA           NA           NA          NA           NA     <NA>       NA
#> 9       NA           NA           NA          NA           NA     <NA>       NA
#> 10      NA           NA           NA          NA          100     <NA>       NA
#> 11      NA           NA           NA          NA          100     <NA>       NA
#> 12      NA           NA           NA          NA           NA     <NA>       NA
#> 13      NA           NA           NA          NA           NA       LV        1
#> 14      NA           NA           NA          NA           NA       DC        2
#> 15      NA           NA           NA          NA           NA       DC        1
#> 16      NA           11           24          11           NA     <NA>       NA
#> 17      NA           12           23          12           NA     <NA>       NA
#> 18      NA            9           13           9           NA     <NA>       NA
#> 19      NA           NA           NA          NA           NA       DC        1
#> 20      NA           46           90          46           NA     <NA>       NA
#> 21      NA           28           65          28           NA     <NA>       NA
#> 22      NA           66           82          66           NA     <NA>       NA
#> 23      NA           NA           NA          NA           NA     <NA>       NA
#> 24      NA           37           72          37           NA     <NA>       NA
#> 25      NA           35           74          35           NA     <NA>       NA
#> 26      NA           29           52          29           NA     <NA>       NA
#> 27      NA           66           93          66           NA     <NA>       NA
#> 28      NA           NA           NA          NA           NA       DC        2
#> 29      NA           NA           NA          NA           NA       LV        1
#> 30      NA           NA           NA          NA           NA     <NA>       NA
#> 31      NA           NA           NA          NA           NA     <NA>       NA
#> 32      NA          183          328         183           NA     <NA>       NA
#> 33      NA          120          170         120           NA     <NA>       NA
#> 34      NA           21           60          21           NA     <NA>       NA
#> 35      NA           16           20          16           NA     <NA>       NA
#> 36      NA           12           18          12           NA     <NA>       NA
#> 37      NA           36           53          36           NA     <NA>       NA
#>    TurtleJFR TurtleAge TurtleCapt PinnipedSp PinnipedGs BoatType BoatGs
#> 1       <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 2       <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 3       <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 4       <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 5       <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 6       <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 7       <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 8       <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 9       <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 10      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 11      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 12      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 13      <NA>         A          N       <NA>         NA     <NA>     NA
#> 14      <NA>         A          N       <NA>         NA     <NA>     NA
#> 15      <NA>         J          N       <NA>         NA     <NA>     NA
#> 16      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 17      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 18      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 19      <NA>         A       <NA>       <NA>         NA     <NA>     NA
#> 20      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 21      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 22      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 23      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 24      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 25      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 26      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 27      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 28         F         A          Y       <NA>         NA     <NA>     NA
#> 29      <NA>         A          N       <NA>         NA     <NA>     NA
#> 30      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 31      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 32      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 33      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 34      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 35      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 36      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#> 37      <NA>      <NA>       <NA>       <NA>         NA     <NA>     NA
#>      PerpDistKm
#> 1  1.525631e+00
#> 2  1.525631e+00
#> 3  1.525631e+00
#> 4  3.075807e+00
#> 5  3.075807e+00
#> 6  3.075807e+00
#> 7  4.593917e-01
#> 8  1.452712e-01
#> 9  7.093572e-01
#> 10 1.225770e+00
#> 11 1.257891e+00
#> 12 8.402403e-02
#> 13 4.811637e-02
#> 14 5.556000e-02
#> 15 3.207758e-02
#> 16 5.185600e-01
#> 17 5.185600e-01
#> 18 5.185600e-01
#> 19 6.547809e-02
#> 20 1.878568e+00
#> 21 1.878568e+00
#> 22 1.878568e+00
#> 23 3.875670e-01
#> 24 2.888916e+00
#> 25 2.888916e+00
#> 26 2.888916e+00
#> 27 2.888916e+00
#> 28 2.850229e-01
#> 29 2.268046e-18
#> 30 0.000000e+00
#> 31 2.115733e+00
#> 32 1.060157e-01
#> 33 1.060157e-01
#> 34 2.542657e+00
#> 35 2.542657e+00
#> 36 2.542657e+00
#> 37 2.542657e+00
```
