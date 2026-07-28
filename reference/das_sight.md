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
GsSchool = school-level group size info

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
#>    EffType ESWsides Course SpdKt Bft SwellHght WindSpdKt RainFog HorizSun
#> 1        S        2     25  10.2   3         3        10       1       NA
#> 2        S        2     26   9.7   3         3        10       3        2
#> 3        S        2     26   9.7   3         3        10       3        2
#> 4        S        2     26   9.7   3         3        10       3        2
#> 5        S        2     26   9.7   3         3        10       3        2
#> 6        S        2     26   9.7   3         3        10       3        2
#> 7        S        2     26   9.7   3         3        10       3        2
#> 8        S        2     26   9.7   3         3        10       3        2
#> 9        S        2     27   9.0   3         3        10       1        2
#> 10       S        2     35   9.2   3         3         9       1       12
#> 11       S        2     20   9.3   3         3         6       1        8
#> 12       S        2     20   9.3   3         3         6       1        8
#> 13       S        2     16   8.9   2         3         6       1        9
#> 14       S        2     25   8.9   2         3         6       1        8
#> 15       S        2     25   8.9   2         3         6       1        8
#> 16       S        2     30   9.5   2         3         6       1        8
#> 17       S        2     30   9.5   2         3         6       1        8
#> 18       S        2     30   9.5   2         3         6       1        8
#> 19       S        2     30   9.5   2         3         6       1        8
#> 20    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 21       S        2     35   9.5   2         1         5       3       NA
#> 22       S        2     23   9.6   2         1         5       3       NA
#> 23       S        2     23   9.6   2         1         5       3       NA
#> 24       S        2     23   9.6   2         1         5       3       NA
#> 25       S        2     23   9.6   2         1         5       3       NA
#>    VertSun Glare Vis ObsL  Rec ObsR ObsInd EffortDot EventNum       file_das
#> 1       NA    NA 6.0  208  280  001   <NA>      TRUE       15 das_sample.das
#> 2        2 FALSE 5.5  125  208  280   <NA>      TRUE       35 das_sample.das
#> 3        2 FALSE 5.5  125  208  280   <NA>     FALSE       39 das_sample.das
#> 4        2 FALSE 5.5  125  208  280   <NA>     FALSE       41 das_sample.das
#> 5        2 FALSE 5.5  125  208  280   <NA>     FALSE       42 das_sample.das
#> 6        2 FALSE 5.5  125  208  280   <NA>     FALSE       44 das_sample.das
#> 7        2 FALSE 5.5  125  208  280   <NA>     FALSE       45 das_sample.das
#> 8        2 FALSE 5.5  125  208  280   <NA>     FALSE       46 das_sample.das
#> 9        2 FALSE 5.5  001  126  149   <NA>      TRUE       59 das_sample.das
#> 10      12 FALSE 6.0  126  149  125   <NA>     FALSE      118 das_sample.das
#> 11       1 FALSE 6.0  280  001  126   <NA>      TRUE      131 das_sample.das
#> 12       1 FALSE 6.0  280  001  126   <NA>      TRUE      136 das_sample.das
#> 13       1 FALSE 6.0  125  208  280   <NA>      TRUE      153 das_sample.das
#> 14       2 FALSE 6.0  149  125  208   <NA>      TRUE      167 das_sample.das
#> 15       2 FALSE 6.0  149  125  208   <NA>     FALSE      170 das_sample.das
#> 16       2 FALSE 6.0  126  149  125   <NA>      TRUE      181 das_sample.das
#> 17       2 FALSE 6.0  126  149  125   <NA>      TRUE      181 das_sample.das
#> 18       2 FALSE 6.0  126  149  125   <NA>     FALSE      186 das_sample.das
#> 19       2 FALSE 6.0  126  149  125   <NA>     FALSE      190 das_sample.das
#> 20      NA    NA  NA <NA> <NA> <NA>   <NA>     FALSE      199 das_sample.das
#> 21      NA    NA 4.0  149  125  208   <NA>      TRUE        8 das_sample.das
#> 22      NA    NA 4.0  149  125  208   <NA>      TRUE       18 das_sample.das
#> 23      NA    NA 4.0  149  125  208   <NA>      TRUE       18 das_sample.das
#> 24      NA    NA 4.0  149  125  208   <NA>      TRUE       20 das_sample.das
#> 25      NA    NA 4.0  149  125  208   <NA>      TRUE       20 das_sample.das
#>    line_num SightNo Subgroup SightNoDaily  Obs ObsStd Bearing Reticle DistNm
#> 1        15    1406     <NA>   20130113_1  208   TRUE     309     2.8   1.06
#> 2        38    1407     <NA>   20130113_2  125   TRUE     326     0.4   2.97
#> 3        45    1407     <NA>         <NA> <NA>  FALSE      11     2.0   1.30
#> 4        47    1407     <NA>         <NA> <NA>  FALSE       5     3.5   0.90
#> 5        48    1407     <NA>         <NA> <NA>  FALSE      50      NA   0.50
#> 6        50    1407     <NA>         <NA> <NA>  FALSE      71     4.5   0.70
#> 7        51    1407     <NA>         <NA> <NA>  FALSE     104     4.5   0.70
#> 8        52    1407     <NA>         <NA> <NA>  FALSE       2     2.2   1.30
#> 9        65    <NA>     <NA>         <NA>  280  FALSE     120      NA   0.03
#> 10      124    <NA>     <NA>         <NA>  149   TRUE     270      NA   0.03
#> 11      137    <NA>     <NA>         <NA>  228  FALSE     300      NA   0.02
#> 12      142    1408     <NA>   20130113_3  280   TRUE     270    14.0   0.28
#> 13      162    <NA>     <NA>         <NA>  231  FALSE      45      NA   0.05
#> 14      176    1409     <NA>   20130113_4  149   TRUE     344     0.2   3.68
#> 15      182    1409     <NA>         <NA> <NA>  FALSE     356     0.4   3.00
#> 16      193    1410     <NA>   20130113_5  125   TRUE      70     1.4   1.66
#> 17      193    1410     <NA>   20130113_5  125   TRUE      70     1.4   1.66
#> 18      202    <NA>     <NA>         <NA>  280  FALSE      42    17.0   0.23
#> 19      206    <NA>     <NA>         <NA>  099  FALSE     180      NA   0.01
#> 20      215    1411     <NA>   20130114_1  280  FALSE       0      NA   0.00
#> 21      238    <NA>     <NA>         <NA>  149   TRUE     309     1.7   1.47
#> 22      248    1412     <NA>   20130114_2  149   TRUE     359     0.3   3.28
#> 23      248    1412     <NA>   20130114_2  149   TRUE     359     0.3   3.28
#> 24      252    1413     <NA>   20130114_3  208   TRUE      38     0.8   2.23
#> 25      252    1413     <NA>   20130114_3  208   TRUE      38     0.8   2.23
#>    Cue Method Photos Birds CalibSchool PhotosAerial Biopsy  Prob nSp Mixed
#> 1    3      4      N     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 2    3      4      Y     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 3   NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 4   NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 5   NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 6   NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 7   NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 8   NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 9   NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 10  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 11  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 12   3      4      N     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 13  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 14   3      4      Y     Y        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 15  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 16   3      4      Y     N        <NA>         <NA>   <NA> FALSE   2  TRUE
#> 17   3      4      Y     N        <NA>         <NA>   <NA> FALSE   2  TRUE
#> 18  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 19  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 20   3      1      N     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 21  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 22   2      4      Y     N        <NA>         <NA>   <NA> FALSE   2  TRUE
#> 23   2      4      Y     N        <NA>         <NA>   <NA> FALSE   2  TRUE
#> 24   3      4      Y     N        <NA>         <NA>   <NA>  TRUE   2  TRUE
#> 25   3      4      Y     N        <NA>         <NA>   <NA>  TRUE   2  TRUE
#>    SpCode SpCodeProb GsSchoolBest GsSchoolHigh GsSchoolLow  GsSpBest GsSpHigh
#> 1     018       <NA>           NA           NA   42.333333        NA       NA
#> 2     076       <NA>      8.00000        14.00    5.666667   8.00000  14.0000
#> 3    <NA>       <NA>           NA           NA          NA        NA       NA
#> 4    <NA>       <NA>           NA           NA          NA        NA       NA
#> 5    <NA>       <NA>           NA           NA          NA        NA       NA
#> 6    <NA>       <NA>           NA           NA          NA        NA       NA
#> 7    <NA>       <NA>           NA           NA          NA        NA       NA
#> 8    <NA>       <NA>           NA           NA          NA        NA       NA
#> 9      LV       <NA>      1.00000           NA          NA   1.00000       NA
#> 10     DC       <NA>      2.00000           NA          NA   2.00000       NA
#> 11     DC       <NA>      1.00000           NA          NA   1.00000       NA
#> 12    037       <NA>     10.66667        20.00   10.666667  10.66667  20.0000
#> 13     DC       <NA>      1.00000           NA          NA   1.00000       NA
#> 14    016       <NA>     46.66667        79.00   46.666667  46.66667  79.0000
#> 15   <NA>       <NA>           NA           NA          NA        NA       NA
#> 16    013       <NA>     41.75000        72.75   41.750000  30.06000  52.3800
#> 17    016       <NA>     41.75000        72.75   41.750000  11.69000  20.3700
#> 18     DC       <NA>      2.00000           NA          NA   2.00000       NA
#> 19     LV       <NA>      1.00000           NA          NA   1.00000       NA
#> 20    075       <NA>           NA           NA          NA        NA       NA
#> 21   <NA>       <NA>           NA           NA          NA        NA       NA
#> 22    018       <NA>    151.50000       249.00  151.500000 128.77500 211.6500
#> 23    277       <NA>    151.50000       249.00  151.500000  22.72500  37.3500
#> 24    016        016     21.25000        37.75   21.250000  15.08750  26.8025
#> 25    277        016     21.25000        37.75   21.250000   6.16250  10.9475
#>       GsSpLow CourseSchool TurtleJFR TurtleAge TurtleCapt   PerpDistKm
#> 1   42.333333           NA      <NA>      <NA>       <NA> 1.525631e+00
#> 2    5.666667           NA      <NA>      <NA>       <NA> 3.075807e+00
#> 3          NA           NA      <NA>      <NA>       <NA> 4.593917e-01
#> 4          NA           NA      <NA>      <NA>       <NA> 1.452712e-01
#> 5          NA           NA      <NA>      <NA>       <NA> 7.093572e-01
#> 6          NA          100      <NA>      <NA>       <NA> 1.225770e+00
#> 7          NA          100      <NA>      <NA>       <NA> 1.257891e+00
#> 8          NA           NA      <NA>      <NA>       <NA> 8.402403e-02
#> 9          NA           NA      <NA>         A          N 4.811637e-02
#> 10         NA           NA      <NA>         A          N 5.556000e-02
#> 11         NA           NA      <NA>         J          N 3.207758e-02
#> 12  10.666667           NA      <NA>      <NA>       <NA> 5.185600e-01
#> 13         NA           NA      <NA>         A       <NA> 6.547809e-02
#> 14  46.666667           NA      <NA>      <NA>       <NA> 1.878568e+00
#> 15         NA           NA      <NA>      <NA>       <NA> 3.875670e-01
#> 16  30.060000           NA      <NA>      <NA>       <NA> 2.888916e+00
#> 17  11.690000           NA      <NA>      <NA>       <NA> 2.888916e+00
#> 18         NA           NA         F         A          Y 2.850229e-01
#> 19         NA           NA      <NA>         A          N 2.268046e-18
#> 20         NA           NA      <NA>      <NA>       <NA> 0.000000e+00
#> 21         NA           NA      <NA>      <NA>       <NA> 2.115733e+00
#> 22 128.775000           NA      <NA>      <NA>       <NA> 1.060157e-01
#> 23  22.725000           NA      <NA>      <NA>       <NA> 1.060157e-01
#> 24  15.087500           NA      <NA>      <NA>       <NA> 2.542657e+00
#> 25   6.162500           NA      <NA>      <NA>       <NA> 2.542657e+00
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
#>    EffType ESWsides Course SpdKt Bft SwellHght WindSpdKt RainFog HorizSun
#> 1        S        2     25  10.2   3         3        10       1       NA
#> 2        S        2     25  10.2   3         3        10       1       NA
#> 3        S        2     25  10.2   3         3        10       1       NA
#> 4        S        2     26   9.7   3         3        10       3        2
#> 5        S        2     26   9.7   3         3        10       3        2
#> 6        S        2     26   9.7   3         3        10       3        2
#> 7        S        2     26   9.7   3         3        10       3        2
#> 8        S        2     26   9.7   3         3        10       3        2
#> 9        S        2     26   9.7   3         3        10       3        2
#> 10       S        2     26   9.7   3         3        10       3        2
#> 11       S        2     26   9.7   3         3        10       3        2
#> 12       S        2     26   9.7   3         3        10       3        2
#> 13       S        2     27   9.0   3         3        10       1        2
#> 14       S        2     35   9.2   3         3         9       1       12
#> 15       S        2     20   9.3   3         3         6       1        8
#> 16       S        2     20   9.3   3         3         6       1        8
#> 17       S        2     20   9.3   3         3         6       1        8
#> 18       S        2     20   9.3   3         3         6       1        8
#> 19       S        2     16   8.9   2         3         6       1        9
#> 20       S        2     25   8.9   2         3         6       1        8
#> 21       S        2     25   8.9   2         3         6       1        8
#> 22       S        2     25   8.9   2         3         6       1        8
#> 23       S        2     25   8.9   2         3         6       1        8
#> 24       S        2     30   9.5   2         3         6       1        8
#> 25       S        2     30   9.5   2         3         6       1        8
#> 26       S        2     30   9.5   2         3         6       1        8
#> 27       S        2     30   9.5   2         3         6       1        8
#> 28       S        2     30   9.5   2         3         6       1        8
#> 29       S        2     30   9.5   2         3         6       1        8
#> 30    <NA>       NA     NA    NA  NA        NA        NA      NA       NA
#> 31       S        2     35   9.5   2         1         5       3       NA
#> 32       S        2     23   9.6   2         1         5       3       NA
#> 33       S        2     23   9.6   2         1         5       3       NA
#> 34       S        2     23   9.6   2         1         5       3       NA
#> 35       S        2     23   9.6   2         1         5       3       NA
#> 36       S        2     23   9.6   2         1         5       3       NA
#> 37       S        2     23   9.6   2         1         5       3       NA
#>    VertSun Glare Vis ObsL  Rec ObsR ObsInd EffortDot EventNum       file_das
#> 1       NA    NA 6.0  208  280  001   <NA>      TRUE       15 das_sample.das
#> 2       NA    NA 6.0  208  280  001   <NA>      TRUE       15 das_sample.das
#> 3       NA    NA 6.0  208  280  001   <NA>      TRUE       15 das_sample.das
#> 4        2 FALSE 5.5  125  208  280   <NA>      TRUE       35 das_sample.das
#> 5        2 FALSE 5.5  125  208  280   <NA>      TRUE       35 das_sample.das
#> 6        2 FALSE 5.5  125  208  280   <NA>      TRUE       35 das_sample.das
#> 7        2 FALSE 5.5  125  208  280   <NA>     FALSE       39 das_sample.das
#> 8        2 FALSE 5.5  125  208  280   <NA>     FALSE       41 das_sample.das
#> 9        2 FALSE 5.5  125  208  280   <NA>     FALSE       42 das_sample.das
#> 10       2 FALSE 5.5  125  208  280   <NA>     FALSE       44 das_sample.das
#> 11       2 FALSE 5.5  125  208  280   <NA>     FALSE       45 das_sample.das
#> 12       2 FALSE 5.5  125  208  280   <NA>     FALSE       46 das_sample.das
#> 13       2 FALSE 5.5  001  126  149   <NA>      TRUE       59 das_sample.das
#> 14      12 FALSE 6.0  126  149  125   <NA>     FALSE      118 das_sample.das
#> 15       1 FALSE 6.0  280  001  126   <NA>      TRUE      131 das_sample.das
#> 16       1 FALSE 6.0  280  001  126   <NA>      TRUE      136 das_sample.das
#> 17       1 FALSE 6.0  280  001  126   <NA>      TRUE      136 das_sample.das
#> 18       1 FALSE 6.0  280  001  126   <NA>      TRUE      136 das_sample.das
#> 19       1 FALSE 6.0  125  208  280   <NA>      TRUE      153 das_sample.das
#> 20       2 FALSE 6.0  149  125  208   <NA>      TRUE      167 das_sample.das
#> 21       2 FALSE 6.0  149  125  208   <NA>      TRUE      167 das_sample.das
#> 22       2 FALSE 6.0  149  125  208   <NA>      TRUE      167 das_sample.das
#> 23       2 FALSE 6.0  149  125  208   <NA>     FALSE      170 das_sample.das
#> 24       2 FALSE 6.0  126  149  125   <NA>      TRUE      181 das_sample.das
#> 25       2 FALSE 6.0  126  149  125   <NA>      TRUE      181 das_sample.das
#> 26       2 FALSE 6.0  126  149  125   <NA>      TRUE      181 das_sample.das
#> 27       2 FALSE 6.0  126  149  125   <NA>      TRUE      181 das_sample.das
#> 28       2 FALSE 6.0  126  149  125   <NA>     FALSE      186 das_sample.das
#> 29       2 FALSE 6.0  126  149  125   <NA>     FALSE      190 das_sample.das
#> 30      NA    NA  NA <NA> <NA> <NA>   <NA>     FALSE      199 das_sample.das
#> 31      NA    NA 4.0  149  125  208   <NA>      TRUE        8 das_sample.das
#> 32      NA    NA 4.0  149  125  208   <NA>      TRUE       18 das_sample.das
#> 33      NA    NA 4.0  149  125  208   <NA>      TRUE       18 das_sample.das
#> 34      NA    NA 4.0  149  125  208   <NA>      TRUE       20 das_sample.das
#> 35      NA    NA 4.0  149  125  208   <NA>      TRUE       20 das_sample.das
#> 36      NA    NA 4.0  149  125  208   <NA>      TRUE       20 das_sample.das
#> 37      NA    NA 4.0  149  125  208   <NA>      TRUE       20 das_sample.das
#>    line_num SightNo Subgroup SightNoDaily  Obs ObsStd Bearing Reticle DistNm
#> 1        15    1406     <NA>   20130113_1  208   TRUE     309     2.8   1.06
#> 2        15    1406     <NA>   20130113_1  208   TRUE     309     2.8   1.06
#> 3        15    1406     <NA>   20130113_1  208   TRUE     309     2.8   1.06
#> 4        38    1407     <NA>   20130113_2  125   TRUE     326     0.4   2.97
#> 5        38    1407     <NA>   20130113_2  125   TRUE     326     0.4   2.97
#> 6        38    1407     <NA>   20130113_2  125   TRUE     326     0.4   2.97
#> 7        45    1407     <NA>         <NA> <NA>  FALSE      11     2.0   1.30
#> 8        47    1407     <NA>         <NA> <NA>  FALSE       5     3.5   0.90
#> 9        48    1407     <NA>         <NA> <NA>  FALSE      50      NA   0.50
#> 10       50    1407     <NA>         <NA> <NA>  FALSE      71     4.5   0.70
#> 11       51    1407     <NA>         <NA> <NA>  FALSE     104     4.5   0.70
#> 12       52    1407     <NA>         <NA> <NA>  FALSE       2     2.2   1.30
#> 13       65    <NA>     <NA>         <NA>  280  FALSE     120      NA   0.03
#> 14      124    <NA>     <NA>         <NA>  149   TRUE     270      NA   0.03
#> 15      137    <NA>     <NA>         <NA>  228  FALSE     300      NA   0.02
#> 16      142    1408     <NA>   20130113_3  280   TRUE     270    14.0   0.28
#> 17      142    1408     <NA>   20130113_3  280   TRUE     270    14.0   0.28
#> 18      142    1408     <NA>   20130113_3  280   TRUE     270    14.0   0.28
#> 19      162    <NA>     <NA>         <NA>  231  FALSE      45      NA   0.05
#> 20      176    1409     <NA>   20130113_4  149   TRUE     344     0.2   3.68
#> 21      176    1409     <NA>   20130113_4  149   TRUE     344     0.2   3.68
#> 22      176    1409     <NA>   20130113_4  149   TRUE     344     0.2   3.68
#> 23      182    1409     <NA>         <NA> <NA>  FALSE     356     0.4   3.00
#> 24      193    1410     <NA>   20130113_5  125   TRUE      70     1.4   1.66
#> 25      193    1410     <NA>   20130113_5  125   TRUE      70     1.4   1.66
#> 26      193    1410     <NA>   20130113_5  125   TRUE      70     1.4   1.66
#> 27      193    1410     <NA>   20130113_5  125   TRUE      70     1.4   1.66
#> 28      202    <NA>     <NA>         <NA>  280  FALSE      42    17.0   0.23
#> 29      206    <NA>     <NA>         <NA>  099  FALSE     180      NA   0.01
#> 30      215    1411     <NA>   20130114_1  280  FALSE       0      NA   0.00
#> 31      238    <NA>     <NA>         <NA>  149   TRUE     309     1.7   1.47
#> 32      248    1412     <NA>   20130114_2  149   TRUE     359     0.3   3.28
#> 33      248    1412     <NA>   20130114_2  149   TRUE     359     0.3   3.28
#> 34      252    1413     <NA>   20130114_3  208   TRUE      38     0.8   2.23
#> 35      252    1413     <NA>   20130114_3  208   TRUE      38     0.8   2.23
#> 36      252    1413     <NA>   20130114_3  208   TRUE      38     0.8   2.23
#> 37      252    1413     <NA>   20130114_3  208   TRUE      38     0.8   2.23
#>    Cue Method Photos Birds CalibSchool PhotosAerial Biopsy  Prob nSp Mixed
#> 1    3      4      N     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 2    3      4      N     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 3    3      4      N     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 4    3      4      Y     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 5    3      4      Y     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 6    3      4      Y     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 7   NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 8   NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 9   NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 10  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 11  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 12  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 13  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 14  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 15  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 16   3      4      N     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 17   3      4      N     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 18   3      4      N     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 19  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 20   3      4      Y     Y        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 21   3      4      Y     Y        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 22   3      4      Y     Y        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 23  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 24   3      4      Y     N        <NA>         <NA>   <NA> FALSE   2  TRUE
#> 25   3      4      Y     N        <NA>         <NA>   <NA> FALSE   2  TRUE
#> 26   3      4      Y     N        <NA>         <NA>   <NA> FALSE   2  TRUE
#> 27   3      4      Y     N        <NA>         <NA>   <NA> FALSE   2  TRUE
#> 28  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 29  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 30   3      1      N     N        <NA>         <NA>   <NA> FALSE   1 FALSE
#> 31  NA     NA   <NA>  <NA>        <NA>         <NA>   <NA>    NA  NA    NA
#> 32   2      4      Y     N        <NA>         <NA>   <NA> FALSE   2  TRUE
#> 33   2      4      Y     N        <NA>         <NA>   <NA> FALSE   2  TRUE
#> 34   3      4      Y     N        <NA>         <NA>   <NA>  TRUE   2  TRUE
#> 35   3      4      Y     N        <NA>         <NA>   <NA>  TRUE   2  TRUE
#> 36   3      4      Y     N        <NA>         <NA>   <NA>  TRUE   2  TRUE
#> 37   3      4      Y     N        <NA>         <NA>   <NA>  TRUE   2  TRUE
#>    ObsEstimate SpCode1 SpCode2 SpCode3 SpCode4 SpCodeProb1 SpCodeProb2
#> 1          280     018    <NA>    <NA>    <NA>        <NA>        <NA>
#> 2          001     018    <NA>    <NA>    <NA>        <NA>        <NA>
#> 3          208     018    <NA>    <NA>    <NA>        <NA>        <NA>
#> 4          280     076    <NA>    <NA>    <NA>        <NA>        <NA>
#> 5          001     076    <NA>    <NA>    <NA>        <NA>        <NA>
#> 6          125     076    <NA>    <NA>    <NA>        <NA>        <NA>
#> 7         <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 8         <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 9         <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 10        <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 11        <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 12        <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 13        <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 14        <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 15        <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 16         280     037    <NA>    <NA>    <NA>        <NA>        <NA>
#> 17         001     037    <NA>    <NA>    <NA>        <NA>        <NA>
#> 18         126     037    <NA>    <NA>    <NA>        <NA>        <NA>
#> 19        <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 20         125     016    <NA>    <NA>    <NA>        <NA>        <NA>
#> 21         149     016    <NA>    <NA>    <NA>        <NA>        <NA>
#> 22         208     016    <NA>    <NA>    <NA>        <NA>        <NA>
#> 23        <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 24         280     013     016    <NA>    <NA>        <NA>        <NA>
#> 25         125     013     016    <NA>    <NA>        <NA>        <NA>
#> 26         149     013     016    <NA>    <NA>        <NA>        <NA>
#> 27         126     013     016    <NA>    <NA>        <NA>        <NA>
#> 28        <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 29        <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 30         280     075    <NA>    <NA>    <NA>        <NA>        <NA>
#> 31        <NA>    <NA>    <NA>    <NA>    <NA>        <NA>        <NA>
#> 32         149     018     277    <NA>    <NA>        <NA>        <NA>
#> 33         126     018     277    <NA>    <NA>        <NA>        <NA>
#> 34         125     016     277    <NA>    <NA>         016         016
#> 35         208     016     277    <NA>    <NA>         016         016
#> 36         149     016     277    <NA>    <NA>         016         016
#> 37         126     016     277    <NA>    <NA>         016         016
#>    SpCodeProb3 SpCodeProb4 SpPerc1 SpPerc2 SpPerc3 SpPerc4 GsSchoolBest
#> 1         <NA>        <NA>     100      NA      NA      NA           NA
#> 2         <NA>        <NA>     100      NA      NA      NA           NA
#> 3         <NA>        <NA>     100      NA      NA      NA           NA
#> 4         <NA>        <NA>     100      NA      NA      NA            6
#> 5         <NA>        <NA>     100      NA      NA      NA            9
#> 6         <NA>        <NA>     100      NA      NA      NA            9
#> 7         <NA>        <NA>      NA      NA      NA      NA           NA
#> 8         <NA>        <NA>      NA      NA      NA      NA           NA
#> 9         <NA>        <NA>      NA      NA      NA      NA           NA
#> 10        <NA>        <NA>      NA      NA      NA      NA           NA
#> 11        <NA>        <NA>      NA      NA      NA      NA           NA
#> 12        <NA>        <NA>      NA      NA      NA      NA           NA
#> 13        <NA>        <NA>      NA      NA      NA      NA           NA
#> 14        <NA>        <NA>      NA      NA      NA      NA           NA
#> 15        <NA>        <NA>      NA      NA      NA      NA           NA
#> 16        <NA>        <NA>     100      NA      NA      NA           11
#> 17        <NA>        <NA>     100      NA      NA      NA           12
#> 18        <NA>        <NA>     100      NA      NA      NA            9
#> 19        <NA>        <NA>      NA      NA      NA      NA           NA
#> 20        <NA>        <NA>     100      NA      NA      NA           46
#> 21        <NA>        <NA>     100      NA      NA      NA           28
#> 22        <NA>        <NA>     100      NA      NA      NA           66
#> 23        <NA>        <NA>      NA      NA      NA      NA           NA
#> 24        <NA>        <NA>      68      32      NA      NA           37
#> 25        <NA>        <NA>      75      25      NA      NA           35
#> 26        <NA>        <NA>      65      35      NA      NA           29
#> 27        <NA>        <NA>      80      20      NA      NA           66
#> 28        <NA>        <NA>      NA      NA      NA      NA           NA
#> 29        <NA>        <NA>      NA      NA      NA      NA           NA
#> 30        <NA>        <NA>     100      NA      NA      NA           NA
#> 31        <NA>        <NA>      NA      NA      NA      NA           NA
#> 32        <NA>        <NA>      80      20      NA      NA          183
#> 33        <NA>        <NA>      90      10      NA      NA          120
#> 34        <NA>        <NA>      60      40      NA      NA           21
#> 35        <NA>        <NA>      56      44      NA      NA           16
#> 36        <NA>        <NA>      70      30      NA      NA           12
#> 37        <NA>        <NA>      98       2      NA      NA           36
#>    GsSchoolHigh GsSchoolLow CourseSchool TurtleSp TurtleGs TurtleJFR TurtleAge
#> 1            NA          43           NA     <NA>       NA      <NA>      <NA>
#> 2            NA          36           NA     <NA>       NA      <NA>      <NA>
#> 3            NA          48           NA     <NA>       NA      <NA>      <NA>
#> 4            10           6           NA     <NA>       NA      <NA>      <NA>
#> 5            10           2           NA     <NA>       NA      <NA>      <NA>
#> 6            22           9           NA     <NA>       NA      <NA>      <NA>
#> 7            NA          NA           NA     <NA>       NA      <NA>      <NA>
#> 8            NA          NA           NA     <NA>       NA      <NA>      <NA>
#> 9            NA          NA           NA     <NA>       NA      <NA>      <NA>
#> 10           NA          NA          100     <NA>       NA      <NA>      <NA>
#> 11           NA          NA          100     <NA>       NA      <NA>      <NA>
#> 12           NA          NA           NA     <NA>       NA      <NA>      <NA>
#> 13           NA          NA           NA       LV        1      <NA>         A
#> 14           NA          NA           NA       DC        2      <NA>         A
#> 15           NA          NA           NA       DC        1      <NA>         J
#> 16           24          11           NA     <NA>       NA      <NA>      <NA>
#> 17           23          12           NA     <NA>       NA      <NA>      <NA>
#> 18           13           9           NA     <NA>       NA      <NA>      <NA>
#> 19           NA          NA           NA       DC        1      <NA>         A
#> 20           90          46           NA     <NA>       NA      <NA>      <NA>
#> 21           65          28           NA     <NA>       NA      <NA>      <NA>
#> 22           82          66           NA     <NA>       NA      <NA>      <NA>
#> 23           NA          NA           NA     <NA>       NA      <NA>      <NA>
#> 24           72          37           NA     <NA>       NA      <NA>      <NA>
#> 25           74          35           NA     <NA>       NA      <NA>      <NA>
#> 26           52          29           NA     <NA>       NA      <NA>      <NA>
#> 27           93          66           NA     <NA>       NA      <NA>      <NA>
#> 28           NA          NA           NA       DC        2         F         A
#> 29           NA          NA           NA       LV        1      <NA>         A
#> 30           NA          NA           NA     <NA>       NA      <NA>      <NA>
#> 31           NA          NA           NA     <NA>       NA      <NA>      <NA>
#> 32          328         183           NA     <NA>       NA      <NA>      <NA>
#> 33          170         120           NA     <NA>       NA      <NA>      <NA>
#> 34           60          21           NA     <NA>       NA      <NA>      <NA>
#> 35           20          16           NA     <NA>       NA      <NA>      <NA>
#> 36           18          12           NA     <NA>       NA      <NA>      <NA>
#> 37           53          36           NA     <NA>       NA      <NA>      <NA>
#>    TurtleCapt PinnipedSp PinnipedGs BoatType BoatGs   PerpDistKm
#> 1        <NA>       <NA>         NA     <NA>     NA 1.525631e+00
#> 2        <NA>       <NA>         NA     <NA>     NA 1.525631e+00
#> 3        <NA>       <NA>         NA     <NA>     NA 1.525631e+00
#> 4        <NA>       <NA>         NA     <NA>     NA 3.075807e+00
#> 5        <NA>       <NA>         NA     <NA>     NA 3.075807e+00
#> 6        <NA>       <NA>         NA     <NA>     NA 3.075807e+00
#> 7        <NA>       <NA>         NA     <NA>     NA 4.593917e-01
#> 8        <NA>       <NA>         NA     <NA>     NA 1.452712e-01
#> 9        <NA>       <NA>         NA     <NA>     NA 7.093572e-01
#> 10       <NA>       <NA>         NA     <NA>     NA 1.225770e+00
#> 11       <NA>       <NA>         NA     <NA>     NA 1.257891e+00
#> 12       <NA>       <NA>         NA     <NA>     NA 8.402403e-02
#> 13          N       <NA>         NA     <NA>     NA 4.811637e-02
#> 14          N       <NA>         NA     <NA>     NA 5.556000e-02
#> 15          N       <NA>         NA     <NA>     NA 3.207758e-02
#> 16       <NA>       <NA>         NA     <NA>     NA 5.185600e-01
#> 17       <NA>       <NA>         NA     <NA>     NA 5.185600e-01
#> 18       <NA>       <NA>         NA     <NA>     NA 5.185600e-01
#> 19       <NA>       <NA>         NA     <NA>     NA 6.547809e-02
#> 20       <NA>       <NA>         NA     <NA>     NA 1.878568e+00
#> 21       <NA>       <NA>         NA     <NA>     NA 1.878568e+00
#> 22       <NA>       <NA>         NA     <NA>     NA 1.878568e+00
#> 23       <NA>       <NA>         NA     <NA>     NA 3.875670e-01
#> 24       <NA>       <NA>         NA     <NA>     NA 2.888916e+00
#> 25       <NA>       <NA>         NA     <NA>     NA 2.888916e+00
#> 26       <NA>       <NA>         NA     <NA>     NA 2.888916e+00
#> 27       <NA>       <NA>         NA     <NA>     NA 2.888916e+00
#> 28          Y       <NA>         NA     <NA>     NA 2.850229e-01
#> 29          N       <NA>         NA     <NA>     NA 2.268046e-18
#> 30       <NA>       <NA>         NA     <NA>     NA 0.000000e+00
#> 31       <NA>       <NA>         NA     <NA>     NA 2.115733e+00
#> 32       <NA>       <NA>         NA     <NA>     NA 1.060157e-01
#> 33       <NA>       <NA>         NA     <NA>     NA 1.060157e-01
#> 34       <NA>       <NA>         NA     <NA>     NA 2.542657e+00
#> 35       <NA>       <NA>         NA     <NA>     NA 2.542657e+00
#> 36       <NA>       <NA>         NA     <NA>     NA 2.542657e+00
#> 37       <NA>       <NA>         NA     <NA>     NA 2.542657e+00
```
