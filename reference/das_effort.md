# Summarize DAS effort

Chop DAS data into effort segments

## Usage

``` r
das_effort(x, ...)

# S3 method for class 'data.frame'
das_effort(x, ...)

# S3 method for class 'das_df'
das_effort(
  x,
  method = c("condition", "equallength", "section"),
  conditions = NULL,
  strata.files = NULL,
  distance.method = c("greatcircle", "lawofcosines", "haversine", "vincenty"),
  seg0.drop = FALSE,
  comment.drop = FALSE,
  event.touse = NULL,
  num.cores = NULL,
  ...
)
```

## Arguments

- x:

  an object of class `das_df`, or a data frame that can be coerced to
  class `das_df`

- ...:

  arguments passed to the specified chopping function, such as `seg.km`
  or `seg.min.km`

- method:

  character; method to use to chop DAS data into effort segments Can be
  "condition", "equallength", "section", or any partial match thereof
  (case sensitive)

- conditions:

  character vector of names of conditions to include in segdata output.
  These values must be column names from the output of
  [`das_process`](https://swfsc.github.io/swfscDAS/reference/das_process.md),
  e.g. 'Bft', 'SwellHght', etc. If `method == "condition"`, then these
  also are the conditions which trigger segment chopping when they
  change. Only the following conditions can be used for chopping: 'Bft',
  'SwellHght', 'RainFog', 'HorizSun', 'VertSun', 'Glare', 'Vis',
  'Course', 'SpdKt'

- strata.files:

  list of path(s) of the CSV file(s) with points defining each stratum.
  The CSV files must contain headers and be a closed polygon. The list
  should be named; see the Details section. If `NULL` (the default),
  then no effort segments are not classified by strata.

- distance.method:

  character; method to use to calculate distance between lat/lon
  coordinates. Can be "greatcircle", "lawofcosines", "haversine",
  "vincenty", or any partial match thereof (case sensitive). Default is
  "greatcircle"

- seg0.drop:

  logical; flag indicating whether or not to drop segments of length 0
  that contain no sighting (S, K, M, G, t) events. Default is `FALSE`

- comment.drop:

  logical; flag indicating if comments ("C" events) should be ignored
  (i.e. position information should not be used) when segment chopping.
  Default is `FALSE`

- event.touse:

  character vector of events to use to determine segment lengths;
  overrides `comment.drop`. If `NULL` (the default), then all on effort
  events are used. If used, this argument must include at least R, E, S,
  and A events, and cannot include ? or 1:8 events

- num.cores:

  Number of CPUs to over which to distribute computations. Defaults to
  `NULL`, which uses one fewer than the number of cores reported by
  [`detectCores`](https://rdrr.io/r/parallel/detectCores.html). Using 1
  core likely will be faster for smaller datasets

## Value

List of three data frames:

- segdata: one row for every segment, and columns for information
  including unique segment number (segnum), the corresponding effort
  section (section_id), the segment index within the corresponding
  effort section (section_sub_id), the starting and ending line of the
  segment in the DAS file (stlin, endlin), start/end/midpoint
  coordinates(lat1/lon1, lat2/lon2, and mlat/mlon, respectively), the
  start/end/midpoint date/time of the segment (DateTime1, DateTime2, and
  mDateTime, respectively; mDateTime is the average of DateTime1 and
  DateTime2), segment length (dist), conditions (e.g. Beaufort), and, if
  applicable, stratum (InStratumName).

- sightinfo: details for all sightings in `x`, including: the unique
  segment number it is associated with, segment mid points (lat/lon),
  the 'included' column described in the 'Details' section, and the
  output information described in
  [`das_sight`](https://swfsc.github.io/swfscDAS/reference/das_sight.md)
  for `return.format` is "default"

- randpicks: see
  [`das_chop_equallength`](https://swfsc.github.io/swfscDAS/reference/das_chop_equallength.md);
  `NULL` if using "condition" method

## Details

This is the top-level function for chopping processed DAS data into
modeling segments (henceforth 'segments'), and assigning sightings and
related information (e.g., weather conditions) to each segment. This
function returns data frames with all relevant information for the
effort segments and associated sightings ('segdata' and 'sightinfo',
respectively). Before chopping, the DAS data is filtered for events
(rows) where either the 'OnEffort' column is `TRUE` or the 'Event'
column "E". In other words, the data is filtered for continuous effort
sections (henceforth 'effort sections'), where effort sections run from
"R" to "E" events (inclusive), and then passed to the chopping function
specified using `method`. Note that while B events immediately preceding
an R are on effort, they are ignored during effort chopping. In
addition, all on effort events (other than ? and numeric events) with
`NA` DateTime, Lat, or Lon values are verbosely removed.

If `strata.files` is not `NULL`, then the effort lines will be split by
the user-provided stratum (strata). In this case, a column 'stratum'
will be added to the end of the segdata data frame with the
user-provided name of the stratum that the segment was in, or `NA` if
the segment was not in any of the strata. If no name was provided for
the stratum in `strata.files`, then the value will be "Stratum#", where
"#" is the index of the applicable stratum in `strata.files`. While the
user can provide as many strata as they want, these strata can share
boundaries but they cannot overlap. See
[`das_effort_strata`](https://swfsc.github.io/swfscDAS/reference/das_effort_strata.md)
for more details.

The following chopping methods are currently available: "condition",
"equallength", and "section. When using the "condition" method, effort
sections are chopped into segments every time a condition changes,
thereby ensuring that the conditions are consistent across the entire
segment. See
[`das_chop_condition`](https://swfsc.github.io/swfscDAS/reference/das_chop_condition.md)
for more details about this method, including arguments that must be
passed to it via the argument `...`

The "equallength" method consists of chopping effort sections into
equal-length segments of length `seg.km`, and doing a weighted average
of the conditions for the length of that segment. See
[`das_chop_equallength`](https://swfsc.github.io/swfscDAS/reference/das_chop_equallength.md)
for more details about this method, including arguments that must be
passed to it via the argument `...`

The "section" method involves 'chopping' the effort into continuous
effort sections, i.e. each continuous effort section is a single effort
segment. See
[`das_chop_section`](https://swfsc.github.io/swfscDAS/reference/das_chop_section.md)
for more details about this method.

The distance between the lat/lon points of subsequent events is
calculated using the method specified in `distance.method`. If
"greatcircle",
[`distance_greatcircle`](https://swfsc.github.io/swfscDAS/reference/distance_greatcircle.md)
is used, while
[`distance`](https://rdrr.io/pkg/swfscMisc/man/distance.html) is used
otherwise. See
[`das_sight`](https://swfsc.github.io/swfscDAS/reference/das_sight.md)
for how the sightings are processed.

The sightinfo data frame includes the column 'included', which is used
in
[`das_effort_sight`](https://swfsc.github.io/swfscDAS/reference/das_effort_sight.md)
when summarizing the number of sightings and animals for selected
species.
[`das_effort_sight`](https://swfsc.github.io/swfscDAS/reference/das_effort_sight.md)
is a separate function to allow users to personalize the included values
as desired for their analysis. By default, i.e. in the output of this
function, 'included' is `TRUE` if: the sighting was made when on effort,
by a standard observer (see
[`das_sight`](https://swfsc.github.io/swfscDAS/reference/das_sight.md)),
and in a Beaufort sea state less than or equal to five.

## See also

Internal functions called by `das_effort`:
[`das_chop_condition`](https://swfsc.github.io/swfscDAS/reference/das_chop_condition.md),
[`das_chop_equallength`](https://swfsc.github.io/swfscDAS/reference/das_chop_equallength.md),
[`das_chop_section`](https://swfsc.github.io/swfscDAS/reference/das_chop_section.md),
[`das_segdata`](https://swfsc.github.io/swfscDAS/reference/das_segdata.md)

## Examples

``` r
y <- system.file("extdata", "das_sample.das", package = "swfscDAS")
y.proc <- das_process(y)

# Using "condition" method
das_effort(
  y.proc, method = "condition", conditions = c("Bft", "SwellHght", "Vis"),
  seg.min.km = 0.05, num.cores = 1
)
#> $segdata
#>       segnum section_id section_sub_id           file stlin endlin     lat1
#> lat        1          1              1 das_sample.das     2     20 39.32033
#> lat7       2          2              1 das_sample.das    23     33 39.37617
#> lat1       3          2              2 das_sample.das    33     43 39.42950
#> lat8       4          3              1 das_sample.das    59     69 39.56800
#> lat11      5          3              2 das_sample.das    69     70 39.66082
#> lat2       6          3              3 das_sample.das    70     75 39.66133
#> lat3       7          3              4 das_sample.das    75     78 39.67350
#> lat4       8          3              5 das_sample.das    78     84 39.69050
#> lat5       9          3              6 das_sample.das    84     85 39.73915
#> lat6      10          3              7 das_sample.das    85     90 39.75114
#> lat9      11          4              1 das_sample.das    99    108 39.94517
#> lat12     12          4              2 das_sample.das   108    121 39.96900
#> lat10     13          5              1 das_sample.das   127    147 40.15217
#> lat13     14          6              1 das_sample.das   150    160 40.26867
#> lat14     15          6              2 das_sample.das   160    164 40.32033
#> lat15     16          7              1 das_sample.das   167    174 40.38250
#> lat16     17          7              2 das_sample.das   174    181 40.42965
#> lat17     18          8              1 das_sample.das   188    199 40.52200
#> lat18     19          9              1 das_sample.das   232    240 40.98717
#> lat19     20         10              1 das_sample.das   242    259 41.02383
#>            lon1           DateTime1     lat2      lon2           DateTime2
#> lat   -137.6043 2013-01-13 06:27:39 39.36716 -137.5817 2013-01-13 06:46:25
#> lat7  -137.5978 2013-01-13 06:58:04 39.42950 -137.5715 2013-01-13 07:20:02
#> lat1  -137.5715 2013-01-13 07:20:02 39.51933 -137.5277 2013-01-13 07:57:05
#> lat8  -137.4530 2013-01-13 09:22:13 39.66082 -137.4132 2013-01-13 09:59:38
#> lat11 -137.4132 2013-01-13 09:59:38 39.66133 -137.4130 2013-01-13 09:59:50
#> lat2  -137.4130 2013-01-13 09:59:50 39.67350 -137.4080 2013-01-13 10:04:35
#> lat3  -137.4080 2013-01-13 10:04:35 39.69050 -137.4010 2013-01-13 10:11:09
#> lat4  -137.4010 2013-01-13 10:11:09 39.73915 -137.4031 2013-01-13 10:30:28
#> lat5  -137.4031 2013-01-13 10:30:28 39.75114 -137.4091 2013-01-13 10:35:14
#> lat6  -137.4091 2013-01-13 10:35:14 39.75433 -137.4107 2013-01-13 10:36:27
#> lat9  -137.3692 2013-01-13 11:51:51 39.96900 -137.3542 2013-01-13 12:02:29
#> lat12 -137.3542 2013-01-13 12:02:29 40.12745 -137.2488 2013-01-13 13:16:38
#> lat10 -137.1737 2013-01-13 13:50:07 40.26617 -137.1348 2013-01-13 14:38:13
#> lat13 -137.1268 2013-01-13 14:59:19 40.32033 -137.1108 2013-01-13 15:20:26
#> lat14 -137.1108 2013-01-13 15:20:26 40.37596 -137.0915 2013-01-13 15:43:08
#> lat15 -137.0977 2013-01-13 15:58:41 40.42965 -137.0745 2013-01-13 16:20:02
#> lat16 -137.0745 2013-01-13 16:20:02 40.45133 -137.0628 2013-01-13 16:29:50
#> lat17 -137.0533 2013-01-13 16:59:54 40.52533 -137.0515 2013-01-13 17:01:21
#> lat18 -135.5980 2013-01-14 11:24:32 41.01582 -135.5782 2013-01-14 11:37:24
#> lat19 -135.5743 2013-01-14 11:40:38 41.04599 -135.5595 2013-01-14 11:50:29
#>           mlat      mlon           mDateTime    dist year month day    mtime
#> lat   39.34377 -137.5930 2013-01-13 06:37:02  5.5577 2013     1  13 06:37:02
#> lat7  39.40288 -137.5848 2013-01-13 07:09:03  6.3431 2013     1  13 07:09:03
#> lat1  39.47435 -137.5493 2013-01-13 07:38:33 10.6674 2013     1  13 07:38:33
#> lat8  39.61433 -137.4327 2013-01-13 09:40:55 10.8651 2013     1  13 09:40:55
#> lat11 39.66108 -137.4131 2013-01-13 09:59:44  0.0574 2013     1  13 09:59:44
#> lat2  39.66744 -137.4106 2013-01-13 10:02:12  1.4189 2013     1  13 10:02:12
#> lat3  39.68200 -137.4045 2013-01-13 10:07:52  1.9817 2013     1  13 10:07:52
#> lat4  39.71506 -137.3921 2013-01-13 10:20:48  5.6822 2013     1  13 10:20:48
#> lat5  39.74517 -137.4062 2013-01-13 10:32:51  1.4286 2013     1  13 10:32:51
#> lat6  39.75274 -137.4100 2013-01-13 10:35:50  0.3747 2013     1  13 10:35:50
#> lat9  39.95710 -137.3617 2013-01-13 11:57:10  2.9404 2013     1  13 11:57:10
#> lat12 40.04864 -137.3024 2013-01-13 12:39:33 19.7686 2013     1  13 12:39:33
#> lat10 40.20895 -137.1531 2013-01-13 14:14:10 13.0922 2013     1  13 14:14:10
#> lat13 40.29448 -137.1187 2013-01-13 15:09:52  5.8993 2013     1  13 15:09:52
#> lat14 40.34833 -137.1019 2013-01-13 15:31:47  6.4018 2013     1  13 15:31:47
#> lat15 40.40618 -137.0864 2013-01-13 16:09:21  5.5964 2013     1  13 16:09:21
#> lat16 40.44053 -137.0687 2013-01-13 16:24:56  2.6020 2013     1  13 16:24:56
#> lat17 40.52365 -137.0524 2013-01-13 17:00:37  0.4016 2013     1  13 17:00:37
#> lat18 41.00151 -135.5881 2013-01-14 11:30:58  3.5940 2013     1  14 11:30:58
#> lat19 41.03500 -135.5671 2013-01-14 11:45:33  2.7600 2013     1  14 11:45:33
#>       Cruise Mode OffsetGMT EffType ESWsides maxdistBft maxdistSwellHght
#> lat     1000    C         5       S        2          3                3
#> lat7    1000    C         5       S        2          3                3
#> lat1    1000    C         5       S        2          3                3
#> lat8    1000    C         5       S        2          3                3
#> lat11   1000    C         5       S        2          3                3
#> lat2    1000    C         5       S        2          2                3
#> lat3    1000    C         5       S        2          2                3
#> lat4    1000    C         5       S        2          2                3
#> lat5    1000    C         5       S        2          2                3
#> lat6    1000    C         5       S        2          2                3
#> lat9    1000    C         5       S        2          3                3
#> lat12   1000    C         5       S        2          3                3
#> lat10   1000    C         5       S        2          3                3
#> lat13   1000    C         5       S        2          3                3
#> lat14   1000    C         5       S        2          2                3
#> lat15   1000    C         5       S        2          3                3
#> lat16   1000    C         5       S        2          2                3
#> lat17   1000    C         5       S        2          2                3
#> lat18   1000    C         5       S        2          2                1
#> lat19   1000    C         5       S        2          2                1
#>       maxdistVis
#> lat          6.0
#> lat7         6.0
#> lat1         5.5
#> lat8         5.5
#> lat11        6.0
#> lat2         6.0
#> lat3         5.5
#> lat4         4.5
#> lat5         3.5
#> lat6         2.5
#> lat9         5.8
#> lat12        6.0
#> lat10        6.0
#> lat13        6.0
#> lat14        6.0
#> lat15        6.0
#> lat16        6.0
#> lat17        6.0
#> lat18        4.0
#> lat19        4.0
#> 
#> $sightinfo
#>    segnum     mlat      mlon Event            DateTime year      Lat       Lon
#> 1       1 39.34377 -137.5930     S 2013-01-13 06:46:02 2013 39.36617 -137.5820
#> 2       3 39.47435 -137.5493     S 2013-01-13 07:56:22 2013 39.51767 -137.5285
#> 3       4 39.61433 -137.4327     t 2013-01-13 09:34:27 2013 39.59733 -137.4400
#> 4      13 40.20895 -137.1531     t 2013-01-13 14:02:55 2013 40.18283 -137.1622
#> 5      13 40.20895 -137.1531     S 2013-01-13 14:37:56 2013 40.26567 -137.1350
#> 6      15 40.34833 -137.1019     t 2013-01-13 15:36:47 2013 40.36050 -137.0978
#> 7      17 40.44053 -137.0687     S 2013-01-13 16:29:50 2013 40.45133 -137.0628
#> 8      18 40.52365 -137.0524     S 2013-01-13 17:00:45 2013 40.52400 -137.0522
#> 9      18 40.52365 -137.0524     S 2013-01-13 17:00:45 2013 40.52400 -137.0522
#> 10     19 41.00151 -135.5881     F 2013-01-14 11:25:32 2013 40.98950 -135.5965
#> 11     20 41.03500 -135.5671     S 2013-01-14 11:47:51 2013 41.04017 -135.5635
#> 12     20 41.03500 -135.5671     S 2013-01-14 11:47:51 2013 41.04017 -135.5635
#> 13     20 41.03500 -135.5671     S 2013-01-14 11:49:14 2013 41.04333 -135.5615
#> 14     20 41.03500 -135.5671     S 2013-01-14 11:49:14 2013 41.04333 -135.5615
#>    OnEffort Cruise Mode OffsetGMT EffType ESWsides Course SpdKt Bft SwellHght
#> 1      TRUE   1000    C         5       S        2     25  10.2   3         3
#> 2      TRUE   1000    C         5       S        2     26   9.7   3         3
#> 3      TRUE   1000    C         5       S        2     27   9.0   3         3
#> 4      TRUE   1000    C         5       S        2     20   9.3   3         3
#> 5      TRUE   1000    C         5       S        2     20   9.3   3         3
#> 6      TRUE   1000    C         5       S        2     16   8.9   2         3
#> 7      TRUE   1000    C         5       S        2     25   8.9   2         3
#> 8      TRUE   1000    C         5       S        2     30   9.5   2         3
#> 9      TRUE   1000    C         5       S        2     30   9.5   2         3
#> 10     TRUE   1000    C         5       S        2     35   9.5   2         1
#> 11     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 12     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 13     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 14     TRUE   1000    C         5       S        2     23   9.6   2         1
#>    WindSpdKt RainFog HorizSun VertSun Glare Vis ObsL Rec ObsR ObsInd EffortDot
#> 1         10       1       NA      NA    NA 6.0  208 280  001   <NA>      TRUE
#> 2         10       3        2       2 FALSE 5.5  125 208  280   <NA>      TRUE
#> 3         10       1        2       2 FALSE 5.5  001 126  149   <NA>      TRUE
#> 4          6       1        8       1 FALSE 6.0  280 001  126   <NA>      TRUE
#> 5          6       1        8       1 FALSE 6.0  280 001  126   <NA>      TRUE
#> 6          6       1        9       1 FALSE 6.0  125 208  280   <NA>      TRUE
#> 7          6       1        8       2 FALSE 6.0  149 125  208   <NA>      TRUE
#> 8          6       1        8       2 FALSE 6.0  126 149  125   <NA>      TRUE
#> 9          6       1        8       2 FALSE 6.0  126 149  125   <NA>      TRUE
#> 10         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 11         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 12         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 13         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 14         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#>    EventNum       file_das line_num SightNo Subgroup SightNoDaily Obs ObsStd
#> 1        15 das_sample.das       15    1406     <NA>   20130113_1 208   TRUE
#> 2        35 das_sample.das       38    1407     <NA>   20130113_2 125   TRUE
#> 3        59 das_sample.das       65    <NA>     <NA>         <NA> 280  FALSE
#> 4       131 das_sample.das      137    <NA>     <NA>         <NA> 228  FALSE
#> 5       136 das_sample.das      142    1408     <NA>   20130113_3 280   TRUE
#> 6       153 das_sample.das      162    <NA>     <NA>         <NA> 231  FALSE
#> 7       167 das_sample.das      176    1409     <NA>   20130113_4 149   TRUE
#> 8       181 das_sample.das      193    1410     <NA>   20130113_5 125   TRUE
#> 9       181 das_sample.das      193    1410     <NA>   20130113_5 125   TRUE
#> 10        8 das_sample.das      238    <NA>     <NA>         <NA> 149   TRUE
#> 11       18 das_sample.das      248    1412     <NA>   20130114_1 149   TRUE
#> 12       18 das_sample.das      248    1412     <NA>   20130114_1 149   TRUE
#> 13       20 das_sample.das      252    1413     <NA>   20130114_2 208   TRUE
#> 14       20 das_sample.das      252    1413     <NA>   20130114_2 208   TRUE
#>    Bearing Reticle DistNm Cue Method Photos Birds CalibSchool PhotosAerial
#> 1      309     2.8   1.06   3      4      N     N        <NA>         <NA>
#> 2      326     0.4   2.97   3      4      Y     N        <NA>         <NA>
#> 3      120      NA   0.03  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 4      300      NA   0.02  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 5      270    14.0   0.28   3      4      N     N        <NA>         <NA>
#> 6       45      NA   0.05  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 7      344     0.2   3.68   3      4      Y     Y        <NA>         <NA>
#> 8       70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 9       70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 10     309     1.7   1.47  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 11     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 12     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 13      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#> 14      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#>    Biopsy  Prob nSp Mixed SpCode SpCodeProb GsSchoolBest GsSchoolHigh
#> 1    <NA> FALSE   1 FALSE    018       <NA>           NA           NA
#> 2    <NA> FALSE   1 FALSE    076       <NA>      8.00000        14.00
#> 3    <NA>    NA  NA    NA     LV       <NA>      1.00000           NA
#> 4    <NA>    NA  NA    NA     DC       <NA>      1.00000           NA
#> 5    <NA> FALSE   1 FALSE    037       <NA>     10.66667        20.00
#> 6    <NA>    NA  NA    NA     DC       <NA>      1.00000           NA
#> 7    <NA> FALSE   1 FALSE    016       <NA>     46.66667        79.00
#> 8    <NA> FALSE   2  TRUE    013       <NA>     41.75000        72.75
#> 9    <NA> FALSE   2  TRUE    016       <NA>     41.75000        72.75
#> 10   <NA>    NA  NA    NA   <NA>       <NA>           NA           NA
#> 11   <NA> FALSE   2  TRUE    018       <NA>    151.50000       249.00
#> 12   <NA> FALSE   2  TRUE    277       <NA>    151.50000       249.00
#> 13   <NA>  TRUE   2  TRUE    016        016     21.25000        37.75
#> 14   <NA>  TRUE   2  TRUE    277        016     21.25000        37.75
#>    GsSchoolLow  GsSpBest GsSpHigh    GsSpLow CourseSchool TurtleJFR TurtleAge
#> 1    42.333333        NA       NA  42.333333           NA      <NA>      <NA>
#> 2     5.666667   8.00000  14.0000   5.666667           NA      <NA>      <NA>
#> 3           NA   1.00000       NA         NA           NA      <NA>         A
#> 4           NA   1.00000       NA         NA           NA      <NA>         J
#> 5    10.666667  10.66667  20.0000  10.666667           NA      <NA>      <NA>
#> 6           NA   1.00000       NA         NA           NA      <NA>         A
#> 7    46.666667  46.66667  79.0000  46.666667           NA      <NA>      <NA>
#> 8    41.750000  30.06000  52.3800  30.060000           NA      <NA>      <NA>
#> 9    41.750000  11.69000  20.3700  11.690000           NA      <NA>      <NA>
#> 10          NA        NA       NA         NA           NA      <NA>      <NA>
#> 11  151.500000 128.77500 211.6500 128.775000           NA      <NA>      <NA>
#> 12  151.500000  22.72500  37.3500  22.725000           NA      <NA>      <NA>
#> 13   21.250000  15.08750  26.8025  15.087500           NA      <NA>      <NA>
#> 14   21.250000   6.16250  10.9475   6.162500           NA      <NA>      <NA>
#>    TurtleCapt PerpDistKm included
#> 1        <NA> 1.52563078     TRUE
#> 2        <NA> 3.07580701     TRUE
#> 3           N 0.04811637    FALSE
#> 4           N 0.03207758    FALSE
#> 5        <NA> 0.51856000     TRUE
#> 6        <NA> 0.06547809    FALSE
#> 7        <NA> 1.87856781     TRUE
#> 8        <NA> 2.88891582     TRUE
#> 9        <NA> 2.88891582     TRUE
#> 10       <NA> 2.11573325     TRUE
#> 11       <NA> 0.10601569     TRUE
#> 12       <NA> 0.10601569     TRUE
#> 13       <NA> 2.54265727     TRUE
#> 14       <NA> 2.54265727     TRUE
#> 
#> $randpicks
#> NULL
#> 

# Using "section" method
das_effort(y.proc, method = "section", num.cores = 1)
#> $segdata
#>      segnum section_id section_sub_id           file stlin endlin     lat1
#> lat       1          1              1 das_sample.das     2     20 39.32033
#> lat1      2          2              1 das_sample.das    23     43 39.37617
#> lat2      3          3              1 das_sample.das    59     90 39.56800
#> lat3      4          4              1 das_sample.das    99    121 39.94517
#> lat4      5          5              1 das_sample.das   127    147 40.15217
#> lat5      6          6              1 das_sample.das   150    164 40.26867
#> lat6      7          7              1 das_sample.das   167    181 40.38250
#> lat7      8          8              1 das_sample.das   188    199 40.52200
#> lat8      9          9              1 das_sample.das   232    240 40.98717
#> lat9     10         10              1 das_sample.das   242    259 41.02383
#>           lon1           DateTime1     lat2      lon2           DateTime2
#> lat  -137.6043 2013-01-13 06:27:39 39.36716 -137.5817 2013-01-13 06:46:25
#> lat1 -137.5978 2013-01-13 06:58:04 39.51933 -137.5277 2013-01-13 07:57:05
#> lat2 -137.4530 2013-01-13 09:22:13 39.75433 -137.4107 2013-01-13 10:36:27
#> lat3 -137.3692 2013-01-13 11:51:51 40.12745 -137.2488 2013-01-13 13:16:38
#> lat4 -137.1737 2013-01-13 13:50:07 40.26617 -137.1348 2013-01-13 14:38:13
#> lat5 -137.1268 2013-01-13 14:59:19 40.37596 -137.0915 2013-01-13 15:43:08
#> lat6 -137.0977 2013-01-13 15:58:41 40.45133 -137.0628 2013-01-13 16:29:50
#> lat7 -137.0533 2013-01-13 16:59:54 40.52533 -137.0515 2013-01-13 17:01:21
#> lat8 -135.5980 2013-01-14 11:24:32 41.01582 -135.5782 2013-01-14 11:37:24
#> lat9 -135.5743 2013-01-14 11:40:38 41.04599 -135.5595 2013-01-14 11:50:29
#>          mlat      mlon           mDateTime    dist year month day    mtime
#> lat  39.34377 -137.5930 2013-01-13 06:37:02  5.5577 2013     1  13 06:37:02
#> lat1 39.44767 -137.5625 2013-01-13 07:27:34 17.0106 2013     1  13 07:27:34
#> lat2 39.66117 -137.4131 2013-01-13 09:59:20 21.8086 2013     1  13 09:59:20
#> lat3 40.03679 -137.3101 2013-01-13 12:34:14 22.7090 2013     1  13 12:34:14
#> lat4 40.20895 -137.1531 2013-01-13 14:14:10 13.0922 2013     1  13 14:14:10
#> lat5 40.32254 -137.1102 2013-01-13 15:21:13 12.3011 2013     1  13 15:21:13
#> lat6 40.41714 -137.0810 2013-01-13 16:14:15  8.1984 2013     1  13 16:14:15
#> lat7 40.52365 -137.0524 2013-01-13 17:00:37  0.4016 2013     1  13 17:00:37
#> lat8 41.00151 -135.5881 2013-01-14 11:30:58  3.5940 2013     1  14 11:30:58
#> lat9 41.03500 -135.5671 2013-01-14 11:45:33  2.7600 2013     1  14 11:45:33
#>      Cruise Mode OffsetGMT EffType ESWsides   avgBft avgSwellHght avgHorizSun
#> lat    1000    C         5       S        2 3.000000            3    2.000000
#> lat1   1000    C         5       S        2 3.000000            3    2.000000
#> lat2   1000    C         5       S        2 2.500835            3    2.000000
#> lat3   1000    C         5       S        2 3.000000            3   12.000000
#> lat4   1000    C         5       S        2 3.000000            3    8.000000
#> lat5   1000    C         5       S        2 2.479576            3    8.520424
#> lat6   1000    C         5       S        2 2.682617            3    8.000000
#> lat7   1000    C         5       S        2 2.000000            3    8.000000
#> lat8   1000    C         5       S        2 2.000000            1          NA
#> lat9   1000    C         5       S        2 2.000000            1          NA
#>      avgVertSun avgGlare   avgVis avgCourse avgSpdKt
#> lat           3        0 6.000000  23.56198 9.912395
#> lat1          2        0 5.686447  27.11868 9.476263
#> lat2          2        0 5.090748  95.65614 9.287725
#> lat3         12        0 5.974103  34.61155 9.428340
#> lat4          1        0 6.000000  20.43919 9.343919
#> lat5          1        0 6.000000  16.47958 9.091830
#> lat6          2        0 6.000000  25.00000 8.900000
#> lat7          2        0 6.000000  30.00000 9.500000
#> lat8         NA       NA 4.000000  35.00000 9.500000
#> lat9         NA       NA 4.000000  23.00000 9.600000
#> 
#> $sightinfo
#>    segnum     mlat      mlon Event            DateTime year      Lat       Lon
#> 1       1 39.34377 -137.5930     S 2013-01-13 06:46:02 2013 39.36617 -137.5820
#> 2       2 39.44767 -137.5625     S 2013-01-13 07:56:22 2013 39.51767 -137.5285
#> 3       3 39.66117 -137.4131     t 2013-01-13 09:34:27 2013 39.59733 -137.4400
#> 4       5 40.20895 -137.1531     t 2013-01-13 14:02:55 2013 40.18283 -137.1622
#> 5       5 40.20895 -137.1531     S 2013-01-13 14:37:56 2013 40.26567 -137.1350
#> 6       6 40.32254 -137.1102     t 2013-01-13 15:36:47 2013 40.36050 -137.0978
#> 7       7 40.41714 -137.0810     S 2013-01-13 16:29:50 2013 40.45133 -137.0628
#> 8       8 40.52365 -137.0524     S 2013-01-13 17:00:45 2013 40.52400 -137.0522
#> 9       8 40.52365 -137.0524     S 2013-01-13 17:00:45 2013 40.52400 -137.0522
#> 10      9 41.00151 -135.5881     F 2013-01-14 11:25:32 2013 40.98950 -135.5965
#> 11     10 41.03500 -135.5671     S 2013-01-14 11:47:51 2013 41.04017 -135.5635
#> 12     10 41.03500 -135.5671     S 2013-01-14 11:47:51 2013 41.04017 -135.5635
#> 13     10 41.03500 -135.5671     S 2013-01-14 11:49:14 2013 41.04333 -135.5615
#> 14     10 41.03500 -135.5671     S 2013-01-14 11:49:14 2013 41.04333 -135.5615
#>    OnEffort Cruise Mode OffsetGMT EffType ESWsides Course SpdKt Bft SwellHght
#> 1      TRUE   1000    C         5       S        2     25  10.2   3         3
#> 2      TRUE   1000    C         5       S        2     26   9.7   3         3
#> 3      TRUE   1000    C         5       S        2     27   9.0   3         3
#> 4      TRUE   1000    C         5       S        2     20   9.3   3         3
#> 5      TRUE   1000    C         5       S        2     20   9.3   3         3
#> 6      TRUE   1000    C         5       S        2     16   8.9   2         3
#> 7      TRUE   1000    C         5       S        2     25   8.9   2         3
#> 8      TRUE   1000    C         5       S        2     30   9.5   2         3
#> 9      TRUE   1000    C         5       S        2     30   9.5   2         3
#> 10     TRUE   1000    C         5       S        2     35   9.5   2         1
#> 11     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 12     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 13     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 14     TRUE   1000    C         5       S        2     23   9.6   2         1
#>    WindSpdKt RainFog HorizSun VertSun Glare Vis ObsL Rec ObsR ObsInd EffortDot
#> 1         10       1       NA      NA    NA 6.0  208 280  001   <NA>      TRUE
#> 2         10       3        2       2 FALSE 5.5  125 208  280   <NA>      TRUE
#> 3         10       1        2       2 FALSE 5.5  001 126  149   <NA>      TRUE
#> 4          6       1        8       1 FALSE 6.0  280 001  126   <NA>      TRUE
#> 5          6       1        8       1 FALSE 6.0  280 001  126   <NA>      TRUE
#> 6          6       1        9       1 FALSE 6.0  125 208  280   <NA>      TRUE
#> 7          6       1        8       2 FALSE 6.0  149 125  208   <NA>      TRUE
#> 8          6       1        8       2 FALSE 6.0  126 149  125   <NA>      TRUE
#> 9          6       1        8       2 FALSE 6.0  126 149  125   <NA>      TRUE
#> 10         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 11         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 12         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 13         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 14         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#>    EventNum       file_das line_num SightNo Subgroup SightNoDaily Obs ObsStd
#> 1        15 das_sample.das       15    1406     <NA>   20130113_1 208   TRUE
#> 2        35 das_sample.das       38    1407     <NA>   20130113_2 125   TRUE
#> 3        59 das_sample.das       65    <NA>     <NA>         <NA> 280  FALSE
#> 4       131 das_sample.das      137    <NA>     <NA>         <NA> 228  FALSE
#> 5       136 das_sample.das      142    1408     <NA>   20130113_3 280   TRUE
#> 6       153 das_sample.das      162    <NA>     <NA>         <NA> 231  FALSE
#> 7       167 das_sample.das      176    1409     <NA>   20130113_4 149   TRUE
#> 8       181 das_sample.das      193    1410     <NA>   20130113_5 125   TRUE
#> 9       181 das_sample.das      193    1410     <NA>   20130113_5 125   TRUE
#> 10        8 das_sample.das      238    <NA>     <NA>         <NA> 149   TRUE
#> 11       18 das_sample.das      248    1412     <NA>   20130114_1 149   TRUE
#> 12       18 das_sample.das      248    1412     <NA>   20130114_1 149   TRUE
#> 13       20 das_sample.das      252    1413     <NA>   20130114_2 208   TRUE
#> 14       20 das_sample.das      252    1413     <NA>   20130114_2 208   TRUE
#>    Bearing Reticle DistNm Cue Method Photos Birds CalibSchool PhotosAerial
#> 1      309     2.8   1.06   3      4      N     N        <NA>         <NA>
#> 2      326     0.4   2.97   3      4      Y     N        <NA>         <NA>
#> 3      120      NA   0.03  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 4      300      NA   0.02  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 5      270    14.0   0.28   3      4      N     N        <NA>         <NA>
#> 6       45      NA   0.05  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 7      344     0.2   3.68   3      4      Y     Y        <NA>         <NA>
#> 8       70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 9       70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 10     309     1.7   1.47  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 11     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 12     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 13      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#> 14      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#>    Biopsy  Prob nSp Mixed SpCode SpCodeProb GsSchoolBest GsSchoolHigh
#> 1    <NA> FALSE   1 FALSE    018       <NA>           NA           NA
#> 2    <NA> FALSE   1 FALSE    076       <NA>      8.00000        14.00
#> 3    <NA>    NA  NA    NA     LV       <NA>      1.00000           NA
#> 4    <NA>    NA  NA    NA     DC       <NA>      1.00000           NA
#> 5    <NA> FALSE   1 FALSE    037       <NA>     10.66667        20.00
#> 6    <NA>    NA  NA    NA     DC       <NA>      1.00000           NA
#> 7    <NA> FALSE   1 FALSE    016       <NA>     46.66667        79.00
#> 8    <NA> FALSE   2  TRUE    013       <NA>     41.75000        72.75
#> 9    <NA> FALSE   2  TRUE    016       <NA>     41.75000        72.75
#> 10   <NA>    NA  NA    NA   <NA>       <NA>           NA           NA
#> 11   <NA> FALSE   2  TRUE    018       <NA>    151.50000       249.00
#> 12   <NA> FALSE   2  TRUE    277       <NA>    151.50000       249.00
#> 13   <NA>  TRUE   2  TRUE    016        016     21.25000        37.75
#> 14   <NA>  TRUE   2  TRUE    277        016     21.25000        37.75
#>    GsSchoolLow  GsSpBest GsSpHigh    GsSpLow CourseSchool TurtleJFR TurtleAge
#> 1    42.333333        NA       NA  42.333333           NA      <NA>      <NA>
#> 2     5.666667   8.00000  14.0000   5.666667           NA      <NA>      <NA>
#> 3           NA   1.00000       NA         NA           NA      <NA>         A
#> 4           NA   1.00000       NA         NA           NA      <NA>         J
#> 5    10.666667  10.66667  20.0000  10.666667           NA      <NA>      <NA>
#> 6           NA   1.00000       NA         NA           NA      <NA>         A
#> 7    46.666667  46.66667  79.0000  46.666667           NA      <NA>      <NA>
#> 8    41.750000  30.06000  52.3800  30.060000           NA      <NA>      <NA>
#> 9    41.750000  11.69000  20.3700  11.690000           NA      <NA>      <NA>
#> 10          NA        NA       NA         NA           NA      <NA>      <NA>
#> 11  151.500000 128.77500 211.6500 128.775000           NA      <NA>      <NA>
#> 12  151.500000  22.72500  37.3500  22.725000           NA      <NA>      <NA>
#> 13   21.250000  15.08750  26.8025  15.087500           NA      <NA>      <NA>
#> 14   21.250000   6.16250  10.9475   6.162500           NA      <NA>      <NA>
#>    TurtleCapt PerpDistKm included
#> 1        <NA> 1.52563078     TRUE
#> 2        <NA> 3.07580701     TRUE
#> 3           N 0.04811637    FALSE
#> 4           N 0.03207758    FALSE
#> 5        <NA> 0.51856000     TRUE
#> 6        <NA> 0.06547809    FALSE
#> 7        <NA> 1.87856781     TRUE
#> 8        <NA> 2.88891582     TRUE
#> 9        <NA> 2.88891582     TRUE
#> 10       <NA> 2.11573325     TRUE
#> 11       <NA> 0.10601569     TRUE
#> 12       <NA> 0.10601569     TRUE
#> 13       <NA> 2.54265727     TRUE
#> 14       <NA> 2.54265727     TRUE
#> 
#> $randpicks
#>    effort_section randpicks
#> 1               1        NA
#> 2               2        NA
#> 3               3        NA
#> 4               4        NA
#> 5               5        NA
#> 6               6        NA
#> 7               7        NA
#> 8               8        NA
#> 9               9        NA
#> 10             10        NA
#> 

# \donttest{
# Using "equallength" method
y.rand <- system.file("extdata", "das_sample_randpicks.csv", package = "swfscDAS")
das_effort(
  y.proc, method = "equallength", seg.km = 10, randpicks.load = y.rand,
  num.cores = 1
)
#> $segdata
#>       segnum section_id section_sub_id           file stlin endlin     lat1
#> lat        1          1              1 das_sample.das     2     20 39.32033
#> lat2       2          2              1 das_sample.das    23     34 39.37617
#> lat1       3          2              2 das_sample.das    34     43 39.43510
#> lat3       4          3              1 das_sample.das    59     68 39.56800
#> lat11      5          3              2 das_sample.das    68     90 39.65336
#> lat4       6          4              1 das_sample.das    99    113 39.94517
#> lat12      7          4              2 das_sample.das   113    121 40.02582
#> lat5       8          5              1 das_sample.das   127    147 40.15217
#> lat6       9          6              1 das_sample.das   150    164 40.26867
#> lat7      10          7              1 das_sample.das   167    181 40.38250
#> lat8      11          8              1 das_sample.das   188    199 40.52200
#> lat9      12          9              1 das_sample.das   232    240 40.98717
#> lat10     13         10              1 das_sample.das   242    259 41.02383
#>            lon1           DateTime1     lat2      lon2           DateTime2
#> lat   -137.6043 2013-01-13 06:27:39 39.36716 -137.5817 2013-01-13 06:46:25
#> lat2  -137.5978 2013-01-13 06:58:04 39.43510 -137.5687 2013-01-13 07:22:21
#> lat1  -137.5687 2013-01-13 07:22:21 39.51933 -137.5277 2013-01-13 07:57:05
#> lat3  -137.4530 2013-01-13 09:22:13 39.65336 -137.4163 2013-01-13 09:56:43
#> lat11 -137.4163 2013-01-13 09:56:43 39.75433 -137.4107 2013-01-13 10:36:27
#> lat4  -137.3692 2013-01-13 11:51:51 40.02582 -137.3171 2013-01-13 12:29:17
#> lat12 -137.3171 2013-01-13 12:29:17 40.12745 -137.2488 2013-01-13 13:16:38
#> lat5  -137.1737 2013-01-13 13:50:07 40.26617 -137.1348 2013-01-13 14:38:13
#> lat6  -137.1268 2013-01-13 14:59:19 40.37596 -137.0915 2013-01-13 15:43:08
#> lat7  -137.0977 2013-01-13 15:58:41 40.45133 -137.0628 2013-01-13 16:29:50
#> lat8  -137.0533 2013-01-13 16:59:54 40.52533 -137.0515 2013-01-13 17:01:21
#> lat9  -135.5980 2013-01-14 11:24:32 41.01582 -135.5782 2013-01-14 11:37:24
#> lat10 -135.5743 2013-01-14 11:40:38 41.04599 -135.5595 2013-01-14 11:50:29
#>           mlat      mlon           mDateTime    dist year month day    mtime
#> lat   39.34377 -137.5930 2013-01-13 06:37:02  5.5577 2013     1  13 06:37:02
#> lat2  39.40568 -137.5834 2013-01-13 07:10:12  7.0106 2013     1  13 07:10:12
#> lat1  39.47716 -137.5480 2013-01-13 07:39:43 10.0000 2013     1  13 07:39:43
#> lat3  39.61063 -137.4343 2013-01-13 09:39:28 10.0000 2013     1  13 09:39:28
#> lat11 39.70406 -137.3954 2013-01-13 10:16:35 11.8086 2013     1  13 10:16:35
#> lat4  39.98555 -137.3433 2013-01-13 12:10:34 10.0000 2013     1  13 12:10:34
#> lat12 40.07696 -137.2836 2013-01-13 12:52:57 12.7090 2013     1  13 12:52:57
#> lat5  40.20895 -137.1531 2013-01-13 14:14:10 13.0922 2013     1  13 14:14:10
#> lat6  40.32254 -137.1102 2013-01-13 15:21:13 12.3011 2013     1  13 15:21:13
#> lat7  40.41714 -137.0810 2013-01-13 16:14:15  8.1984 2013     1  13 16:14:15
#> lat8  40.52365 -137.0524 2013-01-13 17:00:37  0.4016 2013     1  13 17:00:37
#> lat9  41.00151 -135.5881 2013-01-14 11:30:58  3.5940 2013     1  14 11:30:58
#> lat10 41.03500 -135.5671 2013-01-14 11:45:33  2.7600 2013     1  14 11:45:33
#>       Cruise Mode OffsetGMT EffType ESWsides   avgBft avgSwellHght avgHorizSun
#> lat     1000    C         5       S        2 3.000000            3    2.000000
#> lat2    1000    C         5       S        2 3.000000            3          NA
#> lat1    1000    C         5       S        2 3.000000            3    2.000000
#> lat3    1000    C         5       S        2 3.000000            3    2.000000
#> lat11   1000    C         5       S        2 2.078121            3    2.000000
#> lat4    1000    C         5       S        2 3.000000            3   12.000000
#> lat12   1000    C         5       S        2 3.000000            3   12.000000
#> lat5    1000    C         5       S        2 3.000000            3    8.000000
#> lat6    1000    C         5       S        2 2.479576            3    8.520424
#> lat7    1000    C         5       S        2 2.682617            3    8.000000
#> lat8    1000    C         5       S        2 2.000000            3    8.000000
#> lat9    1000    C         5       S        2 2.000000            1          NA
#> lat10   1000    C         5       S        2 2.000000            1          NA
#>       avgVertSun avgGlare   avgVis avgCourse avgSpdKt
#> lat            3        0 6.000000  23.56198 9.912395
#> lat2          NA       NA 5.952398  28.71439 9.157122
#> lat1           2        0 5.500000  26.00000 9.700000
#> lat3           2        0 5.500000  27.00000 9.000000
#> lat11          2        0 4.744176 153.79707 9.531382
#> lat4          12        0 5.941191  34.11787 9.570596
#> lat12         12        0 6.000000  35.00000 9.316406
#> lat5           1        0 6.000000  20.43919 9.343919
#> lat6           1        0 6.000000  16.47958 9.091830
#> lat7           2        0 6.000000  25.00000 8.900000
#> lat8           2        0 6.000000  30.00000 9.500000
#> lat9          NA       NA 4.000000  35.00000 9.500000
#> lat10         NA       NA 4.000000  23.00000 9.600000
#> 
#> $sightinfo
#>    segnum     mlat      mlon Event            DateTime year      Lat       Lon
#> 1       1 39.34377 -137.5930     S 2013-01-13 06:46:02 2013 39.36617 -137.5820
#> 2       3 39.47716 -137.5480     S 2013-01-13 07:56:22 2013 39.51767 -137.5285
#> 3       4 39.61063 -137.4343     t 2013-01-13 09:34:27 2013 39.59733 -137.4400
#> 4       8 40.20895 -137.1531     t 2013-01-13 14:02:55 2013 40.18283 -137.1622
#> 5       8 40.20895 -137.1531     S 2013-01-13 14:37:56 2013 40.26567 -137.1350
#> 6       9 40.32254 -137.1102     t 2013-01-13 15:36:47 2013 40.36050 -137.0978
#> 7      10 40.41714 -137.0810     S 2013-01-13 16:29:50 2013 40.45133 -137.0628
#> 8      11 40.52365 -137.0524     S 2013-01-13 17:00:45 2013 40.52400 -137.0522
#> 9      11 40.52365 -137.0524     S 2013-01-13 17:00:45 2013 40.52400 -137.0522
#> 10     12 41.00151 -135.5881     F 2013-01-14 11:25:32 2013 40.98950 -135.5965
#> 11     13 41.03500 -135.5671     S 2013-01-14 11:47:51 2013 41.04017 -135.5635
#> 12     13 41.03500 -135.5671     S 2013-01-14 11:47:51 2013 41.04017 -135.5635
#> 13     13 41.03500 -135.5671     S 2013-01-14 11:49:14 2013 41.04333 -135.5615
#> 14     13 41.03500 -135.5671     S 2013-01-14 11:49:14 2013 41.04333 -135.5615
#>    OnEffort Cruise Mode OffsetGMT EffType ESWsides Course SpdKt Bft SwellHght
#> 1      TRUE   1000    C         5       S        2     25  10.2   3         3
#> 2      TRUE   1000    C         5       S        2     26   9.7   3         3
#> 3      TRUE   1000    C         5       S        2     27   9.0   3         3
#> 4      TRUE   1000    C         5       S        2     20   9.3   3         3
#> 5      TRUE   1000    C         5       S        2     20   9.3   3         3
#> 6      TRUE   1000    C         5       S        2     16   8.9   2         3
#> 7      TRUE   1000    C         5       S        2     25   8.9   2         3
#> 8      TRUE   1000    C         5       S        2     30   9.5   2         3
#> 9      TRUE   1000    C         5       S        2     30   9.5   2         3
#> 10     TRUE   1000    C         5       S        2     35   9.5   2         1
#> 11     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 12     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 13     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 14     TRUE   1000    C         5       S        2     23   9.6   2         1
#>    WindSpdKt RainFog HorizSun VertSun Glare Vis ObsL Rec ObsR ObsInd EffortDot
#> 1         10       1       NA      NA    NA 6.0  208 280  001   <NA>      TRUE
#> 2         10       3        2       2 FALSE 5.5  125 208  280   <NA>      TRUE
#> 3         10       1        2       2 FALSE 5.5  001 126  149   <NA>      TRUE
#> 4          6       1        8       1 FALSE 6.0  280 001  126   <NA>      TRUE
#> 5          6       1        8       1 FALSE 6.0  280 001  126   <NA>      TRUE
#> 6          6       1        9       1 FALSE 6.0  125 208  280   <NA>      TRUE
#> 7          6       1        8       2 FALSE 6.0  149 125  208   <NA>      TRUE
#> 8          6       1        8       2 FALSE 6.0  126 149  125   <NA>      TRUE
#> 9          6       1        8       2 FALSE 6.0  126 149  125   <NA>      TRUE
#> 10         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 11         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 12         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 13         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 14         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#>    EventNum       file_das line_num SightNo Subgroup SightNoDaily Obs ObsStd
#> 1        15 das_sample.das       15    1406     <NA>   20130113_1 208   TRUE
#> 2        35 das_sample.das       38    1407     <NA>   20130113_2 125   TRUE
#> 3        59 das_sample.das       65    <NA>     <NA>         <NA> 280  FALSE
#> 4       131 das_sample.das      137    <NA>     <NA>         <NA> 228  FALSE
#> 5       136 das_sample.das      142    1408     <NA>   20130113_3 280   TRUE
#> 6       153 das_sample.das      162    <NA>     <NA>         <NA> 231  FALSE
#> 7       167 das_sample.das      176    1409     <NA>   20130113_4 149   TRUE
#> 8       181 das_sample.das      193    1410     <NA>   20130113_5 125   TRUE
#> 9       181 das_sample.das      193    1410     <NA>   20130113_5 125   TRUE
#> 10        8 das_sample.das      238    <NA>     <NA>         <NA> 149   TRUE
#> 11       18 das_sample.das      248    1412     <NA>   20130114_1 149   TRUE
#> 12       18 das_sample.das      248    1412     <NA>   20130114_1 149   TRUE
#> 13       20 das_sample.das      252    1413     <NA>   20130114_2 208   TRUE
#> 14       20 das_sample.das      252    1413     <NA>   20130114_2 208   TRUE
#>    Bearing Reticle DistNm Cue Method Photos Birds CalibSchool PhotosAerial
#> 1      309     2.8   1.06   3      4      N     N        <NA>         <NA>
#> 2      326     0.4   2.97   3      4      Y     N        <NA>         <NA>
#> 3      120      NA   0.03  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 4      300      NA   0.02  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 5      270    14.0   0.28   3      4      N     N        <NA>         <NA>
#> 6       45      NA   0.05  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 7      344     0.2   3.68   3      4      Y     Y        <NA>         <NA>
#> 8       70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 9       70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 10     309     1.7   1.47  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 11     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 12     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 13      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#> 14      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#>    Biopsy  Prob nSp Mixed SpCode SpCodeProb GsSchoolBest GsSchoolHigh
#> 1    <NA> FALSE   1 FALSE    018       <NA>           NA           NA
#> 2    <NA> FALSE   1 FALSE    076       <NA>      8.00000        14.00
#> 3    <NA>    NA  NA    NA     LV       <NA>      1.00000           NA
#> 4    <NA>    NA  NA    NA     DC       <NA>      1.00000           NA
#> 5    <NA> FALSE   1 FALSE    037       <NA>     10.66667        20.00
#> 6    <NA>    NA  NA    NA     DC       <NA>      1.00000           NA
#> 7    <NA> FALSE   1 FALSE    016       <NA>     46.66667        79.00
#> 8    <NA> FALSE   2  TRUE    013       <NA>     41.75000        72.75
#> 9    <NA> FALSE   2  TRUE    016       <NA>     41.75000        72.75
#> 10   <NA>    NA  NA    NA   <NA>       <NA>           NA           NA
#> 11   <NA> FALSE   2  TRUE    018       <NA>    151.50000       249.00
#> 12   <NA> FALSE   2  TRUE    277       <NA>    151.50000       249.00
#> 13   <NA>  TRUE   2  TRUE    016        016     21.25000        37.75
#> 14   <NA>  TRUE   2  TRUE    277        016     21.25000        37.75
#>    GsSchoolLow  GsSpBest GsSpHigh    GsSpLow CourseSchool TurtleJFR TurtleAge
#> 1    42.333333        NA       NA  42.333333           NA      <NA>      <NA>
#> 2     5.666667   8.00000  14.0000   5.666667           NA      <NA>      <NA>
#> 3           NA   1.00000       NA         NA           NA      <NA>         A
#> 4           NA   1.00000       NA         NA           NA      <NA>         J
#> 5    10.666667  10.66667  20.0000  10.666667           NA      <NA>      <NA>
#> 6           NA   1.00000       NA         NA           NA      <NA>         A
#> 7    46.666667  46.66667  79.0000  46.666667           NA      <NA>      <NA>
#> 8    41.750000  30.06000  52.3800  30.060000           NA      <NA>      <NA>
#> 9    41.750000  11.69000  20.3700  11.690000           NA      <NA>      <NA>
#> 10          NA        NA       NA         NA           NA      <NA>      <NA>
#> 11  151.500000 128.77500 211.6500 128.775000           NA      <NA>      <NA>
#> 12  151.500000  22.72500  37.3500  22.725000           NA      <NA>      <NA>
#> 13   21.250000  15.08750  26.8025  15.087500           NA      <NA>      <NA>
#> 14   21.250000   6.16250  10.9475   6.162500           NA      <NA>      <NA>
#>    TurtleCapt PerpDistKm included
#> 1        <NA> 1.52563078     TRUE
#> 2        <NA> 3.07580701     TRUE
#> 3           N 0.04811637    FALSE
#> 4           N 0.03207758    FALSE
#> 5        <NA> 0.51856000     TRUE
#> 6        <NA> 0.06547809    FALSE
#> 7        <NA> 1.87856781     TRUE
#> 8        <NA> 2.88891582     TRUE
#> 9        <NA> 2.88891582     TRUE
#> 10       <NA> 2.11573325     TRUE
#> 11       <NA> 0.10601569     TRUE
#> 12       <NA> 0.10601569     TRUE
#> 13       <NA> 2.54265727     TRUE
#> 14       <NA> 2.54265727     TRUE
#> 
#> $randpicks
#>    effort_section randpicks
#> 1               1        NA
#> 2               2         1
#> 3               3         2
#> 4               4         2
#> 5               5         1
#> 6               6         1
#> 7               7        NA
#> 8               8        NA
#> 9               9        NA
#> 10             10        NA
#> 

# Using "section" method and chop by strata
stratum.file <- system.file("extdata", "das_sample_stratum.csv", package = "swfscDAS")
das_effort(
  y.proc, method = "section", strata.files = list(Poly1 = stratum.file),
  num.cores = 1
)
#> although coordinates are longitude/latitude, st_intersection assumes that they
#> are planar
#> $segdata
#>    segnum section_id section_sub_id           file stlin endlin     lat1
#> 1       1          1              1 das_sample.das     2     20 39.32033
#> 2       2          2              1 das_sample.das    23     43 39.37617
#> 3       3          3              1 das_sample.das    59     90 39.56800
#> 4       4          4              1 das_sample.das    99    121 39.94517
#> 5       5          5              1 das_sample.das   127    147 40.15217
#> 6       6          6              1 das_sample.das   150    164 40.26867
#> 7       7          7              1 das_sample.das   167    181 40.38250
#> 8       8          8              1 das_sample.das   188    199 40.52200
#> 9       9          9              1 das_sample.das   232    240 40.98717
#> 10     10         10              1 das_sample.das   242    259 41.02383
#>         lon1           DateTime1     lat2      lon2           DateTime2
#> 1  -137.6043 2013-01-13 06:27:39 39.36716 -137.5817 2013-01-13 06:46:25
#> 2  -137.5978 2013-01-13 06:58:04 39.51933 -137.5277 2013-01-13 07:57:05
#> 3  -137.4530 2013-01-13 09:22:13 39.75433 -137.4107 2013-01-13 10:36:27
#> 4  -137.3692 2013-01-13 11:51:51 40.12745 -137.2488 2013-01-13 13:16:38
#> 5  -137.1737 2013-01-13 13:50:07 40.26617 -137.1348 2013-01-13 14:38:13
#> 6  -137.1268 2013-01-13 14:59:19 40.37596 -137.0915 2013-01-13 15:43:08
#> 7  -137.0977 2013-01-13 15:58:41 40.45133 -137.0628 2013-01-13 16:29:50
#> 8  -137.0533 2013-01-13 16:59:54 40.52533 -137.0515 2013-01-13 17:01:21
#> 9  -135.5980 2013-01-14 11:24:32 41.01582 -135.5782 2013-01-14 11:37:24
#> 10 -135.5743 2013-01-14 11:40:38 41.04599 -135.5595 2013-01-14 11:50:29
#>        mlat      mlon           mDateTime    dist year month day    mtime
#> 1  39.34377 -137.5930 2013-01-13 06:37:02  5.5577 2013     1  13 06:37:02
#> 2  39.44767 -137.5625 2013-01-13 07:27:34 17.0106 2013     1  13 07:27:34
#> 3  39.66117 -137.4131 2013-01-13 09:59:20 21.8086 2013     1  13 09:59:20
#> 4  40.03679 -137.3101 2013-01-13 12:34:14 22.7090 2013     1  13 12:34:14
#> 5  40.20895 -137.1531 2013-01-13 14:14:10 13.0922 2013     1  13 14:14:10
#> 6  40.32254 -137.1102 2013-01-13 15:21:13 12.3011 2013     1  13 15:21:13
#> 7  40.41714 -137.0810 2013-01-13 16:14:15  8.1984 2013     1  13 16:14:15
#> 8  40.52365 -137.0524 2013-01-13 17:00:37  0.4016 2013     1  13 17:00:37
#> 9  41.00151 -135.5881 2013-01-14 11:30:58  3.5940 2013     1  14 11:30:58
#> 10 41.03500 -135.5671 2013-01-14 11:45:33  2.7600 2013     1  14 11:45:33
#>    Cruise Mode OffsetGMT EffType ESWsides   avgBft avgSwellHght avgHorizSun
#> 1    1000    C         5       S        2 3.000000            3    2.000000
#> 2    1000    C         5       S        2 3.000000            3    2.000000
#> 3    1000    C         5       S        2 2.500835            3    2.000000
#> 4    1000    C         5       S        2 3.000000            3   12.000000
#> 5    1000    C         5       S        2 3.000000            3    8.000000
#> 6    1000    C         5       S        2 2.479576            3    8.520424
#> 7    1000    C         5       S        2 2.682617            3    8.000000
#> 8    1000    C         5       S        2 2.000000            3    8.000000
#> 9    1000    C         5       S        2 2.000000            1          NA
#> 10   1000    C         5       S        2 2.000000            1          NA
#>    avgVertSun avgGlare   avgVis avgCourse avgSpdKt  stratum
#> 1           3        0 6.000000  23.56198 9.912395     <NA>
#> 2           2        0 5.686447  27.11868 9.476263     <NA>
#> 3           2        0 5.090748  95.65614 9.287725     <NA>
#> 4          12        0 5.974103  34.61155 9.428340     <NA>
#> 5           1        0 6.000000  20.43919 9.343919 Stratum1
#> 6           1        0 6.000000  16.47958 9.091830 Stratum1
#> 7           2        0 6.000000  25.00000 8.900000 Stratum1
#> 8           2        0 6.000000  30.00000 9.500000 Stratum1
#> 9          NA       NA 4.000000  35.00000 9.500000 Stratum1
#> 10         NA       NA 4.000000  23.00000 9.600000 Stratum1
#> 
#> $sightinfo
#>    segnum     mlat      mlon Event            DateTime year      Lat       Lon
#> 1       1 39.34377 -137.5930     S 2013-01-13 06:46:02 2013 39.36617 -137.5820
#> 2       2 39.44767 -137.5625     S 2013-01-13 07:56:22 2013 39.51767 -137.5285
#> 3       3 39.66117 -137.4131     t 2013-01-13 09:34:27 2013 39.59733 -137.4400
#> 4       5 40.20895 -137.1531     t 2013-01-13 14:02:55 2013 40.18283 -137.1622
#> 5       5 40.20895 -137.1531     S 2013-01-13 14:37:56 2013 40.26567 -137.1350
#> 6       6 40.32254 -137.1102     t 2013-01-13 15:36:47 2013 40.36050 -137.0978
#> 7       7 40.41714 -137.0810     S 2013-01-13 16:29:50 2013 40.45133 -137.0628
#> 8       8 40.52365 -137.0524     S 2013-01-13 17:00:45 2013 40.52400 -137.0522
#> 9       8 40.52365 -137.0524     S 2013-01-13 17:00:45 2013 40.52400 -137.0522
#> 10      9 41.00151 -135.5881     F 2013-01-14 11:25:32 2013 40.98950 -135.5965
#> 11     10 41.03500 -135.5671     S 2013-01-14 11:47:51 2013 41.04017 -135.5635
#> 12     10 41.03500 -135.5671     S 2013-01-14 11:47:51 2013 41.04017 -135.5635
#> 13     10 41.03500 -135.5671     S 2013-01-14 11:49:14 2013 41.04333 -135.5615
#> 14     10 41.03500 -135.5671     S 2013-01-14 11:49:14 2013 41.04333 -135.5615
#>    OnEffort Cruise Mode OffsetGMT EffType ESWsides Course SpdKt Bft SwellHght
#> 1      TRUE   1000    C         5       S        2     25  10.2   3         3
#> 2      TRUE   1000    C         5       S        2     26   9.7   3         3
#> 3      TRUE   1000    C         5       S        2     27   9.0   3         3
#> 4      TRUE   1000    C         5       S        2     20   9.3   3         3
#> 5      TRUE   1000    C         5       S        2     20   9.3   3         3
#> 6      TRUE   1000    C         5       S        2     16   8.9   2         3
#> 7      TRUE   1000    C         5       S        2     25   8.9   2         3
#> 8      TRUE   1000    C         5       S        2     30   9.5   2         3
#> 9      TRUE   1000    C         5       S        2     30   9.5   2         3
#> 10     TRUE   1000    C         5       S        2     35   9.5   2         1
#> 11     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 12     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 13     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 14     TRUE   1000    C         5       S        2     23   9.6   2         1
#>    WindSpdKt RainFog HorizSun VertSun Glare Vis ObsL Rec ObsR ObsInd EffortDot
#> 1         10       1       NA      NA    NA 6.0  208 280  001   <NA>      TRUE
#> 2         10       3        2       2 FALSE 5.5  125 208  280   <NA>      TRUE
#> 3         10       1        2       2 FALSE 5.5  001 126  149   <NA>      TRUE
#> 4          6       1        8       1 FALSE 6.0  280 001  126   <NA>      TRUE
#> 5          6       1        8       1 FALSE 6.0  280 001  126   <NA>      TRUE
#> 6          6       1        9       1 FALSE 6.0  125 208  280   <NA>      TRUE
#> 7          6       1        8       2 FALSE 6.0  149 125  208   <NA>      TRUE
#> 8          6       1        8       2 FALSE 6.0  126 149  125   <NA>      TRUE
#> 9          6       1        8       2 FALSE 6.0  126 149  125   <NA>      TRUE
#> 10         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 11         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 12         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 13         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#> 14         5       3       NA      NA    NA 4.0  149 125  208   <NA>      TRUE
#>    EventNum       file_das line_num SightNo Subgroup SightNoDaily Obs ObsStd
#> 1        15 das_sample.das       15    1406     <NA>   20130113_1 208   TRUE
#> 2        35 das_sample.das       38    1407     <NA>   20130113_2 125   TRUE
#> 3        59 das_sample.das       65    <NA>     <NA>         <NA> 280  FALSE
#> 4       131 das_sample.das      137    <NA>     <NA>         <NA> 228  FALSE
#> 5       136 das_sample.das      142    1408     <NA>   20130113_3 280   TRUE
#> 6       153 das_sample.das      162    <NA>     <NA>         <NA> 231  FALSE
#> 7       167 das_sample.das      176    1409     <NA>   20130113_4 149   TRUE
#> 8       181 das_sample.das      193    1410     <NA>   20130113_5 125   TRUE
#> 9       181 das_sample.das      193    1410     <NA>   20130113_5 125   TRUE
#> 10        8 das_sample.das      238    <NA>     <NA>         <NA> 149   TRUE
#> 11       18 das_sample.das      248    1412     <NA>   20130114_1 149   TRUE
#> 12       18 das_sample.das      248    1412     <NA>   20130114_1 149   TRUE
#> 13       20 das_sample.das      252    1413     <NA>   20130114_2 208   TRUE
#> 14       20 das_sample.das      252    1413     <NA>   20130114_2 208   TRUE
#>    Bearing Reticle DistNm Cue Method Photos Birds CalibSchool PhotosAerial
#> 1      309     2.8   1.06   3      4      N     N        <NA>         <NA>
#> 2      326     0.4   2.97   3      4      Y     N        <NA>         <NA>
#> 3      120      NA   0.03  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 4      300      NA   0.02  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 5      270    14.0   0.28   3      4      N     N        <NA>         <NA>
#> 6       45      NA   0.05  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 7      344     0.2   3.68   3      4      Y     Y        <NA>         <NA>
#> 8       70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 9       70     1.4   1.66   3      4      Y     N        <NA>         <NA>
#> 10     309     1.7   1.47  NA     NA   <NA>  <NA>        <NA>         <NA>
#> 11     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 12     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 13      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#> 14      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#>    Biopsy  Prob nSp Mixed SpCode SpCodeProb GsSchoolBest GsSchoolHigh
#> 1    <NA> FALSE   1 FALSE    018       <NA>           NA           NA
#> 2    <NA> FALSE   1 FALSE    076       <NA>      8.00000        14.00
#> 3    <NA>    NA  NA    NA     LV       <NA>      1.00000           NA
#> 4    <NA>    NA  NA    NA     DC       <NA>      1.00000           NA
#> 5    <NA> FALSE   1 FALSE    037       <NA>     10.66667        20.00
#> 6    <NA>    NA  NA    NA     DC       <NA>      1.00000           NA
#> 7    <NA> FALSE   1 FALSE    016       <NA>     46.66667        79.00
#> 8    <NA> FALSE   2  TRUE    013       <NA>     41.75000        72.75
#> 9    <NA> FALSE   2  TRUE    016       <NA>     41.75000        72.75
#> 10   <NA>    NA  NA    NA   <NA>       <NA>           NA           NA
#> 11   <NA> FALSE   2  TRUE    018       <NA>    151.50000       249.00
#> 12   <NA> FALSE   2  TRUE    277       <NA>    151.50000       249.00
#> 13   <NA>  TRUE   2  TRUE    016        016     21.25000        37.75
#> 14   <NA>  TRUE   2  TRUE    277        016     21.25000        37.75
#>    GsSchoolLow  GsSpBest GsSpHigh    GsSpLow CourseSchool TurtleJFR TurtleAge
#> 1    42.333333        NA       NA  42.333333           NA      <NA>      <NA>
#> 2     5.666667   8.00000  14.0000   5.666667           NA      <NA>      <NA>
#> 3           NA   1.00000       NA         NA           NA      <NA>         A
#> 4           NA   1.00000       NA         NA           NA      <NA>         J
#> 5    10.666667  10.66667  20.0000  10.666667           NA      <NA>      <NA>
#> 6           NA   1.00000       NA         NA           NA      <NA>         A
#> 7    46.666667  46.66667  79.0000  46.666667           NA      <NA>      <NA>
#> 8    41.750000  30.06000  52.3800  30.060000           NA      <NA>      <NA>
#> 9    41.750000  11.69000  20.3700  11.690000           NA      <NA>      <NA>
#> 10          NA        NA       NA         NA           NA      <NA>      <NA>
#> 11  151.500000 128.77500 211.6500 128.775000           NA      <NA>      <NA>
#> 12  151.500000  22.72500  37.3500  22.725000           NA      <NA>      <NA>
#> 13   21.250000  15.08750  26.8025  15.087500           NA      <NA>      <NA>
#> 14   21.250000   6.16250  10.9475   6.162500           NA      <NA>      <NA>
#>    TurtleCapt PerpDistKm included
#> 1        <NA> 1.52563078     TRUE
#> 2        <NA> 3.07580701     TRUE
#> 3           N 0.04811637    FALSE
#> 4           N 0.03207758    FALSE
#> 5        <NA> 0.51856000     TRUE
#> 6        <NA> 0.06547809    FALSE
#> 7        <NA> 1.87856781     TRUE
#> 8        <NA> 2.88891582     TRUE
#> 9        <NA> 2.88891582     TRUE
#> 10       <NA> 2.11573325     TRUE
#> 11       <NA> 0.10601569     TRUE
#> 12       <NA> 0.10601569     TRUE
#> 13       <NA> 2.54265727     TRUE
#> 14       <NA> 2.54265727     TRUE
#> 
#> $randpicks
#>    effort_section randpicks
#> 1               1        NA
#> 2               2        NA
#> 3               3        NA
#> 4               4        NA
#> 5               5        NA
#> 6               6        NA
#> 7               7        NA
#> 8               8        NA
#> 9               9        NA
#> 10             10        NA
#> 
# }
```
