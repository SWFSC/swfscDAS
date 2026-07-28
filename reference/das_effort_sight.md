# Summarize DAS sightings by effort segment

Summarize number of sightings and animals for selected species by
segment

## Usage

``` r
das_effort_sight(
  x.list,
  sp.codes,
  sp.events = c("S", "G", "K", "M", "t", "p"),
  gs.columns = c("GsSpBest", "GsSpLow", "GsSpHigh")
)
```

## Arguments

- x.list:

  output of
  [`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md);
  a list of three data frames named 'segdata', 'sightinfo', and
  'randpicks', respectively

- sp.codes:

  character; species code(s) to include in segdata output. These must
  exactly match the species codes in the data, such as including leading
  zeros

- sp.events:

  character; event code(s) to include in the sightinfo output. This
  argument supersedes the 'included' value when determining whether a
  sighting is included in the segment summaries. Must be one or more of:
  "S", "K", "M", "G", "t", "p" (case-sensitive). The default is that all
  of these event codes are kept

- gs.columns:

  character; the column(s) to use to get the group size values that will
  be summarized in the segdata output. Must be one or more of
  'GsSpBest', 'GsSpLow', and 'GsSpBest' (case-sensitive). See Details
  section for more information

## Value

A list, identical to `x.list` except for

1.  the nSI and ANI columns added to `x.list$segdata`, one each for each
    element of `sp.codes`, and

2.  the 'included' column of `x.list$sightinfo`, which has been set as
    `FALSE` for sightings of species not listed in `sp.codes`. Thus, the
    'included' column in the output accurately reflects the sightings
    that were included in the effort segment summaries

## Details

This function takes the output of
[`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md)
and adds columns for the number of sightings (nSI) and number of animals
(ANI) for selected species (selected via `sp.codes`) for each segment to
the segdata element of `x.list`. However, only sightings with an
included value of `TRUE` (included is a column in sightinfo) are
included in the summaries. Having this step separate from
[`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md)
allows users to personalize the included values as desired for their
analysis.

The ANI columns are the sum of the 'GsSp...' column(s) from
[`das_sight`](https://swfsc.github.io/swfscDAS/reference/das_sight.md)
specified using `gs.columns`. If `gs.columns` specifies more than one
column, then the secondary columns will only be used if the values for
the previous columns are `NA`. For instance, if
`gs.columns = c('GsSpBest', 'GsSpLow')`, then for each row in sightinfo,
the value from GsSpLow will be used only if the value from GsSpBest is
`NA`

## Examples

``` r
y <- system.file("extdata", "das_sample.das", package = "swfscDAS")
y.proc <- das_process(y)
y.eff.cond <- das_effort(
  y.proc, method = "condition", conditions = "Bft", seg.min.km = 0.05,
  num.cores = 1
)

das_effort_sight(y.eff.cond, sp.codes = c("013", "076", "DC"), sp.events = c("S", "t"))
#> $segdata
#>    segnum section_id section_sub_id           file stlin endlin     lat1
#> 1       1          1              1 das_sample.das     2     20 39.32033
#> 2       2          2              1 das_sample.das    23     43 39.37617
#> 3       3          3              1 das_sample.das    59     70 39.56800
#> 4       4          3              2 das_sample.das    70     90 39.66133
#> 5       5          4              1 das_sample.das    99    121 39.94517
#> 6       6          5              1 das_sample.das   127    147 40.15217
#> 7       7          6              1 das_sample.das   150    160 40.26867
#> 8       8          6              2 das_sample.das   160    164 40.32033
#> 9       9          7              1 das_sample.das   167    174 40.38250
#> 10     10          7              2 das_sample.das   174    181 40.42965
#> 11     11          8              1 das_sample.das   188    199 40.52200
#> 12     12          9              1 das_sample.das   232    240 40.98717
#> 13     13         10              1 das_sample.das   242    259 41.02383
#>         lon1           DateTime1     lat2      lon2           DateTime2
#> 1  -137.6043 2013-01-13 06:27:39 39.36716 -137.5817 2013-01-13 06:46:25
#> 2  -137.5978 2013-01-13 06:58:04 39.51933 -137.5277 2013-01-13 07:57:05
#> 3  -137.4530 2013-01-13 09:22:13 39.66133 -137.4130 2013-01-13 09:59:50
#> 4  -137.4130 2013-01-13 09:59:50 39.75433 -137.4107 2013-01-13 10:36:27
#> 5  -137.3692 2013-01-13 11:51:51 40.12745 -137.2488 2013-01-13 13:16:38
#> 6  -137.1737 2013-01-13 13:50:07 40.26617 -137.1348 2013-01-13 14:38:13
#> 7  -137.1268 2013-01-13 14:59:19 40.32033 -137.1108 2013-01-13 15:20:26
#> 8  -137.1108 2013-01-13 15:20:26 40.37596 -137.0915 2013-01-13 15:43:08
#> 9  -137.0977 2013-01-13 15:58:41 40.42965 -137.0745 2013-01-13 16:20:02
#> 10 -137.0745 2013-01-13 16:20:02 40.45133 -137.0628 2013-01-13 16:29:50
#> 11 -137.0533 2013-01-13 16:59:54 40.52533 -137.0515 2013-01-13 17:01:21
#> 12 -135.5980 2013-01-14 11:24:32 41.01582 -135.5782 2013-01-14 11:37:24
#> 13 -135.5743 2013-01-14 11:40:38 41.04599 -135.5595 2013-01-14 11:50:29
#>        mlat      mlon           mDateTime    dist year month day    mtime
#> 1  39.34377 -137.5930 2013-01-13 06:37:02  5.5577 2013     1  13 06:37:02
#> 2  39.44767 -137.5625 2013-01-13 07:27:34 17.0106 2013     1  13 07:27:34
#> 3  39.61458 -137.4326 2013-01-13 09:41:01 10.9225 2013     1  13 09:41:01
#> 4  39.70804 -137.3939 2013-01-13 10:18:08 10.8861 2013     1  13 10:18:08
#> 5  40.03679 -137.3101 2013-01-13 12:34:14 22.7090 2013     1  13 12:34:14
#> 6  40.20895 -137.1531 2013-01-13 14:14:10 13.0922 2013     1  13 14:14:10
#> 7  40.29448 -137.1187 2013-01-13 15:09:52  5.8993 2013     1  13 15:09:52
#> 8  40.34833 -137.1019 2013-01-13 15:31:47  6.4018 2013     1  13 15:31:47
#> 9  40.40618 -137.0864 2013-01-13 16:09:21  5.5964 2013     1  13 16:09:21
#> 10 40.44053 -137.0687 2013-01-13 16:24:56  2.6020 2013     1  13 16:24:56
#> 11 40.52365 -137.0524 2013-01-13 17:00:37  0.4016 2013     1  13 17:00:37
#> 12 41.00151 -135.5881 2013-01-14 11:30:58  3.5940 2013     1  14 11:30:58
#> 13 41.03500 -135.5671 2013-01-14 11:45:33  2.7600 2013     1  14 11:45:33
#>    Cruise Mode OffsetGMT EffType ESWsides maxdistBft nSI_013 ANI_013 nSI_076
#> 1    1000    C         5       S        2          3       0    0.00       0
#> 2    1000    C         5       S        2          3       0    0.00       1
#> 3    1000    C         5       S        2          3       0    0.00       0
#> 4    1000    C         5       S        2          2       0    0.00       0
#> 5    1000    C         5       S        2          3       0    0.00       0
#> 6    1000    C         5       S        2          3       0    0.00       0
#> 7    1000    C         5       S        2          3       0    0.00       0
#> 8    1000    C         5       S        2          2       0    0.00       0
#> 9    1000    C         5       S        2          3       0    0.00       0
#> 10   1000    C         5       S        2          2       0    0.00       0
#> 11   1000    C         5       S        2          2       1   30.06       0
#> 12   1000    C         5       S        2          2       0    0.00       0
#> 13   1000    C         5       S        2          2       0    0.00       0
#>    ANI_076 nSI_DC ANI_DC
#> 1        0      0      0
#> 2        8      0      0
#> 3        0      0      0
#> 4        0      0      0
#> 5        0      0      0
#> 6        0      0      0
#> 7        0      0      0
#> 8        0      0      0
#> 9        0      0      0
#> 10       0      0      0
#> 11       0      0      0
#> 12       0      0      0
#> 13       0      0      0
#> 
#> $sightinfo
#>    segnum     mlat      mlon Event            DateTime year      Lat       Lon
#> 1       1 39.34377 -137.5930     S 2013-01-13 06:46:02 2013 39.36617 -137.5820
#> 2       2 39.44767 -137.5625     S 2013-01-13 07:56:22 2013 39.51767 -137.5285
#> 3       3 39.61458 -137.4326     t 2013-01-13 09:34:27 2013 39.59733 -137.4400
#> 4       6 40.20895 -137.1531     t 2013-01-13 14:02:55 2013 40.18283 -137.1622
#> 5       6 40.20895 -137.1531     S 2013-01-13 14:37:56 2013 40.26567 -137.1350
#> 6       8 40.34833 -137.1019     t 2013-01-13 15:36:47 2013 40.36050 -137.0978
#> 7      10 40.44053 -137.0687     S 2013-01-13 16:29:50 2013 40.45133 -137.0628
#> 8      11 40.52365 -137.0524     S 2013-01-13 17:00:45 2013 40.52400 -137.0522
#> 9      11 40.52365 -137.0524     S 2013-01-13 17:00:45 2013 40.52400 -137.0522
#> 10     13 41.03500 -135.5671     S 2013-01-14 11:47:51 2013 41.04017 -135.5635
#> 11     13 41.03500 -135.5671     S 2013-01-14 11:47:51 2013 41.04017 -135.5635
#> 12     13 41.03500 -135.5671     S 2013-01-14 11:49:14 2013 41.04333 -135.5615
#> 13     13 41.03500 -135.5671     S 2013-01-14 11:49:14 2013 41.04333 -135.5615
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
#> 10     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 11     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 12     TRUE   1000    C         5       S        2     23   9.6   2         1
#> 13     TRUE   1000    C         5       S        2     23   9.6   2         1
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
#> 10       18 das_sample.das      248    1412     <NA>   20130114_1 149   TRUE
#> 11       18 das_sample.das      248    1412     <NA>   20130114_1 149   TRUE
#> 12       20 das_sample.das      252    1413     <NA>   20130114_2 208   TRUE
#> 13       20 das_sample.das      252    1413     <NA>   20130114_2 208   TRUE
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
#> 10     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 11     359     0.3   3.28   2      4      Y     N        <NA>         <NA>
#> 12      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
#> 13      38     0.8   2.23   3      4      Y     N        <NA>         <NA>
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
#> 10   <NA> FALSE   2  TRUE    018       <NA>    151.50000       249.00
#> 11   <NA> FALSE   2  TRUE    277       <NA>    151.50000       249.00
#> 12   <NA>  TRUE   2  TRUE    016        016     21.25000        37.75
#> 13   <NA>  TRUE   2  TRUE    277        016     21.25000        37.75
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
#> 10  151.500000 128.77500 211.6500 128.775000           NA      <NA>      <NA>
#> 11  151.500000  22.72500  37.3500  22.725000           NA      <NA>      <NA>
#> 12   21.250000  15.08750  26.8025  15.087500           NA      <NA>      <NA>
#> 13   21.250000   6.16250  10.9475   6.162500           NA      <NA>      <NA>
#>    TurtleCapt PerpDistKm included
#> 1        <NA> 1.52563078    FALSE
#> 2        <NA> 3.07580701     TRUE
#> 3           N 0.04811637    FALSE
#> 4           N 0.03207758    FALSE
#> 5        <NA> 0.51856000    FALSE
#> 6        <NA> 0.06547809    FALSE
#> 7        <NA> 1.87856781    FALSE
#> 8        <NA> 2.88891582     TRUE
#> 9        <NA> 2.88891582    FALSE
#> 10       <NA> 0.10601569    FALSE
#> 11       <NA> 0.10601569    FALSE
#> 12       <NA> 2.54265727    FALSE
#> 13       <NA> 2.54265727    FALSE
#> 
#> $randpicks
#> NULL
#> 
```
