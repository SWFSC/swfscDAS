# Check DAS file

Check that DAS file has accepted formatting and values

## Usage

``` r
das_check(
  file,
  skip = 0,
  file.out = NULL,
  sp.codes.file = NULL,
  sp.codes.skip = 0,
  print.cruise.nums = TRUE
)
```

## Arguments

- file:

  filename(s) of one or more DAS files

- skip:

  integer: see
  [`read_fwf`](https://readr.tidyverse.org/reference/read_fwf.html).
  Default is 0

- file.out:

  filename to which to write the error log; default is `NULL`

- sp.codes.file:

  character; default is `NULL`. Filename of the SpCodes file from which
  to read the species codes. If `NULL`, the default (internal) file will
  be used. The SpCodes file will be read by
  [`das_spcodes_read()`](https://swfsc.github.io/swfscDAS/reference/das_spcodes.md)

- sp.codes.skip:

  integer; default is 0. Passed directly to `skip` argument of
  [`das_spcodes_read()`](https://swfsc.github.io/swfscDAS/reference/das_spcodes.md)

- print.cruise.nums:

  logical; indicates if a table with all the cruise numbers in the `x`
  should be printed using [`table`](https://rdrr.io/r/base/table.html).
  Default is `TRUE`

## Value

A data frame with columns: the file name, line number, cruise number,
'ID' (columns 4-39 from the DAS file), and description of the issue

If `file.out` is not `NULL`, then the error log data frame is also
written to `file.out` using
[`write.csv`](https://rdrr.io/r/utils/write.table.html)

A warning is printed if any events are r events; see
[`das_process`](https://swfsc.github.io/swfscDAS/reference/das_process.md)
for details about r events

## Details

Precursor to a more comprehensive DASCHECK program. This function checks
that the following is true:

- Event codes are one of the following: \#, \*, ?, 1, 2, 3, 4, 5, 6, 7,
  8, A, B, C, E, F, k, K, N, P, Q, r, R, s, S, t, V, W, g, G, p, X, Y, Z

- Latitude values are between -90 and 90 (inclusive; NA values are
  ignored)

- Longitude values are between -180 and 180 (inclusive; NA values are
  ignored)

- The effort dot matches effort determined using B, R, and E events

- There are an equal number of R and E events, and they alternate
  occurrences

- A BR event series or R event does not occur while already on effort

- An E event does not occur while already off effort

- All Data# columns for non-C events are right-justified

- Only C events have data past the 99th column in the DAS file

- The following events have NA (blank) Data# columns: \*

- All of \*, B, R, E, V, W, N, P, and Q events have NA Data# columns
  where specified (see format pdf for more details)

- Event/column pairs meet the following requirements:

|  |  |  |  |
|----|----|----|----|
| *Item* | *Event* | *Column* | *Requirement* |
| Cruise number | B | Data1 | Can be converted to a numeric value |
| Mode | B | Data2 | Must be one of C, P, c, p, or NA (blank) |
| Echo sounder | B | Data4 | Must be one of Y, N, y, n, or NA (blank) |
| Effort type | R | Data1 | Must be one of F, N, S, or NA (blank) |
| ESW sides | R | Data2 | Effective strip width; must be one of F, H, or NA (blank) |
| Course | N | Data1 | Can be converted to a numeric value |
| Speed | N | Data2 | Can be converted to a numeric value |
| Beaufort | V | Data1 | Must be a whole number between 0 and 9 |
| Swell height | V | Data2 | Can be converted to a numeric value |
| Wind speed | V | Data5 | Can be converted to a numeric value |
| Rain or fog | W | Data1 | Must be between 0 and 5 and either a whole number or have decimal value .5 |
| Horizontal sun | W | Data2 | Must be a whole number between 0 and 12 |
| Vertical sun | W | Data3 | Must be a whole number between 0 and 12 |
| Visibility | W | Data5 | Can be converted to a numeric value |
| Sighting (mammal) | S, K, M | Data3-7 | Can be converted to a numeric value |
| Sighting (mammal) | G | Data5-7 | Can be converted to a numeric value |
| Sighting cue (mammal) | S, K, M | Data3 | Must be a whole number between 1 and 6 |
| Sighting method (mammal) | S, K, M, G | Data4 | Must be a whole number between 1 and 7 |
| Bearing (mammal) | S, K, M, G | Data5 | Must be a whole number between 0 and 360 |
| Photos | A | Data3 | Must be one of N, Y, n, y, or NA (blank) |
| Birds | A | Data4 | Must be one of N, Y, n, y, or NA (blank) |
| Calibration school | S, K, M | Data10 | Must be one of N, Y, n, y, or NA (blank) |
| Aerial photos taken | S, K, M | Data11 | Must be one of N, Y, n, y, or NA (blank) |
| Biopsy taken | S, K, M | Data12 | Must be one of N, Y, n, y, or NA (blank) |
| Species codes | A | Data5-8 | If a species codes file is provided, must be one of the provided codes |
| Resight | s, k | Data2-5 | Can be converted to a numeric value |
| Turtle species | t | Data2 | If a species codes file is provided, must be one of the provided codes |
| Turtle sighting | t | Data3-5, 7 | Can be converted to a numeric value |
| Turtle JFR | t | Data6 | Must be one of F, J, N, R, or NA (blank) |
| Fishing vessel | F | Data2-4 | Can be converted to a numeric value |
| Sighting info | 1-8 | Data2-8 | Can be converted to a numeric value |
| Sighting info | 1-8 | Data9 | The Data9 column must be NA (blank) for events 1-8 |

In the table above, 'between' means inclusive.

Long-term items, and checks that are not performed:

- Check that datetimes are sequential, meaning they 1) are the same as
  or 2) come after the previous event

- Check that A events only come immediately after a G/S/K/M event, and
  all G/S/K/M events have an A after them. And that each has at least
  one group size estimate (1:8 event)

## Examples

``` r
y <- system.file("extdata", "das_sample.das", package = "swfscDAS")
if (interactive()) das_check(y)

# Using your own SpCodes file
sp.codes.file <- system.file("extdata", "SpCodes.dat", package = "swfscDAS")
if (interactive()) das_check(y, sp.codes.file = sp.codes.file)
```
