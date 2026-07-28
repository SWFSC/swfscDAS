# Summarize DAS data for a continuous effort section

Summarize DAS effort data by effort segment, while averaging or getting
the max for each condition

## Usage

``` r
das_segdata(x, ...)

# S3 method for class 'data.frame'
das_segdata(x, ...)

# S3 method for class 'das_df'
das_segdata(
  x,
  conditions,
  segdata.method = c("avg", "maxdist"),
  seg.lengths,
  section.id,
  ...
)
```

## Arguments

- x:

  an object of class `das_df`, or a data frame that can be coerced to
  class `das_df` Must contain a single continuous effort section of DAS
  data; see the Details section below

- ...:

  ignored

- conditions:

  see
  [`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md),
  or see Details section for more information

- segdata.method:

  character; either avg" or "maxdist". See Details section for more
  information

- seg.lengths:

  numeric; length of the modeling segments into which `x` will be
  chopped

- section.id:

  numeric; the ID of `x` (the current continuous effort section)

## Value

Data frame with the segdata information described in Details and in
[`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md)

## Details

WARNING - do not call this function directly! It is exported for
documentation purposes, but is intended for internal package use only.

This function was designed to be called by one of the das_chop\_
functions, e.g.
[`das_chop_equallength`](https://swfsc.github.io/swfscDAS/reference/das_chop_equallength.md),
and thus users should avoid calling it themselves. It loops through the
events in `x`, chopping `x` into modeling segments while calculating and
storing relevant information for each segment. Because `x` is a
continuous effort section, it must begin with a "B" or "R" event and end
with the corresponding "E" event.

For each segment, this function reports the segment number, segment ID,
cruise number, the start/end/mid coordinates (lat/lon), start/end/mid
date/times (DateTime), segment length, year, month, day, midpoint time,
mode, OffsetGMT value, effort type, effective strip width sides (number
of sides searched), and average conditions (which are specified by
`conditions`). The segment ID is designated as `section.id` \_ index of
the modeling segment. Thus, if `section.id` is `1`, then the segment ID
for the second segment from `x` is `"1_2"`. The start/end coordinates
and date/times are interpolated as needed, e.g. when using the
'equallength' method.

When `segdata.method` is "avg", the condition values are calculated as a
weighted average by distance. The reported value for logical columns
(e.g. Glare) is the percentage (in decimals) of the segment in which
that condition was `TRUE`. For character columns, the reported value for
each segment is the unique value(s) present in the segment, with `NA`s
omitted, pasted together via `paste(..., collapse = "; ")`. When
`segdata.method` is "maxdist", the reported values are, for each
condition, the value recorded for the longest distance during that
segment (with `NA`s omitted).

Cruise number, mode, effort type, sides searched, and file name are also
included in the segdata output. These values (excluding `NA`s) must be
consistent across the entire effort section, and thus across all
segments in `x`; a warning is printed if there are any inconsistencies

[`bearing`](https://rdrr.io/pkg/swfscMisc/man/bearing.html) and
[`destination`](https://rdrr.io/pkg/swfscMisc/man/destination.html) are
used to calculate the segment start, mid, and end points, with
`method = "vincenty"`.
