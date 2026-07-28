# Chop DAS data - condition

Chop DAS data into a new effort segment every time a specified condition
changes

## Usage

``` r
das_chop_condition(x, ...)

# S3 method for class 'data.frame'
das_chop_condition(x, ...)

# S3 method for class 'das_df'
das_chop_condition(
  x,
  conditions,
  seg.min.km = 0.1,
  distance.method = NULL,
  num.cores = NULL,
  ...
)
```

## Arguments

- x:

  an object of class `das_df`, or a data frame that can be coerced to
  class `das_df`. This data must be filtered for continuous effort
  sections; see the Details section below

- ...:

  ignored

- conditions:

  the conditions that trigger a new segment; see
  [`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md)

- seg.min.km:

  numeric; minimum allowable segment length (in kilometers). Default is
  0.1. See the Details section below for more information

- distance.method:

  character; see
  [`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md).
  Default is `NULL` since these distances should have already been
  calculated

- num.cores:

  see
  [`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md)

## Value

List of two data frames:

- `x`, with columns added for the corresponding unique segment code and
  number

- segdata: data frame with one row for each segment, and columns with
  relevant data (see
  [`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md)
  for specifics)

## Details

WARNING - do not call this function directly! It is exported for
documentation purposes, but is intended for internal package use only.

This function is intended to be called by
[`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md)
when the "condition" method is specified. Thus, `x` must be filtered for
events (rows) where either the 'OnEffort' column is `TRUE` or the
'Event' column is "E"; see
[`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md)
for more details. This function chops each continuous effort section
(henceforth 'effort sections') in `x` into modeling segments (henceforth
'segments') by creating a new segment every time a specified condition
changes. Each effort section runs from an "R" event to its corresponding
"E" event. After chopping,
[`das_segdata`](https://swfsc.github.io/swfscDAS/reference/das_segdata.md)
is called (with `segdata.method = "maxdist"`) to get relevant segdata
information for each segment.

Changes in the one of the conditions specified in the `conditions`
argument triggers a new segment. One exception is if the event at which
this condition change occurs is part of an event series, meaning one of
several events in a row at the same lat/lon points (such as a PVNW event
series). In this situation, the final event of the event series is
considered the last event of the current effort segment, and thus also
the start of the next effort segment.

Related, when multiple condition changes happen at the same lat/lon
points, such as a "RPVNW" series of events at the beginning of the
effort section. When this happens, no segments of length zero are
created; rather, a single segment is created that includes all of the
condition changes (i.e. all of the events in the event series) that
happened during the series of events (i.e. at the same location). Note
that this combining of events at the same position happens even if
`seg.min.km = 0`.

In addition, (almost) all segments whose length is less than
`seg.min.km` are combined with the segment immediately following them to
ensure that the length of (almost) all segments is at least
`seg.min.km`. This allows users to account for situations where multiple
conditions, such as Beaufort and the visibility, change in rapid
succession, for instance \<0.1 km apart. When segments are combined, a
message is printed, and the condition that was recorded for the maximum
distance within the new segment is reported. See
[`das_segdata`](https://swfsc.github.io/swfscDAS/reference/das_segdata.md),
`segdata.method = "maxdist"`, for more details about how the segdata
information is determined. The only exception to this rule is if the
short segment ends in an "E" event, meaning it is the last segment of
the effort section. Since in this case there is no 'next' segment, this
short segment is left as-is.

If the column `dist_from_prev` does not exist, the distance between
subsequent events is calculated as described in
[`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md)
