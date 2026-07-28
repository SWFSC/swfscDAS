# Split effort by strata

Split DAS effort where it intersects with a stratum boundary

## Usage

``` r
das_effort_strata(x, ...)

# S3 method for class 'data.frame'
das_effort_strata(x, ...)

# S3 method for class 'das_df'
das_effort_strata(x, strata.files, ...)
```

## Arguments

- x:

  an object of class `das_df`, or a data frame that can be coerced to
  class `das_df`

- ...:

  ignored

- strata.files:

  list of path(s) of the stratum CSV file(s); see
  [`das_effort()`](https://swfsc.github.io/swfscDAS/reference/das_effort.md)

## Value

The data frame x, with 1) columns added that indicate a) if the point
was in a particular stratum (see
[`das_intersects_strata()`](https://swfsc.github.io/swfscDAS/reference/das_intersects_strata.md)),
and b) the index of the stratum in `strata.files` (column name
'stratum'; 0 if the point intersects with no strata), and 2) two rows
added for each strata crossing that occurs between something other than
an E and R. These rows are necessary because of how `das_effort`
processes effort. The added rows are the same as the event previous to
the strata crossing, except:

- They have the event code "strataE" and "strataR", respectively

- Their coordinates are the coordinates of the intersection of the
  effort line and the stratum boundary

- Their 'idx_eff' values are plus 0.4 and 0.5, respectively

- The second added row has the same stratum info as the point
  immediately after the stratum boundary crossing

## Details

This function should only be called by
[`das_effort()`](https://swfsc.github.io/swfscDAS/reference/das_effort.md),
i.e. it should not be called by users in their personal scripts.
Practically speaking, this functions splits the effort line wherever it
crosses a stratum line. This point of intersection is interpolated;
specifically, it is determined using
[`sf::st_intersection()`](https://r-spatial.github.io/sf/reference/geos_binary_ops.html).
Thus, any effort will be first split at these effort-stratum boundary
intersection points, and then using the specified method (e.g.
condition).
