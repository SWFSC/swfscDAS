# Chop DAS data - section

Chop DAS data into effort segments by continuous effort section

## Usage

``` r
das_chop_section(x, ...)

# S3 method for class 'data.frame'
das_chop_section(x, ...)

# S3 method for class 'das_df'
das_chop_section(x, conditions, distance.method = NULL, num.cores = NULL, ...)
```

## Arguments

- x:

  an object of class `das_df`, or a data frame that can be coerced to
  class `das_df` This data must be filtered for 'OnEffort' events; see
  the Details section below

- ...:

  ignored

- conditions:

  see
  [`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md)

- distance.method:

  character; see
  [`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md).
  Default is `NULL` since these distances should have already been
  calculated

- num.cores:

  see
  [`das_effort`](https://swfsc.github.io/swfscDAS/reference/das_effort.md)

## Value

See
[`das_chop_equallength()`](https://swfsc.github.io/swfscDAS/reference/das_chop_equallength.md).
The randpicks values will all be `NA`

## Details

WARNING - do not call this function directly! It is exported for
documentation purposes, but is intended for internal package use only.

This function is simply a wrapper for
[`das_chop_equallength`](https://swfsc.github.io/swfscDAS/reference/das_chop_equallength.md).
It calls
[`das_chop_equallength`](https://swfsc.github.io/swfscDAS/reference/das_chop_equallength.md),
with `seg.km` set to a value larger than the longest continuous effort
section in `x`. Thus, the effort is 'chopped' into the continuous effort
sections and then summarized.

See the Examples section for an example where the two methods give the
same output. Note that the longest continuous effort section in the
sample data is ~22km.

## Examples

``` r
y <- system.file("extdata", "das_sample.das", package = "swfscDAS")
y.proc <- das_process(y)

y.eff1 <- das_effort(y.proc, method = "equallength", seg.km = 25, num.cores = 1)
#> No argument was passed via randpicks.load, and thus new randpicks values will be generated
y.eff2 <- das_effort(y.proc, method = "section", num.cores = 1)

all.equal(y.eff1, y.eff2)
#> [1] TRUE
```
