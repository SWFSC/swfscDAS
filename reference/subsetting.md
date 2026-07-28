# Subsetting objects created using swfscDAS

Subsetting `das_dfr` or `das_df` objects

## Usage

``` r
# S3 method for class 'das_dfr'
x[i, j, ..., drop = TRUE]

# S3 method for class 'das_dfr'
x$name <- value

# S3 method for class 'das_dfr'
x[i, j, ...] <- value

# S3 method for class 'das_dfr'
x[[i]] <- value

# S3 method for class 'das_df'
x[i, j, ..., drop = TRUE]

# S3 method for class 'das_df'
x$name <- value

# S3 method for class 'das_df'
x[i, j, ...] <- value

# S3 method for class 'das_df'
x[[i]] <- value
```

## Arguments

- x:

  object of class `das_dfr` or `das_df`

- i, j, ...:

  elements to extract or replace, see
  [`[.data.frame`](https://rdrr.io/r/base/Extract.data.frame.html)

- drop:

  logical, see
  [`[.data.frame`](https://rdrr.io/r/base/Extract.data.frame.html)

- name:

  A literal character string or ..., see
  [`[.data.frame`](https://rdrr.io/r/base/Extract.data.frame.html)

- value:

  A suitable replacement value, see
  [`[.data.frame`](https://rdrr.io/r/base/Extract.data.frame.html)

## Details

When subsetting a `das_dfr` or `das_df` object, henceforth a `das_`
object, using any of the functions described in
[`[.data.frame`](https://rdrr.io/r/base/Extract.data.frame.html), then
then the `das_` class is simply dropped and the object is of class
`data.frame`. This is because of the strict format requirements of
`das_` objects; it is likely that a subsetted `das_` object will not
have the format required by subsequent swfscDAS functions, and thus it
is safest to drop the `das_` class. If a data frame is passed to
downstream `swfscDAS` functions that require a `das_` object, then they
will attempt to coerce the object to the necessary `das_` class See
[`as_das_dfr`](https://swfsc.github.io/swfscDAS/reference/as_das_dfr.md)
and
[`as_das_df`](https://swfsc.github.io/swfscDAS/reference/as_das_df.md)
for more details.

## Examples

``` r
y <- system.file("extdata", "das_sample.das", package = "swfscDAS")
y.read <- das_read(y)

# All return a data frame:
class(y.read[1:10, ])
#> [1] "data.frame"
class(y.read[, 1:10])
#> [1] "data.frame"

y.df <- y.read
y.df[, 1] <- "a"
class(y.df)
#> [1] "data.frame"

y.df <- y.read
y.df$Event <- "a"
class(y.df)
#> [1] "data.frame"

y.df <- y.read
y.df[["Event"]] <- "a"
class(y.df)
#> [1] "data.frame"
```
