# Coerce object to a das_df object

Check if an object is of class `das_df`, or coerce it if possible.

## Usage

``` r
as_das_df(x)

# S3 method for class 'das_df'
as_das_df(x)

# S3 method for class 'data.frame'
as_das_df(x)
```

## Arguments

- x:

  an object to be coerced to class `das_df`

## Value

An object of class `das_df`

## Details

Only data frames can be coerced to an object of class `das_df`. If `x`
does not have column names and classes as specified in
[`das_df-class`](https://swfsc.github.io/swfscDAS/reference/das_df-class.md),
then the function returns an error message detailing the first column
that does not meet the requirements of a `das_df` object.

## See also

[`das_df-class`](https://swfsc.github.io/swfscDAS/reference/das_df-class.md)
