# Coerce object to a das_dfr object

Check if an object is of class `das_dfr`, or coerce it if possible.

## Usage

``` r
as_das_dfr(x)

# S3 method for class 'das_dfr'
as_das_dfr(x)

# S3 method for class 'data.frame'
as_das_dfr(x)
```

## Arguments

- x:

  an object to be coerced to class `das_dfr`

## Value

An object of class `das_dfr`

## Details

Only data frames can be coerced to an object of class `das_dfr`. If `x`
does not have column names and classes as specified in
[`das_dfr-class`](https://swfsc.github.io/swfscDAS/reference/das_dfr-class.md),
then the function returns an error message detailing the first column
that does not meet the requirements of a `das_dfr` object.

## See also

[`das_dfr-class`](https://swfsc.github.io/swfscDAS/reference/das_dfr-class.md)
