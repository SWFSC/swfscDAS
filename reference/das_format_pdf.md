# DAS format requirements

Save the PDF document describing the DAS format required by `swfscDAS`
to a specified file

## Usage

``` r
das_format_pdf(file, ...)
```

## Arguments

- file:

  character, the name of the file where the PDF will be saved

- ...:

  passed on to [`base::file.copy()`](https://rdrr.io/r/base/files.html);
  might included named argument `overwrite`

## Value

output of [`base::file.copy()`](https://rdrr.io/r/base/files.html);
`TRUE` if writing of file was successful, and `FALSE` otherwise

## Details

A wrapper function for
[`base::file.copy()`](https://rdrr.io/r/base/files.html). This function
saves the PDF document describing the DAS data format requirements by
copying the PDF document located
at`system.file("DAS_Format.pdf", package = "swfscDAS")` to `file`

This file can also be downloaded from
<https://github.com/swfsc/swfscDAS/blob/master/inst/DAS_Format.pdf>

## Examples

``` r
das_format_pdf(file.path(tempdir(), "DAS_Format.pdf"), overwrite = FALSE)
#> [1] TRUE
```
