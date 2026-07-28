# Read DAS SpCodes file

Read DAS SpCodes file

## Usage

``` r
das_spcodes_read(file, skip = 0)
```

## Arguments

- file:

  character; filename of .dat file from which to read species codes

- skip:

  integer; default is 3. Number of lines to skip when reading SpCodes
  file. See
  [`readr::read_fwf()`](https://readr.tidyverse.org/reference/read_fwf.html)
  for more details

## Value

Data frame with four columns:

- SpCode: species code, columns 1 to 4

- Abbr: species abbreviation, columns 6 to 15

- SciName: species scientific name, columns 18 to 57

- CommonName: species common name, columns 58 until the end of the line

## Details

Provide a standardized function to read a shipboard DAS SpCodes file.
Methods described in 'returns'

## Examples

``` r
sp.codes.file <- system.file("extdata", "SpCodes_sample.dat", package = "swfscDAS")
das_spcodes_read(sp.codes.file)
#> # A tibble: 9 × 4
#>   SpCode Abbr  SciName  CommonName 
#>   <chr>  <chr> <chr>    <chr>      
#> 1 013    Abbr1 SciName1 CommonName1
#> 2 016    Abbr2 SciName2 CommonName2
#> 3 018    Abbr3 SciName3 CommonName3
#> 4 037    Abbr4 SciName4 CommonName4
#> 5 075    Abbr5 SciName5 CommonName5
#> 6 076    Abbr6 SciName6 CommonName6
#> 7 277    Abbr7 SciName7 CommonName7
#> 8 DC     Abbr8 SciName8 CommonName8
#> 9 LV     Abbr9 SciName9 CommonName9
```
