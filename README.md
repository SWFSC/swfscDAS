
<!-- README.md is generated from README.Rmd. Please edit that file -->

# swfscDAS

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/smwoodman/swfscDAS.svg?branch=master)](https://travis-ci.com/smwoodman/swfscDAS)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/smwoodman/swfscDAS?branch=master&svg=true)](https://ci.appveyor.com/project/smwoodman/swfscDAS)
<!-- badges: end -->

Warning: `swfscDAS` is still under active development\! Please consult
with the author (<sam.woodman@noaa.gov>) before using it.

This package contains functions for processing and analyzing DAS data
generated by WinCruz for the Southwest Fisheries Science Center.
Functionality includes reading DAS data into a data frame, processing
this data (extracting state and condition information for each DAS
event), and summarizing sighting and (soon) effort information.

## Installation

You can install `swfscDAS` from [GitHub](https://github.com) with:

``` r
# install.packages("devtools")
devtools::install_github("smwoodman/swfscDAS")
```

## DAS format

You can [download the
PDF](https://github.com/smwoodman/swfscDAS/blob/master/inst/DAS_Format.pdf)
describing the DAS data format requirements of `swfscDAS`.

## Usage

First, you must read and process the DAS data

``` r
library(swfscDAS)
# Get file paths of sample files included in the package
y <- system.file("das_sample.das", package = "swfscDAS")

# Read and process DAS file, i.e. read DAS data into a data frame and add info columns
y.read <- das_read(y)
y.proc <- das_process(y.read)

# Alternatively, the file path can be passed directly to das_process
y.proc <- das_process(y)
```

Note that `das_read` can read multiple files simultaneously

``` r
y.read.mult <- das_read(c(y, y))
```

Next, you can summarize the processed DAS data

``` r
# Summarize sighting information
y.sight <- das_sight(y.proc, mixed.multi = TRUE)

# Summarize effort - coming soon..
```

You can also check that your DAS data has accepted formatting and
values:

``` r
das_check(y)
```
