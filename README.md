# tracelib

<!-- badges: start -->
<!-- badges: end -->

The goal of tracelib is to have a automated Data workflow documentation wherever possible. This is a library with classes, methods, functions to capture metadata for files, actions and related entities, to write and read the metadata into temporary objects, files and a data base

## Installation

You can install the released version of tracelib from Package archive file with:

``` r
install.packages("path_to_package")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(tracelib)
## basic example code
tStartRun("File")

call_tstart_action()

tReadCsv(filepath)

call_tend_action()

tEndRun(jsonFilePath)
```

