# prnsamplr

The goal of prnsamplr is to coordinate survey samples with the help of permanent random numbers. 

## Installation

You can install the released version of prnsamplr from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("prnsamplr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(prnsamplr)

SampledData <- samp(method=pps, df=ExampleData, nsamp="nsamp", stratid="stratum", prn="prn", size="size")
```
