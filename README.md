epivizR
========

## Interactive communication between `epiviz` and R/Bioconductor Sessions

The epivizr package implements two-way communication between the [R/Bioconductor](http://bioconductor.org) environment and [epiviz](http://epiviz.cbcb.umd.edu). Objects in the R environment can be displayed as tracks or scatterplots on Epiviz. Epivizr uses Websockets for communication between the browser Javascript client and the R environment using the same technology underlying the popular [Shiny](http://www.rstudio.com/shiny) system for authoring interactive web-based reports in R.

 

## Installation
Epivizr is available as an R package. We have extended some of the underlying Bioconductor code in the `IRanges` (`IntervalForest` class) and `GenomicRanges` (`GIntervalTree` class) packages for more efficient performance. These are now available in the Bioconductor devel branch (see
[http://bioconductor.org/developers/how-to/useDevel/](http://bioconductor.org/developers/how-to/useDevel/) for more info. The easiest way to install `epivizr` is the following: 

**YOU ARE USING DEVELOPMENT SOFTWARE, USE WITH CAUTION.**

```{r}
install.packages("devtools")
library(devtools)
install_github("epivizr", user="epiviz")
```

## Try it out

The easiest way to try it `epivizr` out is to follow the package vignette:

```{r}
require(epivizr)
browseVignettes("epivizr")
```

## A quick tour

You can get a quick tour of epiviz here: [http://youtu.be/099c4wUxozA](http://youtu.be/099c4wUxozA)
