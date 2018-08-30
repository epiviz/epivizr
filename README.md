epivizR
========

The `epivizr` Bioconductor package implements two-way communication between the [R/Bioconductor](http://bioconductor.org) environment and the [epiviz](http://epiviz.cbcb.umd.edu) web app for interactive data visualization. Objects in the R environment can be displayed as tracks or plots on Epiviz. Epivizr uses Websockets for communication between the browser Javascript client and the R environment using the same technology underlying the popular [Shiny](http://www.rstudio.com/shiny) system for authoring interactive web-based reports in R.

## Installation and requirements
Epivizr is available as part of the [Bioconductor](http://bioconductor.org) project as of version 2.13. To install the release version of `epivizr`:

```{r}
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("epivizr")
```

## Development version

This github repository contains the latest and greatest version of `epivizr` and is tracked by the devel version in Bioconductor (see
[http://bioconductor.org/developers/how-to/useDevel/](http://bioconductor.org/developers/how-to/useDevel/) for more info. 

## Try it out

The easiest way to try `epivizr` out is to follow the package vignette:

```{r}
require(epivizr)
browseVignettes("epivizr")
```

## A quick tour

You can get a quick tour of epiviz here: [http://youtu.be/099c4wUxozA](http://youtu.be/099c4wUxozA)

## Non-blocking

As of version 1.3, Epivizr supports a non-blocking workflow on both UNIX-like and Windows systems where data is served to the webapp without blocking
the R/bioc interactive session. Make sure you are using the latest version of the [httpuv package](http://cran.r-project.org/web/packages/httpuv/index.html) to use this. (Thanks to the
[Rstudio](http://rstudio.org) folks for folding our daemonizing code into the main httpuv release).

## More info

Check out the `epiviz` [project page on github](http:://github.com/epiviz), and the
[documentation page](http://epiviz.github.io).


