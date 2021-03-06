
<!-- README.md is generated from README.Rmd. Please edit that file -->

# s2plot

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

The goal of s2plot is to provide a quick plot function for
[s2](https://github.com/r-spatial/s2) geographies to facilitate
development of that package.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paleolimbot/s2plot")
```

## Example

Plot s2 geographies\!

``` r
library(s2plot)
library(s2)

s2plot(s2::s2_data_countries(), col = "grey90")
s2plot(s2::s2_data_cities("London"), pch = 16, add = T)
```

<img src="man/figures/README-example-1.png" width="100%" />

Make a spinning globe\!

``` r
countries <- s2_data_countries()
ocean <- s2_difference(
  # make a polygon of the whole earth!
  as_s2_geography(TRUE),
  s2_union_agg(countries)
)

lat <- 0
for (lon in seq(0, -360, length.out = 101)[-1]) {
  s2plot(
    countries, 
    col = "white", border = "black", 
    projection = s2plot_projection_orthographic(sprintf("POINT (%s %s)", lon, lat)),
    xlim = c(-1, 1),
    ylim = c(-1, 1)
  )
  
  s2plot(ocean, col = "#0073B8", add = T)
}
```

<img src="man/figures/README-spin-.gif" width="100%" />
