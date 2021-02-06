
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

<!-- badges: start -->

<!-- badges: end -->

This package implements the Maximally Stable Extremal Regions algorithm
(Matas et al., 2002).

## Installation

``` r
devtools::install_github("jmartindelasierra/mser4r")
```

## Example

``` r
library(mser4r)
```

``` r
image <- load_example()
```

``` r
plot(image, axes = FALSE)
```

<img src="man/figures/README-original_image-1.png" width="100%" />

``` r
regions <- mser(image, max_area = 2000)
#> Scale factor: 0.301 (auto)
#> Output: 345x259
#> Thr. steps: 50 (Delta ~ 5)
#> Min. area: 10 (0.01%)
#> Max. area: 2000 (2.24%)
#> Extracting positive regions... done
#> Extracted regions: 359
#> Connecting regions... done
#> Extracting maximally stable regions... done
#> Maximally stable regions: 28
#> Regions merged
#> Resizing regions... done
```

``` r
plot_regions(regions, type = "border")
```

<img src="man/figures/README-mser_image_1-1.png" width="100%" />

``` r
plot_regions(regions, type = "mask", image_bg = image)
```

<img src="man/figures/README-mser_image_2-1.png" width="100%" />
