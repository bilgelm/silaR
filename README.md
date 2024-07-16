
<!-- README.md is generated from README.Rmd. Please edit that file -->

# silaR

<!-- badges: start -->

[![R-CMD-check](https://github.com/bilgelm/silaR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bilgelm/silaR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

silaR is an R implementation of Sampled Iterative Local Approximation
(SILA).

## Installation

You can install the development version of silaR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bilgelm/silaR")
```

## Example

As a simple example, we illustrate how to run ILLA without smoothing in
a dataset consisting of two individuals.

``` r
library(silaR)

set.seed(42)

# generate a simple small dataset
df <- tibble::tibble(
  subid = c(
    1, 1, 1,
    2, 2, 2, 2, 2, 2
  ),
  age = c(
    seq(from = 50, to = 70, length.out = 3),
    seq(from = 50, to = 70, length.out = 6)
  ),
  val = c(
    2, 4, 6,
    2, 4, 6, 8, 10, 12
  ) + stats::rnorm(3 + 6, mean = 0, sd = .1)
)

# run ILLA without smoothing
res <- illa(df, dt = 2, val0 = 2, maxi = 100, skern = 0)
```

``` r
library(ggplot2)

ggplot(df, aes(x = age, y = val, group = subid)) +
  geom_point() +
  geom_line() +
  xlim(c(50, 70)) +
  ylim(c(2, 13)) +
  xlab("Biomarker value") +
  ylab("Age (years)") +
  theme_bw()
```

<div class="figure">

<img src="man/figures/README-exampleplot-1.png" alt="Figure 1. Observed longitudinal biomarker data versus age." width="50%" />
<p class="caption">
Figure 1. Observed longitudinal biomarker data versus age.
</p>

</div>

``` r

ggplot(res$tout, aes(x = adtime, y = val)) +
  geom_point() +
  geom_line() +
  xlim(c(0, 20)) +
  ylim(c(2, 13)) +
  xlab("Biomarker value") +
  ylab("Adjusted time (years)") +
  theme_bw()
```

<div class="figure">

<img src="man/figures/README-exampleplot-2.png" alt="Figure 2. Estimated biomarker data as a function of aligned time." width="50%" />
<p class="caption">
Figure 2. Estimated biomarker data as a function of aligned time.
</p>

</div>