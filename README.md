
<!-- README.md is generated from README.Rmd. Please edit that file -->

# silaR

<!-- badges: start -->

[![R-CMD-check](https://github.com/bilgelm/silaR/actions/workflows/check-R-and-matlab.yaml/badge.svg)](https://github.com/bilgelm/silaR/actions/workflows/check-R-and-matlab.yaml)
[![pre-commit.ci
status](https://results.pre-commit.ci/badge/github/bilgelm/silaR/main.svg)](https://results.pre-commit.ci/latest/github/bilgelm/silaR/main)
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
a dataset consisting of two subjects, and then how to localize these two
subjects on the estimated trajectory.

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

df$Subject <- factor(df$subid)

ggplot(df, aes(x = age, y = val, group = Subject, color = Subject)) +
  geom_point() +
  geom_line() +
  xlim(c(50, 70)) +
  ylim(c(2, 13)) +
  xlab("Age (years)") +
  ylab("Biomarker value") +
  theme_bw(base_size = 20) +
  theme(legend.position = "inside", legend.position.inside = c(.15, .8))
```

<div class="figure">

<img src="man/figures/README-exampleplot-1.png" alt="Figure 1. Observed longitudinal biomarker data versus age." width="50%" />
<p class="caption">
Figure 1. Observed longitudinal biomarker data versus age.
</p>

</div>

``` r

adtime_lims <- c(-12, 25)
ggplot(res$tout, aes(x = adtime, y = val)) +
  geom_line(linewidth = 2, color = "gray", linetype = "dashed") +
  xlim(adtime_lims) +
  ylim(c(2, 13)) +
  xlab("Adjusted time (years)") +
  ylab("Biomarker value") +
  theme_bw(base_size = 20)
```

<div class="figure">

<img src="man/figures/README-exampleplot-2.png" alt="Figure 2. Estimated biomarker data as a function of aligned time." width="50%" />
<p class="caption">
Figure 2. Estimated biomarker data as a function of aligned time.
</p>

</div>

We can now estimate the time shift for each subject by projecting their
data onto the estimated trajectory.

``` r
# run SILA estimate
res_estimate_last <- sila_estimate(res$tout, df)

ggplot(res_estimate_last, aes(x = estdtt0, y = val, color = Subject)) +
  geom_point() +
  geom_line(aes(group = Subject)) +
  geom_line(
    data = res$tout,
    aes(x = adtime, y = val),
    linewidth = 2,
    color = "gray",
    linetype = "dashed"
  ) +
  xlim(adtime_lims) +
  ylim(c(2, 13)) +
  xlab("Adjusted time (years)") +
  ylab("Biomarker value") +
  ggtitle("Align using last visit") +
  theme_bw(base_size = 20) +
  theme(legend.position = "inside", legend.position.inside = c(.15, .8))
```

<div class="figure">

<img src="man/figures/README-silaestimateplot-1.png" alt="Figure 3a. Alignment using last visit. Observed value at last visit intersects the estimated (extrapolated) trajectory shown in gray." width="33%" />
<p class="caption">
Figure 3a. Alignment using last visit. Observed value at last visit
intersects the estimated (extrapolated) trajectory shown in gray.
</p>

</div>

``` r

res_estimate_first <- sila_estimate(res$tout, df, align_event = "first")

ggplot(res_estimate_first, aes(x = estdtt0, y = val, color = Subject)) +
  geom_point() +
  geom_line(aes(group = Subject)) +
  geom_line(
    data = res$tout,
    aes(x = adtime, y = val),
    linewidth = 2,
    color = "gray",
    linetype = "dashed"
  ) +
  xlim(adtime_lims) +
  ylim(c(2, 13)) +
  xlab("Adjusted time (years)") +
  ylab("Biomarker value") +
  ggtitle("Align using first visit") +
  theme_bw(base_size = 20) +
  theme(legend.position = "inside", legend.position.inside = c(.15, .8))
```

<div class="figure">

<img src="man/figures/README-silaestimateplot-2.png" alt="Figure 3b. Alignment using first visit. Observed value at first visit intersects the estimated (extrapolated) trajectory shown in gray." width="33%" />
<p class="caption">
Figure 3b. Alignment using first visit. Observed value at first visit
intersects the estimated (extrapolated) trajectory shown in gray.
</p>

</div>

``` r

res_estimate_all <- sila_estimate(res$tout, df, align_event = "all")

ggplot(res_estimate_all, aes(x = estdtt0, y = val, color = Subject)) +
  geom_point() +
  geom_line(aes(group = Subject)) +
  geom_line(
    data = res$tout,
    aes(x = adtime, y = val),
    linewidth = 2,
    color = "gray",
    linetype = "dashed"
  ) +
  xlim(adtime_lims) +
  ylim(c(2, 13)) +
  xlab("Adjusted time (years)") +
  ylab("Biomarker value") +
  ggtitle("Align using all visits") +
  theme_bw(base_size = 20) +
  theme(legend.position = "inside", legend.position.inside = c(.15, .8))
```

<div class="figure">

<img src="man/figures/README-silaestimateplot-3.png" alt="Figure 3c. Alignment using all visits. Each subject is shifted such that the sum of squared errors with the estimated (extrapolated) trajectory (shown in gray) is minimized." width="33%" />
<p class="caption">
Figure 3c. Alignment using all visits. Each subject is shifted such that
the sum of squared errors with the estimated (extrapolated) trajectory
(shown in gray) is minimized.
</p>

</div>

## Development

### Style

#### Git

This repo follows the [Conventional
Commits](https://www.conventionalcommits.org/en/v1.0.0/#summary)
specification for commit messages. Pre-commit will automatically check
your commit message and fail your commit if your commit message doesn’t
conform to the specification.
