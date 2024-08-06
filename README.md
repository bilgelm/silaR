
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

We illustrate how to run SILA in a simulated dataset, and then how to
localize each subject on the estimated trajectory.

``` r
# Train the SILA model
df <- simulated_longitudinal_data
res <- sila(df, dt = 0.25, val0 = 21, maxi = 200)

resfit <- sila_estimate(res$tsila, df)
```

``` r
# spaghetti plot of value vs. age for simulated data
ggplot(df, aes(x = age, y = val, group = subid, color = factor(subid))) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 21, color = "black", linetype = "dashed") +
  xlab("Age (years)") +
  ylab("Value") +
  ggtitle("Simulated Input Data") +
  theme_bw() +
  theme(legend.position = "none")
```

<img src="man/figures/README-simdata-1.png" width="50%" />

``` r
# plots showing the output from discrete rate sampling (i.e., rate vs. value)
# and modeled value vs. time data.
ggplot(res$tdrs, aes(x = val, y = rate)) +
  geom_ribbon(aes(ymin = rate - ci, ymax = rate + ci), alpha = .3) +
  geom_line() +
  xlab("Value") +
  ylab(expression(Delta * "Value per Year")) +
  ggtitle("Discrete Rate Sampling Curve") +
  theme_bw()

ggplot(res$tsila, aes(x = adtime, y = val)) +
  geom_line(aes(color = "Modeled curve")) +
  geom_hline(aes(yintercept = 21, color = "threshold"), linetype = "dashed") +
  xlab("Time from Threshold") +
  ylab("Value") +
  ggtitle(
    expression(paste("SILA Modeled ", italic("Value vs. Time"), " Curve"))
  ) +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(.15, .85)
  )
```

<img src="man/figures/README-ratevsval-1.png" width="50%" /><img src="man/figures/README-ratevsval-2.png" width="50%" />

``` r
# value vs. time for all subjects
ggplot(
  resfit,
  aes(x = estdtt0, y = val, group = subid, color = factor(subid))
) +
  geom_point() +
  geom_line() +
  geom_line(
    data = res$tsila,
    aes(x = adtime, y = val, group = NULL, color = NULL)
  ) +
  geom_hline(yintercept = 21, color = "black", linetype = "dashed") +
  xlab("Estimated time to threshold (years)") +
  ylab("Value") +
  ggtitle("Data Aligned by Estimated Time to Threshold") +
  theme_bw() +
  theme(legend.position = "none")
```

<img src="man/figures/README-valvstime-1.png" width="50%" />

``` r
# value vs. time for an individual case
one_res <- resfit %>%
  filter(estdtt0 > 1 & estdtt0 < 10) %>%
  group_by(subid) %>%
  mutate(nvis = n()) %>%
  ungroup() %>%
  filter(nvis == 3) %>%
  slice_head(n = 3)

ggplot(one_res, aes(x = age, y = val, group = subid)) +
  geom_point() +
  geom_line(aes(color = "Individual Case Observations")) +
  geom_hline(yintercept = 21, color = "black", linetype = "dashed") +
  xlim(range(df$age)) +
  ylim(range(res$tsila$val)) +
  xlab("Age (years)") +
  ylab("Value") +
  ggtitle("Observations by Age") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(.2, .9)
  )

ggplot(one_res, aes(x = estdtt0, y = val, group = subid)) +
  geom_point() +
  geom_line(aes(color = "Individual Case Observations")) +
  geom_hline(yintercept = 21, color = "black", linetype = "dashed") +
  geom_line(
    data = res$tsila,
    aes(x = adtime, y = val, group = NULL, color = "SILA Modeled Values")
  ) +
  xlim(range(res$tsila$adtime)) +
  xlab("Estimated time to threshold (years)") +
  ylab("Value") +
  ggtitle("Observations by Estimated Time to Threshold") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(.2, .9)
  )
```

<img src="man/figures/README-valvstimeone-1.png" width="50%" /><img src="man/figures/README-valvstimeone-2.png" width="50%" />

## Development

### Style

#### Git

This repo follows the [Conventional
Commits](https://www.conventionalcommits.org/en/v1.0.0/#summary)
specification for commit messages. Pre-commit will automatically check
your commit message and fail your commit if your commit message doesn’t
conform to the specification.
