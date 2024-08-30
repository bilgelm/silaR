
<!-- README.md is generated from README.Rmd. Please edit that file -->

# silaR

<!-- badges: start -->

[![R-CMD-check](https://github.com/bilgelm/silaR/actions/workflows/check-R-and-matlab.yaml/badge.svg)](https://github.com/bilgelm/silaR/actions/workflows/check-R-and-matlab.yaml)
[![pre-commit.ci
status](https://results.pre-commit.ci/badge/github/bilgelm/silaR/main.svg)](https://results.pre-commit.ci/latest/github/bilgelm/silaR/main)
<!-- badges: end -->

silaR is an R implementation of Sampled Iterative Local Approximation
(SILA).

The original [Matlab implementation of
SILA](https://github.com/Betthauser-Neuro-Lab/SILA-AD-Biomarker) was
developed by Dr. Tobey Betthauser at the University of Wisconsin-Madison
to model longitudinal amyloid PET data and estimate individual A+ onset
age. This algorithm may have applications in other areas where long-term
population trajectories can be surmised from shorter longitudinal
assessments of individuals.

This implementation of SILA makes multiple modifications to the original
method:

- Instead of smoothing the rate vs. value function with LOESS, a
  generalized additive model with a log-link is used. Unlike LOESS, this
  ensures that the smoothed function will be non-negative.
- Integration is performed using the `lsoda` ODE integrator.
- The default approach for estimating time shifts is to use all visits
  per subject rather than the last visit only.
- An initial check examines if biomarkers are globally increasing or
  decreasing with age and tries to assess the accuracy of the
  monotonicity assumption. Functions support working with decreasing
  biomarkers.

## Installation

You can install the development version of silaR from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bilgelm/silaR@ode-integrator")
```

## Example

We illustrate how to run SILA in a simulated dataset, and then how to
localize each subject on the estimated trajectory.

``` r
library(silaR)

df <- simulated_longitudinal_data
val0 <- 21

# Train the SILA model
res <- illa(df, val0)

# Get subject-level estimates using fitted SILA model
resfit <- sila_estimate(res, align_event = "all")
```

``` r
library(dplyr)
library(ggplot2)

# spaghetti plot of value vs. age for simulated data
ggplot(df, aes(x = age, y = val, group = subid, color = factor(subid))) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = val0, color = "black", linetype = "dashed") +
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
  geom_point() +
  geom_function(fun = res$val_to_rate) +
  xlab("Value") +
  ylab(expression(Delta * "Value per Year")) +
  ggtitle("Discrete Rate Sampling Curve") +
  theme_bw()

ggplot(
  resfit,
  aes(x = adtime, y = val)
) +
  geom_point(aes(color = factor(subid))) +
  geom_line(aes(group = subid, color = factor(subid))) +
  geom_hline(
    aes(yintercept = val0, color = "threshold"),
    linetype = "dashed",
    color = "gray"
  ) +
  geom_function(
    aes(color = "Modeled curve"),
    fun = res$adtime_to_val, color = "black"
  ) +
  xlab("Time from Threshold") +
  ylab("Value") +
  ggtitle(
    expression(paste("SILA Modeled ", italic("Value vs. Time"), " Curve"))
  ) +
  theme_bw() +
  theme(
    legend.position = "none"
  )
```

<img src="man/figures/README-ratevsval-1.png" width="50%" /><img src="man/figures/README-ratevsval-2.png" width="50%" />

``` r
# value vs. time for an individual case
one_res <- resfit %>%
  filter(adtime > 1 & adtime < 10) %>%
  group_by(subid) %>%
  mutate(nvis = n()) %>%
  ungroup() %>%
  filter(nvis == 3) %>%
  slice_head(n = 3)

ggplot(one_res, aes(x = age, y = val, group = subid)) +
  geom_point() +
  geom_line(aes(color = "Individual Case Observations")) +
  geom_hline(yintercept = val0, color = "black", linetype = "dashed") +
  xlim(range(df$age)) +
  ylim(range(df$val)) +
  xlab("Age (years)") +
  ylab("Value") +
  ggtitle("Observations by Age") +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(.2, .9)
  )

ggplot(one_res, aes(x = adtime, y = val)) +
  geom_hline(yintercept = val0, color = "black", linetype = "dashed") +
  geom_function(
    aes(color = "SILA Modeled Values"),
    fun = res$adtime_to_val, color = "black"
  ) +
  geom_point() +
  geom_line(aes(color = "Individual Case Observations")) +
  xlim(range(resfit$adtime)) +
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

## Citation

To cite package ‘silaR’ in publications use:

Betthauser TJ, Bilgel M, Koscik RL, Jedynak BM, An Y, Kellett KA,
Moghekar A, Jonaitis EM, Stone CK, Engelman CD, Asthana S, Christian BT,
Wong DF, Albert M, Resnick SM, Johnson SC (2022). “Multi-method
investigation of factors influencing amyloid onset and impairment in
three cohorts.” *Brain*, *145*(11), 4065-4079.
<doi:10.1093/brain/awac213> <https://doi.org/10.1093/brain/awac213>.

A BibTeX entry for LaTeX users is

@Article{, author = {Tobey J Betthauser and Murat Bilgel and Rebecca L
Koscik and Bruno M Jedynak and Yang An and Kristina A Kellett and Abhay
Moghekar and Erin M Jonaitis and Charles K Stone and Corinne D Engelman
and Sanjay Asthana and Bradley T Christian and Dean F Wong and Marilyn
Albert and Susan M Resnick and Sterling C Johnson}, title =
{Multi-method investigation of factors influencing amyloid onset and
impairment in three cohorts}, journal = {Brain}, year = {2022}, volume =
{145}, number = {11}, pages = {4065–4079}, doi =
{10.1093/brain/awac213}, }
