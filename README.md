# bruno

**Predicting User-Defined Event Recurrence under Exchangeability**

`bruno` (Beta–Bernoulli Recurrence for Unobserved Next Outcomes) is an R package for analytical prediction of the recurrence of user-defined binary events. Events can be supplied directly as binary indicators or defined using paired pre–post measurements. Predictions concern the next exchangeable case or the number of occurrences in a specified future sample, within a substantively defined predictive class.

**Version:** 0.1.0 · **R:** ≥ 4.1.0 · **Licence:** GPL-3  
**CRAN:** https://cran.r-project.org/package=bruno

## Installation

```r
install.packages("bruno")
library(bruno)
```

## Statistical framework

Let $E_1,E_2,\ldots$ denote binary indicators of a specified event. The predictive interpretation requires a judgement that the observed and future event indicators are exchangeable within the intended predictive class.

For an indefinitely extendible exchangeable binary sequence, de Finetti's representation theorem permits the joint distribution to be represented as a mixture of Bernoulli laws. `bruno` introduces an additional **beta distribution assumption** for the mixing distribution. Exchangeability alone does not imply this parametric specification.

The user specifies an initial event probability $\mu_0\in(0,1)$ and a concentration parameter $\tau>0$. The initial beta parameters are

$$
a = \mu_0\tau, \qquad b = (1-\mu_0)\tau.
$$

If the specified event occurs $s$ times among $n$ analysable cases, conditioning on the observed event count gives

$$
\theta\mid A_{n,s}\sim\operatorname{Beta}(a+s,\;b+n-s).
$$

The predictive probability of occurrence for the next exchangeable case is

$$
P(E_{n+1}=1\mid A_{n,s})
=\frac{a+s}{a+b+n}
=\frac{\mu_0\tau+s}{\tau+n}.
$$

For a prespecified future sample of $m$ cases, the number of event occurrences $T$ follows the beta-binomial predictive distribution:

$$
P(T=t\mid A_{n,s})
=\binom{m}{t}
\frac{B(a+s+t,\;b+n-s+m-t)}{B(a+s,\;b+n-s)},
\qquad t=0,\ldots,m.
$$

Here $B(\cdot,\cdot)$ is the beta function. These quantities are computed analytically, without Monte Carlo simulation. The concentration parameter $\tau$ controls the initial beta specification and does not automatically represent a literal number of previous observations.

## Main functions

| Function | Purpose |
|---|---|
| `bruno()` | Classify a declared event and compute the conditional and predictive quantities. |
| `predict()` | Obtain the beta-binomial distribution for a future recurrence count. |
| `summary()` | Report observed recurrence information and conditional or predictive intervals. |
| `print()` | Display principal results from a fitted object. |

The package includes `bruno_example`, a constructed dataset illustrating event classification, missing observations and user-defined predictive classes.

## Example 1: Events defined from pre–post measurements

```r
library(bruno)

dat <- data.frame(
  id = paste0("P", 1:6),
  score_pre = c(2, 3, 4, 2, 5, 3),
  score_post = c(1, 3, 5, 2, 3, 4)
)

fit <- bruno(
  data = dat,
  id = id,
  pre = score_pre,
  post = score_post,
  event = post <= pre,
  mu0 = 0.50,
  tau = 10,
  future_n = 5
)

print(fit)
summary(fit, level = 0.95)
predict(fit)
```

The event `post <= pre` is defined solely for illustration; the package does not assign it an interpretation such as improvement or intervention benefit. The declared event occurs in four of the six cases. With $\mu_0=0.5$ and $\tau=10$, the next-case predictive probability is

$$
P(E_7=1\mid A_{6,4})=\frac{5+4}{10+6}=0.5625.
$$

## Example 2: Directly specified events

```r
event_status <- c(1, 1, 0, 1, 1, 0)

fit_direct <- bruno(
  id = paste0("P", seq_along(event_status)),
  event = event_status,
  mu0 = 0.50,
  tau = 10
)

summary(fit_direct)
```

The event must be defined before prediction. Logical vectors and numeric vectors containing 0 and 1 (with optional missing values) are accepted.

## Example 3: Future sample and predictive classes

```r
predict(fit, future_n = 10)

data(bruno_example)

fit_grouped <- bruno(
  data = bruno_example,
  id = id,
  pre = pre,
  post = post,
  event = post <= pre,
  mu0 = 0.50,
  tau = 10,
  group = group,
  future_n = 5
)

summary(fit_grouped)
predict(fit_grouped)
```

Prediction is performed separately within each defined class. The user may provide common or class-specific initial probabilities and concentration parameters. Class membership alone does not establish exchangeability.

## Missing observations and audit information

Cases for which the declared event cannot be determined are excluded without imputation. Grouped analyses also exclude cases with missing class membership from class-specific calculations. Case-level inclusion information remains available:

```r
fit_grouped$events
fit_grouped$n_total
fit_grouped$n_analyzed
fit_grouped$n_excluded
```

## Probability intervals

`summary()` reports a central interval for the conditional beta assessment. For future samples larger than one case, it also reports an interval for the predictive recurrence count. These intervals concern different quantities; discreteness may make the probability contained in a predictive-count interval greater than the requested reporting level.

```r
summary(fit, level = 0.95)
summary(fit, level = 0.90)
```

## Interpretation and limitations

- **Event definition:** the user specifies the event and its substantive interpretation.
- **Exchangeability:** the assumption concerns predictive relevance between observed and future cases; `bruno` does not estimate or test it.
- **Beta specification:** this is an additional parametric modelling assumption. Predictions depend on $\mu_0$ and $\tau$.
- **Predictive classes:** a class definition requires substantive justification; labels alone do not establish exchangeability.
- **Missing observations:** indeterminate events and missing class membership are not imputed.
- **Inferential scope:** event recurrence probabilities do not independently establish latent change, intervention efficacy, causal effects, measurement validity or a research decision.

## Reproducibility

The conditional and predictive calculations are analytical and do not require a random seed. Report the event definition, initial probability, concentration parameter, predictive-class definition, future sample size and software version.

```r
packageVersion("bruno")
sessionInfo()
```

## Documentation

```r
help("bruno", package = "bruno")
help("bruno_example", package = "bruno")
help("predict.bruno_fit", package = "bruno")
help("summary.bruno_fit", package = "bruno")
```

Reference manual: https://cran.r-project.org/package=bruno

## Reference

de Finetti, B. (1937). La prévision: Ses lois logiques, ses sources subjectives. *Annales de l'Institut Henri Poincaré, 7*(1), 1–68.

## Author

**Author and maintainer:** Irene Gianeselli.

## Licence

GPL-3.
