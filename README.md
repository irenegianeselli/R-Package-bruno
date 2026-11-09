
# bruno

**Predicting User-Defined Event Recurrence under Exchangeability**

`bruno` (Beta–Bernoulli Recurrence for Unobserved Next Outcomes) is an R package for the analytical prediction of user-defined binary event recurrence under an explicit judgement of exchangeability.

The package assigns predictive probabilities to the occurrence of a specified event in a future case and computes the exact beta-binomial predictive distribution of event counts in a prespecified future sample.

Events may be supplied as binary indicators or defined from paired pre–post measurements. Predictions can be calculated for a single predictive class or separately across user-defined classes.

**Version:** 0.1.0

**CRAN:** https://cran.r-project.org/package=bruno

## Installation

Install the released package from CRAN:

```r
install.packages("bruno")
library(bruno)
```

The package requires R version 4.1.0 or later.

## Statistical framework

Let \(E_1, E_2, \ldots\) denote binary indicators of a user-defined event.

The predictive interpretation requires a substantive judgement that the observed and future event indicators are exchangeable within the intended predictive class.

For an indefinitely extendible exchangeable binary sequence, de Finetti's representation theorem permits the assigned joint probabilities to be expressed as a mixture of Bernoulli distributions.

`bruno` adopts a beta distribution as an additional parametric specification of the mixing distribution. This beta specification is not implied by exchangeability alone.

The user supplies an initial probability \(\mu_0\), where \(0 < \mu_0 < 1\), and a positive concentration parameter \(\tau\). The corresponding beta parameters are

\[
a = \mu_0 \tau,
\qquad
b = (1-\mu_0)\tau.
\]

Suppose the specified event occurs \(s\) times among \(n\) analysable cases. Conditioning on the observed recurrence information gives

\[
\theta \mid A_{n,s}
\sim
\operatorname{Beta}(a+s,\;b+n-s),
\]

where \(A_{n,s}\) denotes the observed information that exactly \(s\) of the \(n\) events occurred.

The predictive probability of the same event occurring in the next exchangeable case is

\[
P(E_{n+1}=1\mid A_{n,s})
=
\frac{a+s}{a+b+n}
=
\frac{\mu_0\tau+s}{\tau+n}.
\]

For a prespecified future sample containing \(m\) cases, the number of event occurrences \(T\) follows the beta-binomial predictive distribution

\[
P(T=t\mid A_{n,s})
=
\binom{m}{t}
\frac{B(a+s+t,\;b+n-s+m-t)}
     {B(a+s,\;b+n-s)},
\quad t=0,\ldots,m,
\]

where \(B(\cdot,\cdot)\) is the beta function.

These predictive quantities are calculated analytically, without Monte Carlo simulation.

The concentration parameter \(\tau\) determines the concentration of the initial beta assessment. It does not automatically represent a literal number of previous observations.

## Main functions

| Function | Description |
|---|---|
| `bruno()` | Classifies the declared event and computes conditional and predictive quantities. |
| `predict()` | Returns the beta-binomial predictive distribution for a specified future sample size. |
| `summary()` | Reports observed recurrence information and conditional and predictive probability intervals. |
| `print()` | Displays the principal quantities stored in a fitted object. |

The package also includes `bruno_example`, a constructed dataset for illustrating event classification, missing-data handling, and prediction across classes.

## Example 1: Event recurrence from paired measurements

Consider six cases with paired measurements. For illustration, the event is defined as a post-measurement value no greater than its corresponding pre-measurement value.

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
  tau = 10
)

print(fit)
summary(fit)
```

The event definition is supplied by the analyst. The package does not assign an interpretation such as improvement or deterioration to the direction of change.

In this example, the declared event occurs in four of the six cases. With \(\mu_0=0.50\) and \(\tau=10\), the initial beta parameters are \(a=b=5\).

The next-case predictive probability is

\[
P(E_7=1\mid A_{6,4})
=
\frac{5+4}{10+6}
=
0.5625.
\]

This probability concerns recurrence of the specified event under the stated predictive assumptions.

## Example 2: Directly specified binary events

The event can also be supplied directly as a logical or binary vector.

```r
event_status <- c(1, 1, 0, 1, 1, 0)
participant <- paste0("P", 1:6)

fit_direct <- bruno(
  id = participant,
  event = event_status,
  mu0 = 0.50,
  tau = 10
)

print(fit_direct)
summary(fit_direct)
```

The event must be defined before prediction. A logical vector or a numeric vector containing only 0 and 1, apart from missing values, is accepted.

## Example 3: Predicting recurrence in a future sample

The `future_n` argument specifies the number of future cases.

```r
fit_future <- bruno(
  data = dat,
  id = id,
  pre = score_pre,
  post = score_post,
  event = post <= pre,
  mu0 = 0.50,
  tau = 10,
  future_n = 5
)

predict(fit_future)
summary(fit_future)
```

The resulting predictive distribution assigns a probability to each possible number of event occurrences, from zero to five.

The future sample size can also be changed without repeating the initial event classification:

```r
predict(fit_future, future_n = 10)
```

The predictive distribution accounts for uncertainty represented by the conditional beta assessment. It is not a binomial distribution with the next-case predictive probability treated as a fixed, known event probability.

## Example 4: Prediction across user-defined classes

`bruno` can perform separate predictions for classes defined by the analyst.

```r
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

print(fit_grouped)
summary(fit_grouped)
predict(fit_grouped)
```

The included dataset contains constructed measurements and two predictive classes, `A` and `B`.

Predictions are conditioned separately on the event recurrence information within each class. The initial probability and concentration parameter may be specified as common values or as named, class-specific numeric vectors.

Class membership does not establish exchangeability. The predictive relevance of observed cases for future cases must be justified within each class.

## Missing observations and case-level audit

Cases for which the declared event cannot be determined are excluded from the recurrence calculation without imputation.

In grouped analyses, cases with missing predictive-class membership are also excluded from class-specific calculations.

The fitted object retains case-level event classifications and inclusion information, allowing users to identify which observations contributed to the predictive analysis.

```r
fit_grouped$events
fit_grouped$n_total
fit_grouped$n_analyzed
fit_grouped$n_excluded
```

The observed recurrence frequency is calculated using analysable cases only.

## Probability intervals

The `summary()` method reports central probability intervals for the conditional beta assessment.

When the future sample contains more than one case, it also reports predictive intervals for the number of future event occurrences.

```r
summary(fit_future, level = 0.95)
summary(fit_future, level = 0.90)
```

The two interval types have different interpretations:

- The conditional beta interval describes the distribution of the mixing probability under the specified model.
- The predictive interval concerns the number of occurrences of the declared event in a future sample.

Because future recurrence counts are discrete, the probability contained within a predictive interval may exceed its nominal reporting level.

## Interpretation and limitations

`bruno` is intended for psychological, educational, pilot-study, and research decision-making applications involving recurrence of an explicitly defined binary event.

The interpretation of its results depends on several conditions.

1. **Event specification.** The event must be explicitly defined by the analyst. Its substantive meaning is not inferred from the measurements.

2. **Exchangeability.** Observed and future events must be regarded as exchangeable within the intended predictive class. The package does not estimate or test this assumption.

3. **Beta specification.** The beta mixing distribution is a parametric assumption additional to exchangeability. Predictive results depend on the initial probability and concentration parameter supplied by the user.

4. **Predictive classes.** Group-specific prediction requires a defensible definition of the predictive classes. Group labels alone do not establish the required predictive relationship.

5. **Missing observations.** Indeterminate events and missing class membership are excluded without imputation.

6. **Scope of inference.** Recurrence probabilities do not independently establish latent change, intervention efficacy, causal effects, measurement validity, or a research decision.

The predicted quantity is the recurrence of the declared event within the specified predictive class and under the adopted probability model.

## Reproducibility

The principal conditional and predictive calculations are analytical and do not require a random seed.

For reproducible reporting, users should document the event definition, predictive classes, initial probability, concentration parameter, future sample size, and package version.

```r
packageVersion("bruno")
sessionInfo()
```

## Documentation

Function-level documentation is available in R:

```r
help("bruno", package = "bruno")
help("bruno_example", package = "bruno")
help("predict.bruno_fit", package = "bruno")
help("summary.bruno_fit", package = "bruno")
```

The package reference manual is available on [CRAN](https://cran.r-project.org/package=bruno).

## Reference

de Finetti, B. (1937). La prévision: Ses lois logiques, ses sources subjectives. *Annales de l'Institut Henri Poincaré, 7*(1), 1–68.

## Author

Irene Gianeselli.

**Maintainer:** Irene Gianeselli.

## Licence

GPL-3.
