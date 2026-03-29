# k-Bit Recurrence Model (kBRM)

## Overview

The k-Bit Recurrence Model (kBRM) is a probabilistic framework for forecasting binary payment sequences — recurring payments that are either made (1) or missed (0) in each period.

The model encodes the k most recent payment outcomes as a binary string (the history state), and conditions the probability of the next payment on this k-bit history. For example, with k = 2, the four possible states are `11` (paid, paid), `10` (paid, missed), `01` (missed, paid), and `00` (missed, missed), each with its own estimated payment probability.

The framework generalises the binomial tree, which conditions only on the most recent outcome (k = 1), to any history length k ≥ 2. It was originally developed for forecasting recovery streams on non-performing debt portfolios, where three months of payment history was identified as the most informative predictor of future behaviour.

---

## Key Features

**Full distributional output.** Unlike a standard binomial tree or Markov chain model, the kBRM propagates the full joint distribution of cumulative payment counts and history states at every forecast horizon. This means expected values, variances, confidence intervals, and quantiles are all available as direct analytical outputs — no simulation or bootstrapping is required.

**Linear state-space complexity.** A naive binary tree grows exponentially with the forecast horizon at O(2^t). The kBRM reduces this to O(2^k) by collapsing all paths that share the same current k-bit state and cumulative payment count. For fixed k this is a constant, irrespective of how far ahead the forecast extends.

**Absorbing barrier for fixed-term products.** An optional maturity parameter T caps the cumulative payment count, so the algorithm correctly models loans or mortgages that terminate after T payments. After maturity, the state space is bounded by a constant 2^k × (T + 1).

**Cyclostationary extension.** The parameter table can include a monthly index (mth_num), allowing the payment probabilities to vary by calendar month. For a 12-month cycle this uses 12 × 2^k parameters, accommodating seasonal payment patterns and macroeconomic cycles with no change to the core algorithm.

**Structural connection to the Hopfield network.** At k = 3, the eight binary states {111, 110, 101, 100, 011, 010, 001, 000} are in exact bijection with the vertices of the three-dimensional binary hypercube — the state space of a three-unit Hopfield network. Both frameworks share a recurrent binary update structure that converges to attractor states. This structural parallel provides theoretical grounding for the practitioner intuition that three months of payment history is the most informative window.

---

## Algorithm Structure

The implementation uses four functions with clean separation of concerns:

| Function | Role |
|---|---|
| `paidOrNotNext()` | Single-step propagation — takes the current distribution and returns the next step |
| `paidOrNotPaid()` | Full propagation for one starting state over all forecast steps |
| `getPymtPercTbl()` | Runs all starting states; returns the complete joint distribution table |
| `getModelDeclineCurve()` | Post-processing: derives expected payments and the decline curve from the distribution table |

The key operation is inside `paidOrNotNext()`: after branching into paid and not-paid paths, rows sharing the same `(code, payNum)` tuple are collapsed by summing their probabilities. This is what keeps the state space O(2^k) rather than O(2^t).

---

## Complexity

| Version | State space per step | Total work over n steps |
|---|---|---|
| Naive binary tree | O(2^t) — exponential | O(2^n) |
| kBRM (open-ended) | O(2^k) — constant in t | O(2^k · n) |
| kBRM (with maturity T) | O(2^k · T) — constant after t > T | O(2^k · n · T) |

---

## Input Format

The parameter table `payCodeTbl` requires columns `code`, `paidPerc`, and `notPaidPerc`. For the cyclostationary extension, add a `mth_num` column.

The helper function `getCodeTbl()` converts raw count data (code, total observations, paid count) into this format, with `paidPerc = output / cnt` as the maximum likelihood estimate of P(payment | state).

```r
x <- data.frame(
  code   = c("11", "10", "01", "00"),
  cnt    = c(1000, 1000, 1000, 1000),
  output = c( 900,  500,  300,   50)
)
y <- getCodeTbl(x)
```

---

## Quick Start

```r
library(dplyr)
source("main.R")

# Build parameter table
x <- data.frame(
  code   = c("11", "10", "01", "00"),
  cnt    = c(1000, 1000, 1000, 1000),
  output = c( 900,  500,  300,   50)
)
y <- getCodeTbl(x)

# Full distributional output over 12 months, maturity T = 24
dist <- getPymtPercTbl(y, numMth = 12, maxPay = 24)

# Decline curve
getModelDeclineCurve(dist)

# 95% confidence interval at each step
marginal <- dist %>%
  group_by(startCode, numStep, payNum) %>%
  summarise(prob = sum(prob)) %>%
  group_by(startCode, numStep) %>%
  arrange(payNum) %>%
  mutate(cum_prob = cumsum(prob))

marginal %>%
  summarise(
    lower_95 = min(payNum[cum_prob >= 0.025]),
    upper_95 = min(payNum[cum_prob >= 0.975])
  )
```

---

## History

- **2010–2011** — Original framework developed in Excel/VB6 for forecasting recovery streams on non-performing debt portfolios (k = 3).
- **2021** — Generalised to arbitrary k ≥ 2 in R; cyclostationary extensions added.
- **2025–2026** — Add absorbing barrier.

---

## Dependencies

- R (≥ 3.5)
- dplyr (Wickham et al., 2019)

---
