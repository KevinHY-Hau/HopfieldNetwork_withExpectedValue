# Kevin Hon Yin Hau
# k-Bit Recurrence Model (kBRM) -- R Implementation
# Includes: absorbing barrier (maxPay), cyclostationary extension (mth_num),
#           full joint distribution output, and decline curve calculation.

library(dplyr)
options(dplyr.summarise.inform = FALSE)


## ---- paidOrNotNext ---------------------------------------------------
## Single-step propagation. Takes the current-step distribution table and
## the parameter table for this step, returns the next-step distribution.
## By operating on the current step only (not full history), memory stays
## O(2^k * min(t, T)) rather than growing quadratically with t.
## payCodeTbl is passed separately because it can change at each step
## (cyclostationary case) or remain fixed (static case).

paidOrNotNext <- function(tbl, payCodeTbl, maxPay = 999) {
  
  lenCode <- nchar(tbl$code[1])
  
  ## Extract the most recent step and join payment probabilities.
  ## Shift the state code left by one position (drop oldest bit).
  tbl_last <- tbl %>%
    filter(numStep == max(tbl$numStep)) %>%
    inner_join(payCodeTbl, c("code" = "code")) %>%
    mutate(code = substr(code, 2, lenCode))
  
  out <- tbl %>%
    union_all(
      ## Paid branch: append '1', increment payNum (capped at maxPay),
      ##              multiply probability by p(s).
      tbl_last %>%
        mutate(code   = paste(code, "1", sep = ""),
               payNum = if_else(payNum >= maxPay, maxPay, payNum + 1L),
               prob   = prob * paidPerc) %>%
        union_all(
          ## Not-paid branch: append '0', payNum unchanged,
          ##                  multiply probability by q(s) = 1 - p(s).
          tbl_last %>%
            mutate(code = paste(code, "0", sep = ""),
                   prob = prob * notPaidPerc)
        ) %>%
        mutate(numStep = numStep + 1L) %>%
        select(-paidPerc, -notPaidPerc)
    )
  
  ## Collapse: sum probabilities over identical (numStep, code, payNum) tuples.
  ## This is the key operation that keeps the state space O(2^k) rather
  ## than O(2^t) -- many paths share the same state and payment count.
  out %>%
    group_by(numStep, code, payNum) %>%
    summarise(prob = sum(prob)) %>%
    ungroup()
}


## ---- paidOrNotPaid ---------------------------------------------------
## Full propagation for one starting state over numMth steps.
## Handles both static (no mth_num column) and cyclostationary
## (mth_num column, cycle length = max(mth_num)) parameter tables.
## Returns the joint distribution (startCode, numStep, payNum, prob)
## for all steps 1 to numMth.

paidOrNotPaid <- function(startCode, payCodeTbl, numMth,
                          start_mth_num = 1, maxPay = 999) {
  
  chk_cycle <- any(names(payCodeTbl) == "mth_num")
  
  ## Pre-select columns for static case (avoids repeated select inside loop).
  if (!chk_cycle)
    pay_code <- payCodeTbl %>% select(code, paidPerc, notPaidPerc)
  
  ## Initialise: step 0, starting state, zero payments, probability = 1.
  tbl <- data.frame(numStep = 0L, code = startCode, payNum = 0L, prob = 1)
  
  for (i in seq_len(numMth)) {
    
    ## Cyclostationary: look up the parameter set for the current calendar
    ## month. The modular index maps any forecast horizon back to the
    ## correct month in the cycle.
    if (chk_cycle) {
      mth_idx  <- ((start_mth_num + i) %% max(payCodeTbl$mth_num)) + 1L
      pay_code <- payCodeTbl %>%
        filter(mth_num == mth_idx) %>%
        select(code, paidPerc, notPaidPerc)
    }
    
    tbl <- paidOrNotNext(tbl, pay_code, maxPay)
  }
  
  ## Remove the step-0 initialisation row and add the startCode label.
  tbl %>%
    filter(numStep > 0) %>%
    mutate(startCode = startCode) %>%
    group_by(startCode, numStep, payNum) %>%
    summarise(prob = sum(prob)) %>%
    ungroup()
}


## ---- getPymtPercTbl --------------------------------------------------
## Runs paidOrNotPaid for every unique starting state in payCodeTbl and
## combines the results. Returns the full joint distribution
## P(N_t = m, S_t = s) for all starting states, steps, and payment counts.
## This table is the foundation for all downstream analysis: expected
## value, variance, confidence intervals, and quantiles are all derived
## from it with no additional computation.

getPymtPercTbl <- function(payCodeTbl, numMth,
                           start_mth_num = 1, maxPay = 999) {
  
  unique_codes <- unique(payCodeTbl$code)
  
  result <- paidOrNotPaid(unique_codes[1], payCodeTbl,
                          numMth, start_mth_num, maxPay)
  
  for (i in seq_along(unique_codes)[-1]) {
    result <- union_all(result,
                        paidOrNotPaid(unique_codes[i], payCodeTbl,
                                      numMth, start_mth_num, maxPay))
  }
  
  result
}


## ---- getModelDeclineCurve --------------------------------------------
## Post-processing only: derives the expected cumulative payments E[N_t]
## and the marginal expected payment (decline curve) D(t) = E[N_t] - E[N_{t-1}]
## from the getPymtPercTbl() output. No further computation is needed.
## D(t) represents the probability-weighted expected payment in month t.
## Under the absorbing barrier model, D(t) -> 0 as t -> infinity.

getModelDeclineCurve <- function(pymtPercTbl) {
  
  result <- pymtPercTbl %>%
    group_by(startCode, numStep) %>%
    summarise(expectedPayt = sum(payNum * prob)) %>%
    ungroup()
  
  result %>%
    left_join(
      result %>%
        mutate(numStep = numStep + 1L, expectedPaytLast = expectedPayt) %>%
        select(startCode, numStep, expectedPaytLast),
      by = c("startCode", "numStep")
    ) %>%
    mutate(declineCurve = expectedPayt - coalesce(expectedPaytLast, 0)) %>%
    select(-expectedPaytLast)
}
