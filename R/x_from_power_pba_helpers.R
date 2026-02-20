#' @noRd
# Find the approximate chance to have
# a sample power "beyond" the target power
# if the population power is power_i,
# given the "sample size" (trial_nrep).
p_c0 <- function(
  target_power,
  power_i,
  trial_nrep
) {
  if (target_power == power_i) {
    return(.50)
  }
  f <- function(level) {
    a <- reject_ci_wilson(
            nreject = trial_nrep * power_i,
            nvalid = trial_nrep,
            level = level)[, ifelse(power_i < target_power, "cihi", "cilo")]
    a - target_power
  }
  tmp1 <- stats::uniroot(
          f,
          interval = c(0.0000001, .999999)
        )
  level_out <- tmp1$root
  p_c <- 1 - (1 - level_out) / 2
  p_c
}

#' @noRd
# Find the approximate chance to have
# a sample power "beyond" the target power
# if the population power is power_i,
# given the "sample size" (trial_nrep).
p_c <- function(
  target_power,
  power_i,
  goal,
  what,
  trial_nrep,
  tolerance = .001,
  level = .95,
  final_nrep
) {
  if (what == "point") {
    out <- p_c0(
        target_power = target_power,
        power_i = power_i,
        trial_nrep = trial_nrep
      )
    return(out)
  }
  target_power_b <- target_power_adjusted(
      target_power = target_power,
      goal = goal,
      what = what,
      tolerance = tolerance,
      nrep = final_nrep,
      level = level
    )
  out <- p_c0(
      target_power = target_power_b,
      power_i = power_i,
      trial_nrep = trial_nrep
    )
  out
}

#' @noRd
# Form a high-density interval(s)
hdi <- function(
  dfun,
  prob = .80
) {
  x <- dfun[, "x", drop = TRUE]
  pbi <- dfun[, "prob", drop = TRUE]
  po <- order(pbi)
  for (i in po) {
    pbi <- pbi - pbi[i]
    if (sum(pbi) <= prob) break
  }
  a <- pbi > 0
  # TODO:
  # - There should be an easier way to do this
  d0 <- vector("numeric", length = length(a))
  for (i in seq_along(a)) {
    if (i == 1) {
      d0[i] <- 1
    } else {
      if (isTRUE(xor(a[i], a[i - 1]))) {
        d0[i] <- d0[i - 1] + 1
      } else {
        d0[i] <- d0[i - 1]
      }
    }
  }
  h1 <- tapply(
          x,
          INDEX = d0,
          FUN = range
        )
  h2 <- tapply(
          a,
          INDEX = d0,
          FUN = \(x) isTRUE(x[1])
        )
  out <- unname(h1[h2])
  out
}
