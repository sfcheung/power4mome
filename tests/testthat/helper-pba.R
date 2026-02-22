# ==== Diagnostic function ====

diag <- function(a_out) {
  (x_tmp <- ceiling(q_dfun(a_out$dfun_out, prob = .50)))
  (x_lo <- q_dfun(a_out$dfun_out, .05))
  (x_hi <- q_dfun(a_out$dfun_out, .95))
  parold <- par(no.readonly = TRUE)
  layout(matrix(1:6, nrow = 3, byrow = TRUE))
  plot(a_out$fit_1)
  abline(h = .80, col = "blue", lwd = 1)
  abline(h = a_out$f_power, col = "red", lwd = 1)
  abline(v = x_tmp, col = "red", lwd = 1)
  abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
  plot(a_out$x_history, type = "l")
  abline(h = x_tmp, col = "blue", lwd = 1)
  abline(h = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
  plot(a_out$f_history, type = "l")
  abline(h = 0, col = "blue", lwd = 1)
  tmp <- c(q_dfun(a_out$dfun_out, .01),
           q_dfun(a_out$dfun_out, .99))
  plot(a_out$dfun_out, type = "l", xlim = tmp)
  abline(v = x_tmp, col = "red", lwd = 1)
  abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
  plot(a_out$fit_1,
      xlim = c(x_lo * .9, x_hi * 1.1))
  abline(h = .80, col = "blue", lwd = 1)
  abline(h = a_out$f_power, col = "red", lwd = 1)
  abline(v = x_tmp, col = "red", lwd = 1)
  abline(v = c(x_lo, x_hi), col = "black", lwd = 1, lty = "dotted")
  hdr_power_history <- a_out$hdr_power_history
  tmp <- sapply(unlist(hdr_power_history, recursive = FALSE),
                range)
  plot(seq_along(hdr_power_history),
       y = rep(a_out$f_power, length(hdr_power_history)),
       ylim = range(tmp),
       type = "l")
  # points(a_out$f_power + a_out$f_history)
  points(a_out$reject_by_power_curve_history)
  for (i in seq_along(hdr_power_history)) {
    for (y in hdr_power_history[[i]]) {
      arrows(
          x0 = i,
          y0 = y[1],
          x1 = i,
          y1 = y[2],
          lty = "dotted",
          code = 3,
          angle = 90,
          length = .05
        )
    }
  }
  par(parold)
  hdr_h <- a_out$hdr_power_history
  tmp <- sapply(hdr_h,
            \(x) ifelse(length(x) == 1,
                        diff(x[[1]]),
                        NA)
            )
  tmp2 <- sapply(hdr_h,
            \(x) ifelse(length(x) == 1,
                        (x[[1]][1] <= a_out$f_power) &
                        (x[[1]][1] >= a_out$f_power),
                        NA)
            )
  print(tmp <= a_out$hdr_power_tol)
}