#' @noRd
header_str <- function(x,
                       hw = .8,
                       sym = "=",
                       min = 2,
                       prefix = character(0),
                       suffix = character(0),
                       sep_i = " ") {
  full_w <- options("width")$width
  hw1 <- floor(full_w * hw)
  x_len <- nchar(x)
  a <- floor((hw1 - x_len - 2) / 2)
  a <- max(min, a)
  out <- paste0(prefix,
                strrep(sym, a),
                sep_i, x, sep_i,
                strrep(sym, a),
                suffix)
  out
}
