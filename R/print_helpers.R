#' @noRd
header_str <- function(x,
                       hw = .8,
                       sym = "=",
                       min = 2,
                       prefix = character(0),
                       suffix = character(0)) {
  full_w <- options("width")$width
  hw1 <- floor(full_w * hw)
  x_len <- nchar(x)
  a <- floor((hw1 - x_len - 2) / 2)
  a <- max(min, a)
  out <- paste0(prefix,
                strrep(sym, a),
                " ", x, " ",
                strrep(sym, a),
                suffix)
  out
}
