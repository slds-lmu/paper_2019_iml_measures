weighted.var <- function(x, w = NULL, na.rm = FALSE) {
  assert_numeric(x)
  assert_numeric(w)
  assert_logical(na.rm)
  if (is.null(w)) {
    w = rep(1, times = length(x))
  }
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  mean.w <- sum(x * w) / sum(w)
  (1 / (sum.w - 1)) * sum(w * (x - mean.w)^2, na.rm =
      na.rm)
}
