lmtree2 = function (formula, data, subset, na.action, weights, offset,
  cluster, ...)
{
  control <- mob_control(...)
  cl <- match.call(expand.dots = TRUE)
  f <- Formula::Formula(formula)
  if (length(f)[2L] == 1L) {
    attr(f, "rhs") <- c(list(1), attr(f, "rhs"))
    formula[[3L]] <- formula(f)[[3L]]
  }
  else {
    f <- NULL
  }
  m <- match.call(expand.dots = FALSE)
  if (!is.null(f))
    m$formula <- formula
  m$fit <- partykit:::lmfit
  m$control <- control
  if ("..." %in% names(m))
    m[["..."]] <- NULL
  m[[1L]] <- as.call(quote(partykit::mob))
  rval <- eval(m, parent.frame())
  rval$info$call <- cl
  class(rval) <- c("lmtree", class(rval))
  return(rval)
}
