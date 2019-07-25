# Extract the slope information
extract_segments = function(model, breaks, intervals, feature.cname = "x") {
  assert_class(model, "lm")
  stopifnot(!any(duplicated(breaks)))
  cfs = coef(model)
  coef_slope = cfs[grep(feature.cname, names(cfs))]
  coef_intercept = cfs[setdiff(names(cfs), names(coef_slope))]
  if(length(cfs) == 2) {
    data.frame(
      xstart = breaks[1],
      xend = breaks[2],
      intercept = coef_intercept,
      slope = coef_slope,
      interval = intervals
    )
  } else {
    coef_slope[2:length(coef_slope)] =  coef_slope[1] + coef_slope[2:length(coef_slope)]
    coef_intercept[2:length(coef_slope)] =  coef_intercept[1] + coef_intercept[2:length(coef_intercept)]
    data.frame(
      xstart = breaks[1:(length(breaks) - 1)],
      xend = breaks[2:length(breaks)],
      intercept = coef_intercept,
      slope = coef_slope,
      interval = intervals
    )
  }
}
