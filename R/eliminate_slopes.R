eliminate_slopes = function(segments, x, ale_values, epsilon, breaks){
  if(nrow(segments) == 1) return(segments)
  # order of slopes by increasing absolute slope

  # can happen that segment slope is NA because only one point is in there
  segments$slope[is.na(segments$slope)] = 0

  x_interval = cut(x, breaks = breaks, include.lowest = TRUE)
  dat = data.frame(x, interval = x_interval)

  pr = function(segs) {
    mx = merge(data.table(dat), segs, by.x = "interval", by.y = "interval", sort = FALSE)
    mx$intercept + mx$slope * mx$x
  }

  slope_order = order(abs(segments$slope))
  for (i in slope_order) {
    segments_new = segments
    segments_new[i, "slope"] = 0
    new_intercept = mean(ale_values[dat$interval == segments_new$interval[i]])
    segments_new[i, "intercept"] = new_intercept
    stopifnot(!any(is.na(pr(segments_new))))
    stopifnot(!any(is.na( ale_values)))
    if (get_r2(pr(segments_new), ale.values = ale_values) < epsilon) {
      segments = segments_new
    }
  }
  segments
}
