#' Draw sparkline for ALE or ALEApprox
#'
#' TODO: Create version for categorical features
#'
#' @param obj Either a AleNumApprox or a FeatureEffect
#' @param ylim The y limits, can be set to NA
#' @param approx Should the approximation be plotted instead of real ALE curve?
#' @param ... Further arguments for ltxsparklines::sparline
spark  = function(obj, ylim = c(NA, NA), approx = FALSE, color = "black",...){
  assert_numeric(ylim, len = 2, null.ok = TRUE)
  assert_multi_class(obj, c("AleNumApprox", "FeatureEffect"))


  if(inherits(obj, "AleApprox")) {
    # removes intervals at min and max
    xspikes = obj$breaks[-c(1,length(obj$breaks))]
    ale = obj$ale
    feature = ale$feature.name
    x = ale$results[,feature]
    y = ale$results$.ale
    # min and max should remain same as ALE, even if approx is plotted
    ymin = ifelse(is.na(ylim[1]), min(ale$results$.ale), ylim[1])
    ymax = ifelse(is.na(ylim[2]), max(ale$results$.ale), ylim[2])
    if(approx) {
      N_POINTS = 50
      true.x = obj$.__enclos_env__$private$x
      x = seq(from = min(true.x), to = max(true.x), length.out = N_POINTS)
      y = obj$predict(x)
      yspikes = rep(ymax, times = length(xspikes))
      color = "gray"
    } else {
      yspikes = rep(ymax, times = length(xspikes))
    }
  } else {
    ale = obj
    feature = ale$feature.name
    x = ale$results[,feature]
    y = ale$results$.ale
    ymin = ifelse(is.na(ylim[1]), min(ale$results$.ale), ylim[1])
    ymax = ifelse(is.na(ylim[2]), max(ale$results$.ale), ylim[2])
    xspikes = NULL
    yspikes = NULL
  }
  stopifnot(!any(is.na(c(x,y))))
  sparkline_string = sparkline(x = x, y = y,
    xspikes = xspikes, yspikes = yspikes, ylim = c(ymin, ymax),...)
  sprintf("{\\definecolor{sparklinecolor}{named}{%s}%s}", color, sparkline_string)
}
