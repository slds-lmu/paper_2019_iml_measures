#' Draw sparkline for ALE or ALEApprox
#'
#' TODO: Create version for categorical features
#'
#' @param obj Either a AleNumApprox or a FeatureEffect
#' @param ylim The y limits, can be set to NA
#' @param ... Further arguments for ltxsparklines::sparline
spark  = function(obj, ylim = c(NA, NA), ...){
  assert_numeric(ylim, len = 2, null.ok = TRUE)
  assert_multi_class(obj, c("AleNumApprox", "FeatureEffect"))

  if(inherits(obj, "AleApprox")) {
    ale = obj$ale
    xspikes = obj$breaks
    ymax = ifelse(is.na(ylim[2]), max(ale$results$.ale), ylim[2])
    yspikes = rep(max(ale$results$.ale), times = length(obj$breaks))
  } else {
    ale = obj
    xspikes = NULL
    yspikes = NULL
  }
  feature = ale$feature.name
  ymax = ifelse(is.na(ylim[2]), max(ale$results$.ale), ylim[2])
  ymin = ifelse(is.na(ylim[1]), min(ale$results$.ale), ylim[1])
  sparkline(x = ale$results[,feature], y = ale$results$.ale,
    xspikes = xspikes, yspikes = yspikes, ylim = c(ymin, ymax),
    startdotcolor = "red", enddotcolor = "red",...)
}
