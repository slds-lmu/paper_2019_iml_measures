#' Draw sparkline for ALE or ALEApprox
#'
#' TODO: Create version for categorical features
#'
#' @param obj Either a AleNumApprox or a FeatureEffect
#' @param ylim The y limits, can be set to NA
#' @param approx Should the approximation be plotted instead of real ALE curve?
#' @param ... Further arguments for ltxsparklines::sparline
spark  = function(obj, ylim = c(NA, NA), approx = FALSE, color = "black", height = 2,...){
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
  sprintf("{\\renewcommand{\\sparklineheight}{%s}\\definecolor{sparklinecolor}{named}{%s}%s}",
    height, color, sparkline_string)
}



#' Compute model summary
#'
#' @param pred Predictor
#' @param ylim the y-axis limits for the sparklines
#' @return character vector with NF, IA, AMEC and sparklines for all features
get_spark_col = function(pred, ylim = c(NA, NA), width = 5, ...) {
  assert_class(pred, "Predictor")
  assert_numeric(ylim, len = 2)
  # a bit smoother for plotting
  fc = FunComplexity$new(pred, grid.size = 50, epsilon = 0.05)
  sparklns = sapply(fc$effects, function(eff) {
    if(all(eff$results$.ale == 0)) {
      ""
    } else {
      spark(eff, width = width, ylim = ylim, ...)
    }
  })
  fc = FunComplexity$new(pred, grid.size = 150, epsilon = 0.05)
  res = c("NF" = fc$n_features,
    "IA"= sprintf("%.2f", 1 - fc$r2),
    "AMEC" = sprintf("%.2f", fc$c_wmean),
    sparklns)
  as.character(res)
}

#' Compute model summaries for subset off pareto set
#'
#' @param paretor_set the mbo pareto set
#' @param indices the subset indices for which to compute the summaries
#' @param ylim the y-axis limits for the sparklines
#' @return data.frame with NF, IA, AMEC and sparklines for all features. columns are models
get_spark_table = function(mbo_obj, indices, ylim = c(NA, NA), log_params,...) {
  assert_class(mbo_obj, "MBOMultiObjResult")
  assert_numeric(indices, any.missing = FALSE)
  assert_numeric(ylim, len = 2)
  pareto_set = mbo_obj$pareto.set
  pareto_front = mbo_obj$pareto.front
  res = lapply(indices, function(i){
    pp = pareto_set[[i]]
    pp = pp[!is.na(pp)]
    lparams = intersect(names(pp), log_params)
    if(length(lparams) > 0) pp[lparams] = lapply(pp[lparams], function(x) 2^x)
    lrn = setHyperPars(lrn.regr, par.vals = pp)
    mod = train(lrn, task)
    pred = Predictor$new(mod, task.dat)
    c(pareto_front[i, "MAE"], unlist(get_spark_col(pred, ylim = ylim, ...)))
  })
  data.frame(res)
}
