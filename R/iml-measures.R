FunComplexity = R6::R6Class(
  "FunComplexity",
  inherit = iml::FeatureEffects,
  public = list(
    epsilon = NULL,
    feature_cost = NULL,
    max_feat_cost = NULL,
    var_explained = NULL,
    SST = NULL,
    SSE = NULL,
    SSM = NULL,
    complexity_total = NULL,
    complexities = NULL,
    initialize = function(predictor, grid.size = 20, parallel = FALSE,
      epsilon = 0.05, feature_cost = 1, max_feat_cost = 10) {
      if(predictor$task == "classification" & is.null(pred$class)) {
        stop("Please set class in Predictor")
      }
      assert_numeric(epsilon, lower = 0, upper = 1, any.missing = FALSE, len = 1)
      assert_numeric(feature_cost, len = 1, any.missing = FALSE)
      assert_numeric(max_feat_cost, len = 1, any.missing = FALSE)
      self$feature_cost = feature_cost
      self$max_feat_cost = max_feat_cost
      super$initialize(predictor, features = predictor$data$feature.names,
        method = "ale", grid.size = grid.size, center.at = NULL,
        parallel = parallel)
      self$compute(epsilon)
    },
    predict = function(dat) {
      res = data.frame(lapply(self$effects, function(eff) {
        eff$predict(dat)
      }))
      rowSums(res)
    },
    compute = function(epsilon) {
      self$epsilon = epsilon
      private$measure_var()
      private$measure_non_linearities()
    }
  ),
  private = list(
    # Named list of approximation models
    approx_models = NULL,
    measure_var = function(){
      if(is.null(private$multiClass) || !private$multiClass) {
        dat = data.frame(self$predictor$data$get.x())
        predictions = self$predictor$predict(dat)[[1]]
        ale_predictions = self$predict(dat)
        SST = var(predictions)
        if(SST == 0) {
          self$var_explained = 1
        } else {
          SSE = var(ale_predictions - predictions)
          self$var_explained = 1 - SSE/SST
        }
      }
    },
    measure_non_linearities = function(){

    }
  )
)




# =============================================================================
# Number of linear segments in segmented linear regression
# =============================================================================

n_segs = function(pred, max_df = 20, ...){
  feature_names = pred$data$feature.names
  n_seg_values = sapply(feature_names, function(fname){
    # test both with segments and step function
    min(n_segs_feature(pred, feature.name = fname, cat.mode = FALSE, ...),
      n_segs_feature(pred, feature.name = fname, cat.mode = TRUE,  ...),
      max_df, na.rm = TRUE)
  })
  sum(n_seg_values)
}

plot_segs_feature = function(pred, feature.name, grid.size = 50, epsilon = 0.05, cat.mode = FALSE) {
  mod = fit_segs_feature(pred, feature.name, grid.size, epsilon, cat.mode)
  df = segs_complexity(pred, mod, feature.name, cat.mode)
  plot_segments(mod, pred, feature.name, pred_seg(mod), grid.size, df = df, epsilon = epsilon)
}

plot_segments = function(mod, pred, feature.name, prediction, grid.size, df, epsilon) {
  ale = FeatureEffect$new(pred, feature.name, grid.size = grid.size, method = "ale")

  feature.name = ale$feature.name
  feature = pred$data$get.x()[[feature.name]]
  dat = data.frame(x = feature, y = prediction)
  if(inherits(mod, "party")) {
    dat$node = factor(pred_seg(mod, type = "node"))
  } else {
    dat$node = 1
  }

  p = ale$plot()
  if(is.numeric(feature)) {
    p  = p + geom_line(aes(x = x, y = y, group = node), color = "red", data = dat, lty = 2)
  } else {
    dat = unique(dat)
    p = p + geom_point(aes(x = x, y = y, color = node), data = dat)
  }
  ale.fun = get_ale_function(pred, feature.name, grid.size = grid.size)
  ale.values = ale.fun(feature)
  r_squared = 1 - get_r(mod, ale.values = ale.values)
  p + ggtitle(sprintf("DF: %i, eps: %.4f, Rsquared: %.4f", df, epsilon, r_squared))
}


segs_complexity = function(pred, mod, feature.name, cat.mode){

  if(is.null(mod)) return(NA)

  # compute complexity of approximiation
  feature = pred$data$get.x()[[feature.name]]
  cost_of_feature = 5

  if(inherits(mod, "lm")) {
    # Feature is not used
    if(length(coef(mod)) == 1) {
      return(0)
    } else {
      return(cost_of_feature + 1)
    }
  }

  if(inherits(mod, "party")) {
    n_df = width(mod)
  } else {
    n_df = length(coef(mod)) - 1
  }


  if(is.numeric(feature) & !cat.mode) {
    # first segment costs 1 degrees of freedom, every following costs 2
    # because lm would also be 1 df. for each additional segment, we need to know starting point and
    # the slope, which makes two degrees of freedom
    n_df = 2 * n_df - 1
  } else {
    # first category can be seen as reference category
    # any additional category is one degree of freedom
    n_df = n_df - 1
  }
  n_df + cost_of_feature
}
# returns best fitting model
n_segs_feature = function(pred, feature.name, grid.size = 50, epsilon = 0.05, plot = FALSE, cat.mode, max_df) {
  mod = fit_segs_feature(pred, feature.name, grid.size = grid.size, epsilon = epsilon, cat.mode = cat.mode)
  segs_complexity(pred = pred, mod = mod, feature.name = feature.name,
    cat.mode = cat.mode)
}




# TODO: Enforce max.df
# TODO: Don't use all data for approx, but use grid plus weights by density
#       For this, checkout the other measures that I tried out
# TODO: increase party.alpha first, and if epsilon already ok, start pruning
# return best fitting model
fit_segs_feature = function(pred, feature.name, grid.size = 50, epsilon = 0.05, cat.mode = FALSE) {
  ale.fun = get_ale_function(pred, feature.name, grid.size = grid.size)
  feature = pred$data$get.x()[[feature.name]]
  ale.values = ale.fun(feature)

  # flat function
  if(n_distinct(ale.values) == 1) return(lm(ale.values ~ 1))

  if(is.numeric(feature)) {
    mod_best = lm(ale.values ~ feature)
    # linear model case
    if(check_r(mod_best, ale.values, epsilon)) {
      return(mod_best)
    }
  }
  if(is.numeric(feature) & !cat.mode) {
    # fully grown tree
    # mod =  partykit::lmtree(ale.values ~ feature | feature,  alpha = 0.5, minsize = 2)
    mod =  lmtree2(ale.values ~ feature | feature,  alpha = 0.5, minsize = 2, vcov = "info")
  } else {
    mod = partykit::ctree(ale.values ~ feature,  control = ctree_control(alpha = 0.5, minbucket = 1))
  }

  # Maximal depth tree couldn't find good enough approx
  if(!check_r(mod, ale.values, epsilon)) {
    warning(sprintf("couldnt find better than alpha model. width is %.2f. SSE percentage is %.3f",
      width(mod), get_r(mod, ale.values)))
    return(NULL)
  }
  while(check_r(mod, ale.values, epsilon)) {
    mod_best = mod
    mod = prune_tree_1n(mod)
  }
  mod_best
}


pred_seg = function(mod, type = "response", ...) {
  if(inherits(mod, "lmtree")) {
    predict(mod, newdata = mod$data, type = type, ...)
  } else {
    predict(mod, type = type, ...)
  }
}

get_r = function(mod, ale.values) {
  seg.predictions = pred_seg(mod)
  SST = var(ale.values)
  if(SST == 0) { return(FALSE)}
  SSE = var(seg.predictions - ale.values)
  SSE/SST
}

check_r = function(mod, ale.values, epsilon){
  get_r(mod, ale.values) < epsilon
}





# =============================================================================
# Monotonicity
# =============================================================================

n_monotone = function(pred, ...){
  ## TODO Implement

}

n_monotone_feature = function(pred, ...){
  ## TODO Implement
}


# =============================================================================
# Correlation (Predictability of features)
# =============================================================================
## TODO
# Measure all pairwise correlations (predictabilities), which yields a non-symetric matrix.

