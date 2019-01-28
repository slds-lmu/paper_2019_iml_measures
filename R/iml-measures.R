FunComplexity = R6::R6Class(
  "FunComplexity",
  inherit = iml::FeatureEffects,
  public = list(
    epsilon = NULL,
    var_explained = NULL,
    SST = NULL,
    SSE = NULL,
    SSM = NULL,
    base_feat_cost = NULL,
    max_feat_cost = NULL,
    complexity_total = NULL,
    complexities = list(),
    initialize = function(predictor, grid.size = 50, parallel = FALSE,
      epsilon = 0.05, base_feat_cost = 1, max_feat_cost = 10) {
      if(predictor$task == "classification" & is.null(predictor$class)) {
        stop("Please set class in Predictor")
      }
      assert_numeric(epsilon, lower = 0, upper = 1, any.missing = FALSE, len = 1)
      assert_numeric(base_feat_cost, len = 1, any.missing = FALSE)
      assert_numeric(max_feat_cost, len = 1, any.missing = FALSE, upper = grid.size)
      self$base_feat_cost = base_feat_cost
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
    },
    plot_complexity = function(feature) {
      assert_choice(feature, self$predictor$data$feature.names)
      mod = private$approx_models[[feature]]
      ale = self$effects[[feature]]
      complexity = self$complexities[[feature]]
      feature_values = self$predictor$data$get.x()[[feature]]
      plot_complexity_(feature_values, mod, ale, complexity, self$epsilon)
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
      private$approx_models = lapply(self$effects, function(eff) {
        feature_name = eff$feature.name
        if(eff$feature.type == "numerical") {
          max_breaks = (self$max_feat_cost / 2) + 1
          res = optim_approx(eff, max_breaks = max_breaks, epsilon = self$epsilon)
          cc = min(self$max_feat_cost, length(coef(res$mod)))
          mod = res$mod
        } else {
          mod = approx_ale_cat(eff, self$max_feat_cost, self$epsilon)
          cc = width(mod)
        }

        self$complexities[feature_name] = cc
        mod
      })
      self$complexity_total  = sum(unlist(self$complexities))
    }
  )
)


# =============================================================================
# Number of linear segments in segmented linear regression
# =============================================================================
plot_complexity_ = function(feature_values, mod, ale, complexity, epsilon) {
  feature = ale$feature.name
  mod_values = predict(mod)
  dat = data.frame(x = feature_values, y = mod_values)

  p = ale$plot()
  if(is.numeric(feature_values)) {
    p  = p + geom_line(aes(x = x, y = y), color = "red",
      data = dat, lty = 2)
  } else {
    dat = unique(dat)
    p = p + geom_point(aes(x = x, y = y), data = dat)
  }
  r_squared = 1 - get_r2(mod, ale.values = ale$predict(feature_values))
  p + ggtitle(sprintf("C: %i, eps: %.4f, Rsquared: %.4f", complexity, epsilon,
    r_squared))
}



# Measures the complexity of a model
count_pieces = function(mod){
  cs = summary(mod)
  sum(cs$coefficients[, "Pr(>|t|)"] < 1)
}

get_r2 = function(mod, ale.values) {
  seg.predictions = predict(mod)
  SST = var(ale.values)
  if(SST == 0) { return(FALSE)}
  SSE = var(seg.predictions - ale.values)
  SSE / SST
}

check_r2 = function(mod, ale.values, epsilon){
  get_r2(mod, ale.values) < epsilon
}


#' Function to optimize for ALE approx
#'
#' @param par The breakpoints
fn = function(par, ale, SST, x, ale_prediction){
  x_interval = cut(x, breaks = c(min(x), par, max(x)), include.lowest = TRUE)
  dat = data.table(xv = x, interval = x_interval, alev = ale_prediction)
  # TODO: Increase speed by using fit.lm or using matrix algebra or Rcpp
  res = dat[, .(sum(.lm.fit(cbind(rep.int(1, times = length(xv)),xv),alev)$residuals^2)), by = interval]
  sum(res$V1)/SST
}

optim_approx = function(ale, epsilon, max_breaks) {
  fname = ale$feature.name
  x = as.numeric(ale$predictor$data$get.x()[,fname, with=FALSE][[1]])
  ale_prediction = ale$predict(x)

  # test 0 breaks
  mod = lm(ale_prediction ~ x)
  if(check_r2(mod, ale_prediction, epsilon)) {
    return(list(mod = mod, r2 = get_r2(mod, ale_prediction)))
  }
  SSE = 1
  for(i in 1:max_breaks) {
    opt_gensa = optim_approx_k(ale, i, epsilon)
    pars = unique(opt_gensa$par)
    if(opt_gensa$value < epsilon)  break()
  }
  # fit lm with par as cut points
  fname = ale$feature.name
  intervals_point = c(min(x), pars, max(x))
  x_interval = cut(x, breaks = intervals_point, include.lowest = TRUE)
  dat = data.frame(x = x, interval = x_interval, ale = ale_prediction )
  mod = lm(ale ~ x * interval, data = dat)
  list(mod = mod, r2 = get_r2(mod, ale_prediction))
}

optim_approx_k = function(ale, n_breaks,  epsilon){
  fname = ale$feature.name
  x = ale$predictor$data$get.x()[,fname, with=FALSE][[1]]
  lower = rep(min(x), times = n_breaks)
  upper = rep(max(x), times = n_breaks)
  SST = var(ale$predict(x)) * length(x)
  par = NULL
  init_breaks = quantile(x, seq(from = 0, to = 1, length.out = n_breaks + 2))[2:(n_breaks +1)]
  GenSA(par = init_breaks, fn, lower, upper, ale = ale,
    control = list(maxit = 100, threshold.stop = epsilon), SST,
    x = x, ale_prediction = ale$predict(x))
}

approx_ale_cat = function(ale, max_feat_cost, epsilon){
  fname = ale$feature.name
  x = ale$predictor$data$get.x()[,fname, with=FALSE][[1]]
  ale_prediction = ale$predict(x)
  # Fully grow tree
  mod = partykit::ctree(ale_prediction ~ x, control = ctree_control(alpha = 0.5, minbucket = 1))
  # prune back until r2 bigger than 0.95
  while(check_r2(mod, ale_prediction, epsilon)) {
    mod_best = mod
    mod = prune_tree_1n(mod)
  }
  mod_best
}
