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
        SST = sum((predictions - mean(predictions))^2)
        if(SST == 0) {
          self$var_explained = 1
        } else {
          SSE = sum((ale_predictions - predictions)^2)
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
          cc = min(self$max_feat_cost, res$n_coef)
          mod = res$mod
        } else {
          res = optim_approx_cat(eff, epsilon = self$epsilon, max_breaks = self$max_feat_cost)
          cc = min(self$max_feat_cost, res$n_cat)
          mod = res$mod
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
  dat = ale$predictor$data$get.x()
  mod_values = mod(dat)
  dat = data.frame(x = dat[[feature]], y = mod_values)

  p = ale$plot()
  if(is.numeric(feature_values)) {
    p  = p + geom_line(aes(x = x, y = y), color = "red",
      data = dat, lty = 2)
  } else {
    dat = unique(dat)
    p = p + geom_point(aes(x = x, y = y), data = dat)
  }
  r_squared = 1 - get_r2(mod_values, ale.values = ale$predict(feature_values))
  p + ggtitle(sprintf("C: %i, eps: %.4f, Rsquared: %.4f", complexity, epsilon,
    r_squared))
}



# Measures the complexity of a model
count_pieces = function(mod){
  cs = summary(mod)
  sum(cs$coefficients[, "Pr(>|t|)"] < 1)
}

get_r2 = function(seg.predictions, ale.values) {
  SST = sum(ale.values^2)
  if(SST == 0) { return(FALSE)}
  SSE = sum((seg.predictions - ale.values)^2)
  SSE / SST
}

check_r2 = function(mod, ale.values, epsilon){
  seg.predictions = predict(mod)
  get_r2(seg.predictions, ale.values) < epsilon
}




optim_approx_cat = function(ale, epsilon, max_breaks) {
  fname = ale$feature.name
  x = ale$predictor$data$get.x()[,fname, with=FALSE][[1]]
  ale_prediction = ale$predict(x)

  # Create table with x, ale, n
  df = data.table(ale = ale_prediction, x = x)
  df = df[,.(n = .N), by = list(ale, x)]

  # centered at zero
  SST = sum(ale_prediction^2)
  max_breaks = min(max_breaks, length(unique(df$ale)))
  for(n_breaks in 1:max_breaks) {
    lower = rep(min(df$ale), times = n_breaks)
    upper = rep(max(df$ale), times = n_breaks)
    init_breaks = quantile(df$ale, seq(from = 0, to = 1, length.out = n_breaks + 2))[2:(n_breaks +1)]
    opt_gensa  = GenSA(par = init_breaks, step_fn, lower, upper, dat = df,
      control = list(maxit = 100, threshold.stop = epsilon), SST)
    pars = opt_gensa$par
    if(opt_gensa$value < epsilon)  break()
  }

  df$lvl = cut(df$ale, breaks = c(min(df$ale), pars, max(df$ale)), include.lowest = TRUE)
  df_pred = df[,.(pred_approx = weighted.mean(ale, w = n)),by = lvl]
  df_inter = merge(df, df_pred, by.x = "lvl", by.y = "lvl")

  pred = function(dat){
    merge(dat, df_inter, by.x = fname, by.y = "x", sort = FALSE)[["pred_approx"]]
  }
  seg.predictions = pred(ale$predictor$data$get.x())
  SSE = sum((seg.predictions - ale_prediction)^2)
  r2 = 1 - SSE / SST
  list(mod = pred, r2 = r2, n_cat = length(unique(df$lvl)))
}

step_fn = function(par, dat, SST){
  dat$lvl = cut(dat$ale, breaks = c(min(dat$ale), par, max(dat$ale)), include.lowest = TRUE)
  dat2 = dat[,.(ale_mean = weighted.mean(ale, w = n), n = sum(n)),by = lvl]
  # ALE plots have mean zero
  SSM = sum((dat2$ale_mean)^2 * dat2$n)
  1 - (SSM/SST)
}



#' Function to optimize for ALE approx
#'
#' @param par The breakpoints
segment_fn = function(par, ale, SST, x, ale_prediction){
  x_interval = cut(x, breaks = unique(c(min(x), par, max(x))), include.lowest = TRUE)
  dat = data.table(xv = x, interval = x_interval, alev = ale_prediction)
  res = dat[, .(sum(.lm.fit(cbind(rep.int(1, times = length(xv)),xv),alev)$residuals^2)), by = interval]
  sum(res$V1)/SST
}

optim_approx = function(ale, epsilon, max_breaks) {
  fname = ale$feature.name
  x = ale$predictor$data$get.x()[,fname, with=FALSE][[1]]
  ale_prediction = ale$predict(x)

  # test 0 breaks
  mod = lm(ale_prediction ~ x)
  if(check_r2(mod, ale_prediction, epsilon)) {
    seg.predictions = predict(mod)
    return(list(mod = function(x) predict(mod, newdata = x),
                r2 = get_r2(seg.predictions, ale_prediction)))
  }

  SST = sum(ale$predict(x)^2)
  for(n_breaks in 1:max_breaks) {
    lower = rep(min(x), times = n_breaks)
    upper = rep(max(x), times = n_breaks)
    init_breaks = quantile(x, seq(from = 0, to = 1, length.out = n_breaks + 2))[2:(n_breaks +1)]
    opt_gensa = GenSA(par = init_breaks, segment_fn, lower, upper, ale = ale,
      control = list(maxit = 100, threshold.stop = epsilon), SST,
      x = x, ale_prediction = ale$predict(x))
    pars = opt_gensa$par
    if(opt_gensa$value < epsilon)  break()
  }
  # fit lm with par as cut points
  intervals_point = unique(c(min(x), pars, max(x)))
  x_interval = cut(x, breaks = intervals_point, include.lowest = TRUE)
  dat = data.frame(x = x, interval = x_interval, ale = ale_prediction )
  mod = lm(ale ~ x * interval, data = dat)

  pred = function(dat) {
    x = dat[[fname]]
    x_interval = cut(x, breaks = intervals_point, include.lowest = TRUE)
    dat = data.frame(x = x, interval = x_interval)
    predict(mod, newdata = dat)
  }

  seg.predictions = predict(mod)
  list(mod = pred, r2 = get_r2(seg.predictions, ale_prediction),
       n_coef = length(coef(mod)))
}

