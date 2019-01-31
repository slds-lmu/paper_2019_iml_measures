# TODO
# Improve plotting to stop at break points
# Write tests
# Add some comments

# TODO when paper is done
# Document FunComplexity
# Profile code to see where bottleneck is

FunComplexity = R6::R6Class(
  "FunComplexity",
  inherit = iml::FeatureEffects,
  public = list(
    epsilon = NULL,
    var_explained = NULL,
    base_feat_cost = NULL,
    max_feat_cost = NULL,
    complexity_total = NULL,
    approx_models = NULL,
    initialize = function(predictor, grid.size = 50, parallel = FALSE,
      epsilon = 0.05, base_feat_cost = 0, max_feat_cost = 10) {
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
      dat2 = data.frame(self$predictor$data$get.x())
      predictions = self$predictor$predict(dat2)[[1]]
      rowSums(res) + mean(predictions)
    },
    predict_approx = function(dat){
      dat2 = data.frame(self$predictor$data$get.x())
      mean_pred = mean(self$predictor$predict(dat2)[[1]])
      res = data.frame(lapply(self$approx_models, function(mod) {
        mod$predict(dat)
      }))
      rowSums(res) + mean_pred
    },
    compute = function(epsilon) {
      self$epsilon = epsilon
      private$measure_var()
      private$measure_non_linearities()
    },
    plot_complexity = function(feature) {
      self$approx_models[[feature]]$plot()
    }

  ),
  private = list(
    # Named list of approximation models
    measure_var = function(){
      if(is.null(private$multiClass) || !private$multiClass) {
        dat = data.frame(self$predictor$data$get.x())
        predictions = self$predictor$predict(dat)[[1]]
        ale_predictions = self$predict(dat)
        SST = ssq(predictions - mean(predictions))
        if(SST == 0) {
          self$var_explained = 1
        } else {
          SSE = ssq(ale_predictions - predictions)
          self$var_explained = 1 - SSE/SST
        }
      }
    },
    measure_non_linearities = function(){
      self$approx_models = lapply(self$effects, function(eff) {
        feature_name = eff$feature.name
        if(eff$feature.type == "numerical") {
          AleNumApprox$new(ale = eff, epsilon = self$epsilon, max_feat_cost = self$max_feat_cost)
        } else {
          AleCatApprox$new(ale = eff, epsilon = self$epsilon, max_feat_cost = self$max_feat_cost)
        }
      })
      self$complexity_total = 1 + sum(unlist(lapply(self$approx_models, function(x) x$n_coefs)))
    }
  )
)


AleApprox = R6::R6Class("AleApprox",
  public = list(
    # ALE to be approximated
    ale = NULL,
    # R-squared
    r2 = NULL,
    # Number of coefficients
    n_coefs = NULL,
    max_breaks = NULL,
    feature = NULL,
    epsilon = NULL,
    transform = NULL,
    predict = NULL,
    SST = NULL,
    max_complex = FALSE,
    initialize = function(ale, epsilon, max_breaks){
      assert_class(ale, "FeatureEffect")
      assert_numeric(epsilon, lower = 0, upper = 1, len = 1, any.missing = FALSE)
      assert_numeric(max_breaks, len = 1)
      self$ale = ale
      self$epsilon = epsilon
      self$max_breaks = max_breaks
      self$feature = ale$feature.name
      private$x = self$ale$predictor$data$get.x()[,self$feature, with=FALSE][[1]]
      private$ale_values = self$ale$predict(private$x)
      self$SST = ssq(private$ale_values)
    }
  ),
  private = list(
    is_null_ale = function() {
      if(all(self$ale$results$.ale == 0)) {
        self$r2 = 1
        self$n_coefs = 0
        self$transform = function(X)  data.frame()
        self$predict = function(X) {
          times = ifelse(is.data.frame(X), nrow(X), length(X))
          rep(0, times = times)
        }
        private$approx_values = rep(0, times = self$ale$predictor$data$n.rows)
        self$max_complex = FALSE
        TRUE
      } else {
        FALSE
      }
    },
    x = NULL,
    ale_values = NULL,
    approx_values = NULL
  )
)

AleCatApprox = R6::R6Class(classname = "AleCatApprox",
  inherit = AleApprox,
  public = list(
    # Table holding the level/new_level info
    tab = NULL,
    initialize = function(ale, epsilon, max_feat_cost) {
      assert_true(ale$feature.type == "categorical")
      super$initialize(ale, epsilon, max_breaks = max_feat_cost)
      if(!private$is_null_ale()) {
        self$approximate()
        self$n_coefs = length(unique(self$tab$lvl)) - 1
        self$transform = function(dat){
          newdat = merge(dat, self$tab, by.x = self$feature,
            by.y = "x", sort = FALSE)[, c("lvl", "ale")]
          model.matrix(ale ~ factor(lvl) - 1, data = newdat)
        }

        self$predict = function(dat){
          merge(dat, self$tab, by.x = self$feature, by.y = "x", sort = FALSE)[["pred_approx"]]
        }

        private$approx_values = self$predict(self$ale$predictor$data$get.x())
        SSE = ssq(private$approx_values -  private$ale_values)
        self$r2 = 1 - SSE / self$SST
      }
    },
    approximate = function(){
      x = private$x
      # Create table with x, ale, n
      df = data.table(ale =  private$ale_values, x = x)
      df = df[,.(n = .N), by = list(ale, x)]
      max_breaks = min(self$max_breaks, length(unique(df$ale)))
      for(n_breaks in 1:max_breaks) {
        lower = rep(min(df$ale), times = n_breaks)
        upper = rep(max(df$ale), times = n_breaks)
        init_breaks = quantile(df$ale, seq(from = 0, to = 1, length.out = n_breaks + 2))[2:(n_breaks +1)]
        opt_gensa  = GenSA(par = init_breaks, step_fn, lower, upper, dat = df,
          control = list(maxit = 100, threshold.stop = self$epsilon), self$SST)
        pars = opt_gensa$par
        if(opt_gensa$value <= self$epsilon)  break()
      }
      if(opt_gensa$value > self$epsilon)  self$max_complex = TRUE
      # Create table for predictions
      df$lvl = cut(df$ale, breaks = c(min(df$ale), pars, max(df$ale)), include.lowest = TRUE)
      df_pred = df[,.(pred_approx = weighted.mean(ale, w = n)),by = lvl]
      self$tab = merge(df, df_pred, by.x = "lvl", by.y = "lvl")
    },
    plot = function() {
      dat = self$ale$predictor$data$get.x()
      dat = unique(data.frame(x = dat[[self$feature]], y = private$approx_values))
      max_string = ifelse(self$max_complex, "+", "")
      self$ale$plot() + geom_point(aes(x = x, y = y), data = dat, color = "red", size = 2)+
        ggtitle(sprintf("C: %i%s, eps: %.4f, R2: %.4f", self$n_coefs, max_string, self$epsilon,
          self$r2))
    }
  )
)

step_fn = function(par, dat, SST){
  breaks = unique(c(min(dat$ale), par, max(dat$ale)))
  dat$lvl = cut(dat$ale, breaks = breaks, include.lowest = TRUE)
  dat2 = dat[,.(ale_mean = weighted.mean(ale, w = n), n = sum(n)),by = lvl]
  # ALE plots have mean zero
  SSM = sum((dat2$ale_mean)^2 * dat2$n)
  1 - (SSM/SST)
}



AleNumApprox = R6::R6Class(classname = "AleNumApprox",
  inherit = AleApprox,
  public = list(
    # Table holding the level/new_level info
    model = NULL,
    breaks = NULL,
    initialize = function(ale, epsilon, max_feat_cost) {
      assert_true(ale$feature.type == "numerical")
      max_breaks = floor(max_feat_cost / 2) - 1
      super$initialize(ale, epsilon, max_breaks)

      if(!private$is_null_ale()) {

        self$approximate()
        # Don't count the intercept
        self$n_coefs = length(coef(self$model)) - 1

        # TODO: Implement using transform and lm.fit and multiplication with coefs
        self$predict = function(dat) {
          if(is.data.frame(dat)) {
            x = dat[[self$feature]]
          } else {
            x = dat
          }
          x_interval = cut(x, breaks = self$breaks, include.lowest = TRUE)
          dat = data.frame(x = x, interval = x_interval)
          predict(self$model, newdata = dat)
        }

        self$transform = function(dat, intercept = TRUE){
          x = ifelse(is.data.frame(dat), dat[,self$feature], dat)
          if(is.null(self$breaks)) return(dat)
          x_interval = cut(x, breaks = self$breaks, include.lowest = TRUE)
          dat = data.frame(x = x, interval = x_interval)
          if(intercept) {
            model.matrix(x ~ interval, data = dat)
          } else {
            model.matrix(x ~ interval - 1, data = dat)
          }
        }
        private$approx_values = self$predict(self$ale$predictor$data$get.x())
        SSE = ssq(private$approx_values -  private$ale_values)
        self$r2 = 1 - SSE / self$SST
      }
    },
    approximate = function(){
      x = private$x
      # test 0 breaks
      mod = lm(private$ale_values ~ x)
      SSE = ssq(private$ale_values - predict(mod))
      if(self$SST == 0 || (SSE/self$SST) < self$epsilon) {
        self$r2 = get_r2(predict(mod), private$ale_values)
        private$approx_values = predict(mod)
        self$model = mod
        return()
      }
      for(n_breaks in 1:self$max_breaks) {
        lower = rep(min(x), times = n_breaks)
        upper = rep(max(x), times = n_breaks)
        init_breaks = quantile(x, seq(from = 0, to = 1, length.out = n_breaks + 2))[2:(n_breaks +1)]
        opt_gensa = GenSA(par = init_breaks, segment_fn, lower, upper, ale = self$ale,
          control = list(maxit = 100, threshold.stop = self$epsilon), self$SST,
          x = x, ale_prediction = private$ale_values)
        pars = opt_gensa$par
        if(opt_gensa$value <= self$epsilon)  break()
      }
      if(opt_gensa$value > self$epsilon)  self$max_complex = TRUE
      # fit lm with par as cut points
      self$breaks = unique(c(min(x), pars, max(x)))
      x_interval = cut(x, breaks = self$breaks, include.lowest = TRUE)
      dat = data.frame(x = x, interval = x_interval, ale = private$ale_values)
      self$model = lm(ale ~ x * interval, data = dat)
      private$approx_values = predict(mod)
      self$r2 = get_r2(private$approx_values, private$ale_values)
    },
    plot = function() {
      fdat = self$ale$predictor$data$get.x()[[self$feature]]
      x = seq(from = min(fdat), to = max(fdat), length.out = 200)
      y = self$predict(x)
      intervals = cut(x, breaks = self$breaks)
      dat = data.frame(x = x, y = y, interval = intervals)
      max_string = ifelse(self$max_complex, "+", "")
      p = self$ale$plot() + geom_line(aes(x = x, y = y, group = interval), color = "red",
        data = dat, lty = 2) +
        ggtitle(sprintf("C: %i%s, eps: %.4f, R2: %.4f", self$n_coefs, max_string, self$epsilon,
          self$r2))
      if(!is.null(self$breaks)) p = p + geom_vline(data = data.frame(breaks = self$breaks), aes(xintercept = self$breaks))
      p

    }

  )
)


#' Function to optimize for ALE approx
#'
#' @param par The breakpoints
segment_fn = function(par, ale, SST, x, ale_prediction){
  x_interval = cut(x, breaks = unique(c(min(x), par, max(x))), include.lowest = TRUE)
  dat = data.table(xv = x, interval = x_interval, alev = ale_prediction)
  res = dat[, .(ssq(.lm.fit(cbind(rep.int(1, times = length(xv)),xv),alev)$residuals)), by = interval]
  sum(res$V1)/SST
}



ssq = function(x) {
  assert_numeric(x, any.missing = FALSE, min.len = 1)
  sum(x^2)
}

get_r2 = function(seg.predictions, ale.values) {
  SST = ssq(ale.values)
  if(SST == 0) { return(FALSE)}
  SSE = ssq(seg.predictions - ale.values)
  SSE / SST
}


