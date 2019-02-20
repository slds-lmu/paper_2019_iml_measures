# TODO
# Write tests
# Add some comments

# TODO when paper is done
# Document FunComplexity
# Profile code to see where bottleneck is

FunComplexity = R6::R6Class(
  "FunComplexity",
  inherit = iml::FeatureEffects,
  public = list(
    # user defined max. segments approx error
    epsilon = NULL,
    # The maximum complexity a feature can have
    max_c = NULL,
    # How well the main effects model approximates total model
    r2 = NULL,
    # The mean complexity per ALE plot, weighted by its variance
    c_wmean = NULL,
    # Number of features used in predictor
    n_features = NULL,
    # The approximation models of the ALE plots
    approx_models = NULL,
    initialize = function(predictor, grid.size = 50, parallel = FALSE,
      epsilon = 0.05, max_c = 10) {
      if(predictor$task == "classification" & is.null(predictor$class)) {
        stop("Please set class in Predictor")
      }
      assert_numeric(epsilon, lower = 0, upper = 1, any.missing = FALSE, len = 1)
      assert_numeric(max_c, len = 1, any.missing = FALSE, upper = grid.size)
      self$max_c = max_c
      self$epsilon = epsilon
      super$initialize(predictor, features = predictor$data$feature.names,
        method = "ale", grid.size = grid.size, center.at = NULL,
        parallel = parallel)
      private$X = data.frame(self$predictor$data$get.x())
      private$mean_pred = mean(self$predictor$predict(private$X)[[1]])
      private$measure_r2_1st_ale()
      private$measure_non_linearities()
      self$n_features = sum(unlist(lapply(self$approx_models, function(x) x$feature_used)))
    },
    # 1st-order ALE model predictions
    predict = function(dat) {
      res = data.frame(lapply(self$effects, function(eff) {
        eff$predict(dat)
      }))
      rowSums(res) + private$mean_pred
    },
    # 1st-order ALE approximiation model predictions
    predict_approx = function(dat, features = NULL){
      if(is.null(features)) features = self$features
      res = data.frame(lapply(self$approx_models[features], function(mod) {
        mod$predict(dat)
      }))
      rowSums(res) + private$mean_pred
    },
    plot_complexity = function(feature) {
      self$approx_models[[feature]]$plot()
    }

  ),
  private = list(
    # Feature matrix
    X = NULL,
    # SST of black box model
    SST = NULL,
    # SSE of 1st-order ALE model
    SSE = NULL,
    # The mean prediction of the black box predictor
    mean_pred = NULL,
    measure_r2_1st_ale = function(){
      if(is.null(private$multiClass) || !private$multiClass) {
        predictions = self$predictor$predict(private$X)[[1]]
        ale_predictions = self$predict(private$X)
        private$SST = ssq(predictions - private$mean_pred)
        if(private$SST == 0) {
          self$r2 = 1
        } else {
          private$SSE = ssq(ale_predictions - predictions)
          self$r2 = 1 - private$SSE/private$SST
        }
      } else {
        stop("Does not work for multiClass")
      }
    },
    measure_non_linearities = function(){
      self$approx_models = lapply(self$effects, function(eff) {
        feature_name = eff$feature.name
        if(eff$feature.type == "numerical") {
          AleNumApprox$new(ale = eff, epsilon = self$epsilon, max_c = self$max_c)
        } else {
          AleCatApprox$new(ale = eff, epsilon = self$epsilon, max_c = self$max_c)
        }
      })
      # Compute Shapley weights based on r2
      SSM = ssq(self$predict(private$X) - private$mean_pred)
      am_coefs = unlist(lapply(self$approx_models, function(x) x$n_coefs))
      am_weights = unlist(lapply(self$approx_models, function(x) x$var))
      self$c_wmean = weighted.mean(am_coefs, w = am_weights)
      if(all(am_coefs == 0)) self$c_wmean = 0
    },
    generatePlot = function(features = NULL, ncols = NULL, nrows = NULL, fixed_y = TRUE, del_zero=TRUE,...) {
      assert_character(features, null.ok = TRUE)
      if(length(features) > 0) {
        assert_true(all(features %in% self$features))
      } else {
        features = self$features
      }

      if(del_zero){
        features = features[sapply(self$approx_models, function(x) x$feature_used)]
      }

      # Compute size of gtable
      layout = iml:::get_layout(length(features), nrows, ncols)
      # Based on layout, infer which figures will be left and or bottom
      del_ylab_index = setdiff(1:length(features), 1:min(layout$nrows, length(features)))

      if(fixed_y) {
        res = unlist(lapply(features, function(fname){
          cname = ifelse(self$method == "ale", ".ale", ".y.hat")
          values = self$effects[[fname]]$results[cname][[1]]
          values = c(values, self$approx_models[[fname]]$approx_values)
          c(min(values), max(values))
        }))
        ylim = c(min(res), max(res))
      } else {
        ylim = c(NA, NA)
      }
      maxv = max(unlist(lapply(self$approx_models, function(x) x$var)))
      plts = lapply(features, function(fname) {
        gg = self$approx_models[[fname]]$plot(..., ylim = ylim, maxv = maxv) +
          theme(axis.title.y=element_blank())
        ggplotGrob(gg)
      })
      y_axis_label = self$effects[[1]]$.__enclos_env__$private$y_axis_label
      # Fill gtable with graphics
      ml = marrangeGrob(grobs = plts, nrow = layout$nrows, ncol = layout$ncols,
        left = y_axis_label, top = sprintf("ALE main effects, R squared %.2f", self$r2))
      ml
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
    # The maximum number of breaks allowed
    max_breaks = NULL,
    # Name of the feature
    feature = NULL,
    # Maximal allowed approximation error
    epsilon = NULL,
    # prediction function
    predict = NULL,
    # SST of ALE model
    SST_ale = NULL,
    var = NULL,
    shapley_var = NULL,
    max_complex = FALSE,
    feature_used = TRUE,
    approx_values = NULL,
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
      self$SST_ale = ssq(private$ale_values)
      # Variance of the ALE plot weighted by data density
      self$var = self$SST_ale / length(private$x)
    }
  ),
  private = list(
    x = NULL,
    ale_values = NULL,
    is_null_ale = function() {
      if(!feature_used(self$ale$predictor, self$feature)) {
        self$r2 = 1
        self$n_coefs = 0
        self$predict = function(X) {
          times = ifelse(is.data.frame(X), nrow(X), length(X))
          rep(0, times = times)
        }
        self$feature_used = FALSE
        self$approx_values = rep(0, times = self$ale$predictor$data$n.rows)
        self$max_complex = FALSE
        TRUE
      } else {
        FALSE
      }
    }
  )
)

AleCatApprox = R6::R6Class(classname = "AleCatApprox",
  inherit = AleApprox,
  public = list(
    # Table holding the level/new_level info
    tab = NULL,
    initialize = function(ale, epsilon, max_c) {
      assert_true(ale$feature.type == "categorical")
      super$initialize(ale, epsilon, max_breaks = max_c)
      if(!private$is_null_ale()) {
        self$approximate()
        self$n_coefs = ifelse(self$max_complex, max_c, length(unique(self$tab$lvl)) - 1)
        self$predict = function(dat){
          merge(dat, self$tab, by.x = self$feature, by.y = "x", sort = FALSE)[["pred_approx"]]
        }
        self$approx_values = self$predict(self$ale$predictor$data$get.x())
        SSE = ssq(self$approx_values -  private$ale_values)
        self$r2 = 1 - SSE / self$SST_ale
      }
    },
    approximate = function(){
      x = private$x
      # Create table with x, ale, n
      df = data.table(ale =  private$ale_values, x = x)
      df = df[,.(n = .N), by = list(ale, x)]
      df$x = factor(df$x, self$ale$results[,self$feature])
      df = df[order(df$x),]
      max_breaks = min(self$max_breaks, nlevels(x) - 1)
      for(n_breaks in 1:max_breaks) {
        lower = rep(1, times = n_breaks) - 0.00001
        upper = rep(nlevels(x), times = n_breaks) + 0.000001
        # currently ordered factor break optimization handled on continuous scale
        init_breaks = seq(from = 0, to = nlevels(x), length.out = n_breaks+2)[2:(n_breaks+1)]
       if(n_breaks == nlevels(x)) {
          pars = init_breaks
          break()
        }
        opt_gensa  = GenSA(par = init_breaks, step_fn, lower, upper, dat = df,
          control = list(maxit = 100), self$SST_ale)
        pars = opt_gensa$par
        if(opt_gensa$value <= self$epsilon)  break()
      }
      if(opt_gensa$value > self$epsilon)  self$max_complex = TRUE
      # Create table for predictions
      breaks = unique(round(pars, 0))
      df$lvl = cut(1:nrow(df), c(0, breaks, nrow(df)))
      df_pred = df[,.(pred_approx = weighted.mean(ale, w = n)),by = lvl]
      self$tab = merge(df, df_pred, by.x = "lvl", by.y = "lvl")
    },
    plot = function(ylim = c(NA,NA), maxv = NULL) {
      assert_numeric(maxv, null.ok=TRUE)
      dat = self$ale$predictor$data$get.x()
      dat = unique(data.frame(x = dat[[self$feature]], y = self$approx_values))
      max_string = ifelse(self$max_complex, "+", "")
      varv = ifelse(is.null(maxv), self$var, self$var/maxv)
      self$ale$plot(ylim = ylim) + geom_point(aes(x = x, y = y), data = dat, color = "red", size = 2) +
        ggtitle(sprintf("C: %i%s, R2: %.3f, V: %.3f", self$n_coefs, max_string, self$r2, varv))
    }
  )
)

step_fn = function(par, dat, SST){
  breaks = unique(round(par, 0))
  dat$lvl = cut(1:nrow(dat), unique(c(0, breaks, nrow(dat))))
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
    initialize = function(ale, epsilon, max_c) {
      assert_true(ale$feature.type == "numerical")
      max_breaks = floor(max_c / 2) - 1
      super$initialize(ale, epsilon, max_breaks)
      if(!private$is_null_ale()) {
        self$approximate()
        # TODO: Create function count_coefs with alpha as param and tests
        # Don't count the intercept
        cfs = summary(self$model)$coefficients[-1,, drop = FALSE]
        self$n_coefs = ifelse(self$max_complex, max_c, nrow(cfs))
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
        self$approx_values = self$predict(self$ale$predictor$data$get.x())
        SSE = ssq(self$approx_values -  private$ale_values)
        self$r2 = 1 - SSE / self$SST_ale
      }
    },
    approximate = function(){
      x = private$x
      # test 0 breaks
      mod = lm(private$ale_values ~ x)
      SSE = ssq(private$ale_values - predict(mod))
      if(self$SST_ale == 0 || (SSE/self$SST_ale) < self$epsilon) {
        self$r2 = get_r2(predict(mod), private$ale_values)
        self$approx_values = predict(mod)
        self$model = mod
        return()
      }
      for(n_breaks in 1:self$max_breaks) {
        lower = as.numeric(rep(min(x), times = n_breaks))
        upper = as.numeric(rep(max(x), times = n_breaks))
        init_breaks = quantile(x, seq(from = 0, to = 1, length.out = n_breaks + 2))[2:(n_breaks +1)]
        opt_gensa = GenSA(par = init_breaks, segment_fn, lower, upper, ale = self$ale,
          control = list(maxit = 100), self$SST_ale,
          x = x, ale_prediction = private$ale_values)
        pars = opt_gensa$par
        if(opt_gensa$value <= self$epsilon)  break()
      }
      if(opt_gensa$value > self$epsilon)  self$max_complex = TRUE
      # fit lm with par as cut points
      self$breaks = unique(c(min(x), pars, max(x)))
      x_interval = cut(x, breaks = self$breaks, include.lowest = TRUE)
      dat = data.frame(x = x, interval = x_interval, ale = private$ale_values)
      # Try step model first
      model = lm(ale ~ interval, dat = dat)
      if(get_r2(predict(model), private$ale_values) <= self$epsilon) {
        self$model = model
      } else {
        self$model = lm(ale ~ x * interval, data = dat)
      }
      self$approx_values = predict(self$model)
      self$r2 = get_r2(self$approx_values, private$ale_values)
    },
    plot = function(ylim = c(NA, NA), maxv = NULL) {
      assert_numeric(maxv, null.ok=TRUE)
      fdat = self$ale$predictor$data$get.x()[[self$feature]]
      x = seq(from = min(fdat), to = max(fdat), length.out = 200)
      y = self$predict(x)
      intervals = cut(x, breaks = self$breaks)
      dat = data.frame(x = x, y = y, interval = intervals)
      max_string = ifelse(self$max_complex, "+", "")
      varv = ifelse(is.null(maxv), self$var, self$var/maxv)
      p = self$ale$plot(ylim = ylim) +
        geom_line(aes(x = x, y = y, group = interval), color = "red",
          data = dat, lty = 2) +
        ggtitle(sprintf("C: %i%s, R2: %.3f, V: %.3f", self$n_coefs, max_string,self$r2, varv))
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
  # TODO: First check whether step function would be enough
  dat2 = dat[,.(ale_mean = mean(alev), n = .N), by = interval]
  # ALE plots have mean zero
  SSM = sum((dat2$ale_mean)^2 * dat2$n)
  r2step = 1 - (SSM/SST)
  res = dat[, .(ssq(.lm.fit(cbind(rep.int(1, times = length(xv)),xv),alev)$residuals)), by = interval]
  r2segment = sum(res$V1)/SST
  if(r2step < r2segment) print("step was better")
  return(min(r2step, r2segment))
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


feature_used = function(pred, feature, M = 3, sample_size = 100){
  dat = pred$data$get.x()
  for (m in 1:M) {
    # permute feature
    dat2 = dat[sample(1:nrow(dat), size = sample_size)]
    prediction1 = pred$predict(dat2)
    fvalues = sample(dat2[,..feature][[1]])
    dat2 = dat2[, (feature) := fvalues]
    prediction2 = pred$predict(dat2)
    if(any((prediction1 - prediction2) != 0)) return(TRUE)
  }
  FALSE
}
