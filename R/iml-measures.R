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
    eps2 = NULL,
    relevant_features = NULL,
    # How well the main effects model approximates total model
    r2 = NULL,
    base_feat_cost = NULL,
    max_feat_cost = NULL,
    complexity_total = NULL,
    complexity_wtotal = NULL,
    complexity_wtotal2 = NULL,
    complexity_wavg = NULL,
    complexity_wavg2 = NULL,
    complexity_avg = NULL,
    approx_models = NULL,
    ia_shap = NULL,
    initialize = function(predictor, grid.size = 50, parallel = FALSE,
      epsilon = 0.05, base_feat_cost = 0, max_feat_cost = 10, eps2 = 0.05) {
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
      self$eps2 = eps2
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
    predict_approx = function(dat, features = NULL){
      if(is.null(features)) features = self$features
      dat2 = data.frame(self$predictor$data$get.x())
      mean_pred = mean(self$predictor$predict(dat2)[[1]])
      res = data.frame(lapply(self$approx_models[features], function(mod) {
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
    # SST of black box model
    SST = NULL,
    SSE = NULL,
    # Named list of approximation models
    measure_var = function(){
      if(is.null(private$multiClass) || !private$multiClass) {
        dat = data.frame(self$predictor$data$get.x())
        predictions = self$predictor$predict(dat)[[1]]
        ale_predictions = self$predict(dat)
        private$SST = ssq(predictions - mean(predictions))
        if(private$SST == 0) {
          self$r2 = 1
        } else {
          private$SSE = ssq(ale_predictions - predictions)
          self$r2 = 1 - private$SSE/private$SST
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
      self$complexity_avg = mean(unlist(lapply(self$approx_models, function(x) x$n_coefs)))
      # Compute Shapley weights based on r2
      dat = self$predictor$data$get.x()
      mean_pred = mean(self$predictor$predict(dat)[[1]])
      SSM = ssq(self$predict(data.frame(dat)) - mean_pred)

      shapleyv = shapley(self, dat, mean_pred,m = 10000, sst = SSM)
      shapleyv = shapleyv / sum(shapleyv)

      self$ia_shap = shapleyv[length(shapleyv)]
      # keep only for features
      shapleyv = shapleyv[1:(length(shapleyv) - 1)]
      # percent_var = shapleyv
      # names(percent_var) = self$features
      # percent_var = cumsum(sort(percent_var, decreasing = FALSE))
      # self$relevant_features =  names(percent_var)[percent_var > self$eps2]

      for(i in seq_along(shapleyv)) self$approx_models[[i]]$shapley_var = shapleyv[i]
      self$complexity_wtotal = 1 + sum(length(self$features) * shapleyv * unlist(lapply(self$approx_models, function(x) x$n_coefs)))
      self$complexity_wavg = weighted.mean(unlist(lapply(self$approx_models, function(x) x$n_coefs)), w = shapleyv)
      self$complexity_wavg2 = weighted.mean(unlist(lapply(self$approx_models, function(x) x$n_coefs)), w = unlist(lapply(self$approx_models, function(x) x$var)))
      self$complexity_wtotal2 = 1 + length(self$features) * self$complexity_wavg2

    },
    generatePlot = function(features = NULL, ncols = NULL, nrows = NULL, fixed_y = TRUE, ...) {
      assert_character(features, null.ok = TRUE)
      if(length(features) > 0) {
        assert_true(all(features %in% self$features))
      } else {
        features = self$features
      }

      # Compute size of gtable
      layout = get_layout(length(features), nrows, ncols)

      # Based on layout, infer which figures will be left and or bottom
      del_ylab_index = setdiff(1:length(features), 1:min(layout$nrows, length(features)))


      if(fixed_y) {
        res = unlist(lapply(features, function(fname){
          cname = ifelse(self$method == "ale", ".ale", ".y.hat")
          values = self$effects[[fname]]$results[cname]
          c(min(values), max(values))
        }))
        ylim = c(min(res), max(res))
      } else {
        ylim = c(NA, NA)
      }
      plts = lapply(features, function(fname) {
        gg = self$approx_models[[fname]]$plot(..., ylim = ylim) +
          theme(axis.title.y=element_blank())
        ggplotGrob(gg)
      })

      y_axis_label = self$effects[[1]]$.__enclos_env__$private$y_axis_label
      # Fill gtable with graphics
      ml = marrangeGrob(grobs = plts, nrow = layout$nrows, ncol = layout$ncols,
        top = NULL, left = y_axis_label)
      # For graphics not on left side, remove y-axis names and x-axis names
      # return grid
      ml
    }

  )
)

# TODO: upweight the sampled rows by subtracting the 0 and 1 rows weights from global weight
shapley = function(effects, dat, mean_pred, m, sst) {
  p = length(effects$effects)
  ales = data.frame(lapply(effects$effects, function(mod) {
    mod$predict(data.frame(dat))
  }))
  # The Interation Effects
  ales = cbind(ales, data.frame(IA = effects$predictor$predict(dat) - effects$predict(data.frame(dat))))
  # Each column is an ALE feature, each row an observation
  model = function(newdata) {
    res = apply(newdata, 1, function(x) {
      1 - ssq(rowSums(ales[,which(x==1), drop = FALSE]))/sst
    })
    res
  }
  perms = matrix(sample(c(1,0), size = (p + 1) * m, replace = TRUE), nrow = m)
  perms = rbind(rep(0, times = p+1), rep(1, times = p+1), perms)
  shaps = lapply(1:(p+1), function(feature) {
    # create 1,0 matrix from perm with j
    pperms = perms
    pperms[,feature] = 0
    pperms = unique(pperms)
    pperms2 = pperms
    pperms2[,feature] = 1
    # create second matrix from wihtout j
    # get model() and difference
    diffs = model(pperms2) - model(pperms)
    # compute shapley weighting for differences
    S = rowSums(pperms)
    # weights for #feature=1 and #feature=p
    mean1 = mean(diffs[1:2])
    mean2 = weighted.mean(diffs[3:length(diffs)],
                          w = 1/p * 1/choose(p-1,S[3:length(diffs)]))
    weighted.mean(c(mean1,mean2), w = c(2/p, 1-2/p))
  })
  unlist(shaps)
}




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
    var = NULL,
    shapley_var = NULL,
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
      self$var = self$SST / length(private$x)
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
          control = list(maxit = 100), self$SST)
        pars = opt_gensa$par
        if(opt_gensa$value <= self$epsilon)  break()
      }
      if(opt_gensa$value > self$epsilon)  self$max_complex = TRUE
      # Create table for predictions
      df$lvl = cut(df$ale, breaks = c(min(df$ale), pars, max(df$ale)), include.lowest = TRUE)
      df_pred = df[,.(pred_approx = weighted.mean(ale, w = n)),by = lvl]
      self$tab = merge(df, df_pred, by.x = "lvl", by.y = "lvl")
    },
    plot = function(ylim = c(NA,NA)) {
      dat = self$ale$predictor$data$get.x()
      dat = unique(data.frame(x = dat[[self$feature]], y = private$approx_values))
      max_string = ifelse(self$max_complex, "+", "")
      self$ale$plot(ylim = ylim) + geom_point(aes(x = x, y = y), data = dat, color = "red", size = 2) +
        ggtitle(sprintf("C: %i%s, e: %.3f, R2: %.3f, Sh: %.3f", self$n_coefs, max_string, self$epsilon,
          self$r2, self$shapley_var))
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
          control = list(maxit = 100), self$SST,
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
    plot = function(ylim = c(NA, NA)) {
      fdat = self$ale$predictor$data$get.x()[[self$feature]]
      x = seq(from = min(fdat), to = max(fdat), length.out = 200)
      y = self$predict(x)
      intervals = cut(x, breaks = self$breaks)
      dat = data.frame(x = x, y = y, interval = intervals)
      max_string = ifelse(self$max_complex, "+", "")
      p = self$ale$plot(ylim = ylim) +
        geom_line(aes(x = x, y = y, group = interval), color = "red",
        data = dat, lty = 2) +
        ggtitle(sprintf("C: %i%s, e: %.3f, R2: %.3f, Sh: %.3f", self$n_coefs, max_string, self$epsilon,
          self$r2, self$shapley_var))
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

get_layout = function(n_features, nrows = NULL, ncols = NULL) {
  assert_integerish(n_features, lower = 1, null.ok = FALSE, any.missing = FALSE)
  assert_integerish(ncols, lower = 1, null.ok = TRUE, len = 1, any.missing = FALSE)
  assert_integerish(nrows, lower = 1, null.ok = TRUE, len = 1, all.missing = FALSE)

  # Get the size of the gtable
  if(is.null(nrows) & is.null(ncols)) {
    ncols = 3
    nrows = ceiling(n_features/ ncols)
  } else {
    if(is.null(nrows)) {
      nrows = ceiling(n_features / ncols)
    }
    if(is.null(ncols)) {
      ncols = ceiling(n_features / nrows)
    }
  }
  list("nrows" = nrows, "ncols" = ncols)
}
