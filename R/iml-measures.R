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
        # fit step
        mod_step = fit_approx_mod(self$predictor, eff, grid.size = 50, epsilon = self$epsilon,
          type = "step")
        # fit segments
        mod_segment = fit_approx_mod(self$predictor, eff, grid.size = 50,
          epsilon = self$epsilon, type = "segment")
        res = min_model(mod_step, mod_segment, self$max_feat_cost, self$base_feat_cost,
          feature_name)
        self$complexities[feature_name] = res$c
        res$mod
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
  mod_values = predict_approx_mod(mod)
  dat = data.frame(x = feature_values, y = mod_values)

  if(inherits(mod, "party")) {
    dat$node = factor(predict_approx_mod(mod, type = "node"))
  } else {
    dat$node = 1
  }

  p = ale$plot()
  if(is.numeric(feature_values)) {
    p  = p + geom_line(aes(x = x, y = y, group = node), color = "red",
      data = dat, lty = 2)
  } else {
    dat = unique(dat)
    p = p + geom_point(aes(x = x, y = y, color = node), data = dat)
  }
  r_squared = 1 - get_r2(mod, ale.values = ale$predict(feature_values))
  p + ggtitle(sprintf("C: %i, eps: %.4f, Rsquared: %.4f", complexity, epsilon,
    r_squared))
}



min_model  = function(mod_step, mod_segment, max_feat_cost, base_feat_cost, feature_name) {
  n_steps = count_pieces(mod_step)
  n_segments = count_pieces(mod_segment)
  if(all(is.na(c(n_segments, n_steps)))) {
    cc = max_feat_cost
    mod = NULL
  } else if(any(is.na(c(n_segments, n_steps)))) {
    cc = ifelse(is.na(n_segments), n_steps, n_segments)
    if(is.na(n_segments)) {
      mod = mod_step
    } else {
      mod = mod_segment
    }
  } else {
    cc = ifelse(n_steps < n_segments, n_steps, n_segments)
    if(n_steps < n_segments) {
      mod = mod_step
    } else {
      mod = mod_segment
    }
  }
  if(is.null(mod_step) & is.null(mod_segment)) {
    warning(sprintf("Could not approximate ALE plot for %s.
      Assigning max_feat_cost of %.2f", feature_name, max_feat_cost))
    cc = max_feat_cost
  }
  if (cc != 0) {
    cc = min(cc + base_feat_cost, max_feat_cost)
  }
  list(mod = mod, c = cc)
}


# Measures the complexity of a model
count_pieces = function(mod){
  if(is.null(mod)) return(NA)
  if(inherits(mod, "lm")) {
    # Feature is not used
    if(length(coef(mod)) == 1) {conopt minos in R
      return(0)
    } else {
      return(1)
    }
  }
  if(inherits(mod, "party")) {
    n_df = width(mod)
  } else {
    n_df = length(coef(mod)) - 1
  }
  n_df
}

# TODO: Don't use all data for approx, but use grid plus weights by density
#       For this, checkout the other measures that I tried out
# TODO: increase party.alpha first, and if epsilon already ok, start pruning
# return best fitting model
fit_approx_mod = function(pred, ale, grid.size = 50, epsilon = 0.05, type) {
  assert_choice(type, c("step", "segment"))
  feature = pred$data$get.x()[[ale$feature.name]]
  if(type == "segment" & !is.numeric(feature)) return(NULL)
  ale.values = ale$predict(feature)

  # flat function
  if(length(unique(ale.values)) == 1) return(lm(ale.values ~ 1))

  if(is.numeric(feature)) {
    mod_best = lm(ale.values ~ feature)
    # linear model case
    if(check_r2(mod_best, ale.values, epsilon)) {
      return(mod_best)
    }
  }
  if(is.numeric(feature) & type == "segment") {
    # fully grown tree
    mod =  lmtree2(ale.values ~ feature | feature,  alpha = 0.5, minsize = 2, vcov = "info")
  } else {
    mod = partykit::ctree(ale.values ~ feature,  control = ctree_control(alpha = 0.5, minbucket = 1))
  }

  # Maximal depth tree couldn't find good enough approx
  if(!check_r2(mod, ale.values, epsilon)) {
    warning(sprintf("couldnt find better than alpha model. width is %.2f. SSE percentage is %.3f",
      width(mod), get_r2(mod, ale.values)))
    return(NULL)
  }
  while(check_r2(mod, ale.values, epsilon)) {
    mod_best = mod
    mod = prune_tree_1n(mod)
  }
  mod_best
}


get_r2 = function(mod, ale.values) {
  seg.predictions = predict_approx_mod(mod)
  SST = var(ale.values)
  if(SST == 0) { return(FALSE)}
  SSE = var(seg.predictions - ale.values)
  SSE/SST
}

check_r2 = function(mod, ale.values, epsilon){
  get_r2(mod, ale.values) < epsilon
}

predict_approx_mod = function(mod, type = "response", ...) {
  if(inherits(mod, "lmtree")) {
    predict(mod, newdata = mod$data, type = type, ...)
  } else {
    predict(mod, type = type, ...)
  }
}





### Sampling based linear segments approximation approach:

lsamp_best_o = function(ale, max_feat_cost, epsilon, m = 100) {
  max_breaks = max_feat_cost / 2
  n_breaks = 1
  r1 = 1
  while(r2 > epsilon) {
    mod = lsamp_best(ale, n_breaks, m = m, epsilon = epsilon)
    r2 = mod$r2
    n_breaks = n_breaks + 1
  }
  return(mod)
}

lsamp_best = function(ale, n_breaks, m, type, epsilon) {
  r2_best = 1
  ## TODO: If m > number of possible combinations, reduce m
  for(i in 1:m) {
    res = lsamp(ale, n_breaks = n_breaks, "segment")
    if(res$r2 < r2_best) {
      mod_best = res$mod
      r2_best = res$r2
    }
    if (r2_best < epsilon) {
      return(list(mod = mod_best, r2 = r2_best))
    }
  }
  list(mod = mod_best, r2 = r2_best)
}

lsamp = function(ale, n_breaks = 2, type){
  assert_choice(type, choices = c("step", "segment"))
  fname = setdiff(colnames(ale$results), c(".ale", ".type"))
  breakpoints_all = unique(ale$results[,fname])
  x = ale$predictor$data$get.x()[,fname, with=FALSE][[1]]
  xmin = min(x)
  xmax = max(x)
  breakpoints_all = setdiff(breakpoints_all, c(xmin, xmax))
  breakpoints = sample(breakpoints_all, size = n_breaks, replace = FALSE)
  intervals_point = c(min(x), breakpoints, max(x))

  x_interval = cut(x, breaks = intervals_point, include.lowest = TRUE)
  ale_prediction = ale$predict(x)
  dat = data.frame(x = x, interval = x_interval, ale = ale_prediction )
  mod = lm(ale ~ x * interval, data = dat)
  list(mod = mod, r2 = get_r2(mod, ale_prediction))
}

res = lsamp_best_o(ale, 10, 0.01)
plot(ale$predictor$data$get.x()[, "V1", with=FALSE][[1]],predict(res$mod))


plot(ale)
