#' Approximate ALE curve
AleApprox = R6::R6Class("AleApprox",
  public = list(
    # ALE to be approximated
    ale = NULL,
    # R-squared of first order ale model
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
    ssq_ale = NULL,
    var = NULL,
    shapley_var = NULL,
    max_complex = FALSE,
    feature_used = TRUE,
    approx_values = NULL,
    # Number of iterations used to estimate if feature was used
    m_nf = NULL,
    #' @param ale A FeatureEffet object
    #' @param epsilon The allowed approximation error
    #' @param max_breaks The maximum number of segments allowed
    initialize = function(ale, epsilon, max_breaks, m_nf){
      assert_class(ale, "FeatureEffect")
      assert_numeric(epsilon, lower = 0, upper = 1, len = 1,
                     any.missing = FALSE)
      assert_numeric(max_breaks, len = 1)
      assert_numeric(m_nf, len = 1, lower = 1)
      self$ale = ale
      self$epsilon = epsilon
      self$max_breaks = max_breaks
      self$feature = ale$feature.name
      self$m_nf = m_nf
      private$x = self$ale$predictor$data$X[, self$feature, with = FALSE][[1]]
      private$ale_values = self$ale$predict(private$x)
      self$ssq_ale  = ssq(private$ale_values)
      # Variance of the ALE plot weighted by data density
      self$var = self$ssq_ale  / length(private$x)
    }
  ),
  private = list(
    x = NULL,
    ale_values = NULL,
    is_null_ale = function() {
      if(!feature_used(self$ale$predictor, self$feature, sample_size = self$m_nf)) {
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
    initialize = function(ale, epsilon, max_seg, m_nf) {
      assert_true(all.equal(ale$feature.type,"categorical", check.attributes = FALSE))
      super$initialize(ale, epsilon, max_breaks = max_seg, m_nf = m_nf)
      if(!private$is_null_ale()) {
        self$approximate()
        self$n_coefs = ifelse(self$max_complex, max_seg - 1, length(unique(self$tab$lvl)) - 1)
        self$predict = function(dat){
          merge(dat, self$tab, by.x = self$feature, by.y = "x", sort = FALSE)[["pred_approx"]]
        }
        self$approx_values = self$predict(self$ale$predictor$data$get.x())
        ssq_approx_error = ssq(self$approx_values -  private$ale_values)
        self$r2 = 1 - ssq_approx_error / self$ssq_ale
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
        BREAK_SAMPLE_SIZE = 30
        # keep splits from before and try all additional splits.
        splits = t(combn(1:(nlevels(x) - 1), n_breaks))

        if(nrow(splits) > BREAK_SAMPLE_SIZE) splits = splits[sample(1:nrow(splits), BREAK_SAMPLE_SIZE),,drop = FALSE]
        ssms = apply(splits, 1, function(splitx) {
          step_fn(as.numeric(splitx), df, ssq_ale = self$ssq_ale )
        })
        min_ssms = min(ssms)
        best_split_index = which(ssms == min_ssms)[1]
        pars = splits[best_split_index,]
        if(n_breaks == nlevels(x)) {
          pars = 1:(nlevels(x) - 1)
          break()
        }
        if(min_ssms <= self$epsilon)  break()
      }
      if(min_ssms > self$epsilon)  self$max_complex = TRUE
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


#' Compute fit of step approximation
#'
#' @param par cutoff points
#' @param dat data.frame with columns ale and n, number of instances
#' @param ssq_ale sum of squares for ALE
#' @return sum of squared errors
step_fn = function(par, dat, ssq_ale){
  expect_data_table(dat, any.missing = FALSE)
  breaks = unique(round(par, 0))
  dat$lvl = cut(1:nrow(dat), unique(c(0, breaks, nrow(dat))))
  dat2 = dat[, .(ale_mean = weighted.mean(ale, w = n), n = sum(n)), by = lvl]
  # ALE plots have mean zero
  ssq_approx = sum( (dat2$ale_mean) ^ 2 * dat2$n)
  1 - (ssq_approx / ssq_ale)
}

AleNumApprox = R6::R6Class(classname = "AleNumApprox",
  inherit = AleApprox,
  public = list(
    # Table holding the level/new_level info
    model = NULL,
    breaks = NULL,
    # Table for intervals with intercept and slope
    segments = NULL,
    initialize = function(ale, epsilon, max_seg, m_nf) {
      assert_true(all.equal(ale$feature.type, "numerical", check.attributes = FALSE))
      assert_numeric(max_seg)
      # only makes
      max_breaks = max_seg  - 1
      super$initialize(ale, epsilon, max_breaks, m_nf = m_nf)
      if(!private$is_null_ale()) {
        self$approximate()
        # Don't count the intercept
        n_coefs = nrow(self$segments) + sum(self$segments$slope != 0) - 1
        self$n_coefs = min(max_seg * 2, n_coefs)
        self$predict = function(dat) {
          if(is.data.frame(dat)) {
            x = dat[[self$feature]]
          } else {
            x = dat
          }
          x_interval = cut(x, breaks = self$breaks, include.lowest = TRUE)
          dat = data.table(x, interval = x_interval)
          mx = merge(dat, self$segments, by.x = "interval", by.y = "interval", sort = FALSE)
          mx$intercept + mx$slope * mx$x
        }
        self$approx_values = self$predict(self$ale$predictor$data$get.x())
        ssq_approx_error = ssq(self$approx_values -  private$ale_values)
        self$r2 = 1 - ssq_approx_error / self$ssq_ale
      }
    },
    approximate = function(){
      x = private$x
      # test 0 breaks
      mod = lm(private$ale_values ~ x)
      ssq_approx_error = ssq(private$ale_values - predict(mod))
      if( self$ssq_ale  == 0 || (ssq_approx_error/self$ssq_ale ) < self$epsilon) {
        if(any(is.na(predict(mod)))) browser()
        if(any(is.na(private$ale_values))) browser()
        self$r2 = get_r2(predict(mod), private$ale_values)
        self$approx_values = predict(mod)
        model = mod
        self$breaks = c(min(x), max(x))
        x_interval = cut(x, breaks = self$breaks, include.lowest = TRUE)
        self$segments = extract_segments(model, self$breaks, levels(x_interval))
        return()
      }
      for( n_breaks in 1:self$max_breaks) {
        lower = as.numeric(rep(min(x), times = n_breaks))
        upper = as.numeric(rep(max(x), times = n_breaks))
        init_breaks = quantile(x, seq(from = 0, to = 1, length.out = n_breaks + 2))[2:(n_breaks +1)]
        opt_gensa = GenSA(par = init_breaks, segment_fn, lower, upper, ale = self$ale,
          control = list(maxit = 100), self$ssq_ale ,
          x = x, ale_prediction = private$ale_values)
        pars = opt_gensa$par
        if(opt_gensa$value <= self$epsilon)  break()
      }
      if(opt_gensa$value > self$epsilon)  self$max_complex = TRUE
      # fit lm with par as cut points
      self$breaks = sort(unique(c(min(x), pars, max(x))))
      x_interval = cut(x, breaks = self$breaks, include.lowest = TRUE)
      dat = data.frame(x = x, interval = x_interval, ale = private$ale_values)
      # TODO: Try step model first
      model = lm(ale ~ x * interval, data = dat)
      segments = extract_segments(model, self$breaks, levels(x_interval))
      self$segments = eliminate_slopes(segments, x, private$ale_values,
        self$epsilon, self$breaks)
    },
    plot = function(ylim = c(NA, NA), maxv = NULL) {
      assert_numeric(maxv, null.ok=TRUE)
      fdat = self$ale$predictor$data$get.x()[[self$feature]]
      x = seq(from = min(fdat), to = max(fdat), length.out = 200)
      y = self$predict(x)
      intervals = cut(x, breaks = self$breaks, include.lowest = TRUE)
      dat = data.frame(x = x, y = y, interval = intervals)
      max_string = ifelse(self$max_complex, "+", "")
      varv = ifelse(is.null(maxv), self$var, self$var/maxv)
      p = self$ale$plot(ylim = ylim) +
        geom_line(aes(x = x, y = y, group = interval), color = "red",
          data = dat, lty = 2) +
        ggtitle(sprintf("C: %i%s, R2: %.3f, V: %.3f", self$n_coefs, max_string,self$r2, varv))
      if(length(self$breaks) > 2) {
        breaks = self$breaks[2:(length(self$breaks) - 1)]
        p = p + geom_vline(data = data.frame(breaks = self$breaks), aes(xintercept = self$breaks))
      }
      p
    }
  )
)


#' Function to optimize for ALE approx
#'
#' @param par The breakpoints
segment_fn = function(par, ale, ssq_ale, x, ale_prediction){
  x_interval = cut(x, breaks = unique(c(min(x), par, max(x))), include.lowest = TRUE)
  dat = data.table(xv = x, interval = x_interval, alev = ale_prediction)
  res = dat[, .(ssq(.lm.fit(cbind(rep.int(1, times = length(xv)),xv),alev)$residuals)), by = interval]
  error = sum(res$V1)/ssq_ale
  return(error)
}
