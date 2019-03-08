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
    max_seg_cat = NULL,
    max_seg_num = NULL,
    # How well the main effects model approximates total model
    r2 = NULL,
    # The mean complexity per ALE plot, weighted by its variance
    c_wmean = NULL,
    # Number of features used in predictor
    n_features = NULL,
    # The approximation models of the ALE plots
    approx_models = NULL,
    initialize = function(predictor, grid.size = 50, parallel = FALSE,
      epsilon = 0.05, max_seg_cat = 5, max_seg_num = 5) {
      if(predictor$task == "classification" & is.null(predictor$class)) {
        stop("Please set class in Predictor")
      }
      assert_numeric(epsilon, lower = 0, upper = 1, any.missing = FALSE, len = 1)
      assert_numeric(max_seg_cat, len = 1, any.missing = FALSE, upper = grid.size)
      assert_numeric(max_seg_num, len = 1, any.missing = FALSE, upper = grid.size)
      self$max_seg_cat = max_seg_cat
      self$max_seg_num = max_seg_num
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
          AleNumApprox$new(ale = eff, epsilon = self$epsilon, max_seg = self$max_seg_num)
        } else {
          AleCatApprox$new(ale = eff, epsilon = self$epsilon, max_seg = self$max_seg_cat)
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
