
# =============================================================================
# Number of linear segments in segmented linear regression
# =============================================================================

n_segs = function(pred, ...){
  feature_names = pred$data$feature.names
  n_seg_values = sapply(feature_names, function(fname){
    # test both with segments and step function
    min(n_segs_feature(pred, feature.name = fname, cat.mode = FALSE),
      n_segs_feature(pred, feature.name = fname, cat.mode = TRUE, ...))
  })
  sum(n_seg_values)
}

# TODO: Enforce max.df
# TODO: Don't use all data for approx, but use grid plus weights by density
#       For this, checkout the other measures that I tried out
# TODO: increase party.alpha first, and if epsilon already ok, start pruning
# TODO: Check for errors in nodeprune
n_segs_feature = function(pred, feature.name, grid.size = 50, epsilon = 0.05, plot = FALSE, cat.mode = FALSE, max_df = 10) {
  # The base cost of using a feature, regardless of complexity
  cost_of_feature = 0

  ale.fun = get_ale_function(pred, feature.name, grid.size = grid.size)
  feature = pred$data$get.x()[[feature.name]]
  ale.values = ale.fun(feature)

  # flat function
  if(n_distinct(ale.values) == 1) return(0)


  if(is.numeric(feature)) {
    mod_best = lm(ale.values ~ feature)
    # linear model case
    if(check_r(mod_best, ale.values, epsilon)) {
      if(plot) {
        plot_segments(mod_best, pred, feature.name, prediction = predict(mod_best))
      }
      return(1)
    }
  }

  if(is.numeric(feature) & !cat.mode) {
    # fully grown tree
    mod = partykit::lmtree(ale.values ~ feature | feature,  alpha = 0.5, minsize = 2)
  } else {
    mod = partykit::ctree(ale.values ~ feature,  control = ctree_control(alpha = 0.5, minbucket = 1))
  }

  # Maximal depth tree couldn't find good enough approx
  if(!check_r(mod, ale.values, epsilon)) {
    print(sprintf("couldnt find better than alpha model. width is %.2f. SSE percentage is %.3f",
      width(mod), get_r(mod, ale.values)))
    if(plot) {
      plot_segments(mod, pred, feature.name, prediction = predict(mod))
    }
    return(max_df)
  }

  while(check_r(mod, ale.values, epsilon)) {
    mod_best = mod
    mod = prune_tree_1n(mod)
  }
  if(plot) {
    plot_segments(mod_best, pred, feature.name, prediction = pred_tree(mod_best))
  }
  n_df = width(mod_best)

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
  n_df = min(n_df, max_df)
  n_df + cost_of_feature
}

pred_tree = function(mod, ...) {
  if(inherits(mod, "lmtree")) {
    predict(mod, newdata = mod$data, ...)
  } else {
    predict(mod, ...)
  }
}

get_r = function(mod, ale.values) {
  seg.predictions = pred_tree(mod)
  SST = var(ale.values)
  if(SST == 0) { return(FALSE)}
  SSE = var(seg.predictions - ale.values)
  SSE/SST
}

check_r = function(mod, ale.values, epsilon){
  get_r(mod, ale.values) < epsilon
}


plot_segments = function(mod, pred, feature.name, prediction) {
  ale = FeatureEffect$new(pred, feature.name, grid.size = 30, method = "ale")
  feature.name = ale$feature.name
  feature = pred$data$get.x()[[feature.name]]
  dat = data.frame(x = feature, y = prediction)
  if(inherits(mod, "party")) {
    dat$node = factor(pred_tree(mod, type = "node"))
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
  print(p)
}


# =============================================================================
# Polynomial regression number of poylnoms
# =============================================================================
poly_complexity = function(pred, feature.name, grid.size) {
  stop("FIX SST computation first, because ale prediction should be used, not black box prediction")

  ale = FeatureEffect$new(pred, feature = feature.name, method = "ale", grid.size = grid.size)
  dat = ale$results[c(".ale", feature.name)]
  colnames(dat) = c("y", "x")
  feature_values = pred$data$X[,feature.name,with=FALSE][[1]]
  dens = density(feature_values)
  dens_fun = approxfun(dens$x, dens$y)
  w = dens_fun(ale$results[, feature.name])
  mod = lm(y ~ poly(x, 1), data = dat)
  all.dat = data.frame(pred$data$get.x())
  predictions = pred$predict(all.dat)[[1]]
  mean_prediction = mean(predictions)
  all.dat.sub = all.dat[c(feature.name)]
  colnames(all.dat.sub) = "x"
  poly.predictions = predict(mod, all.dat.sub)

  SST = var(predictions)
  if(SST == 0) {
    return(1)
  }
  SSE = var(poly.predictions - predictions)
  SSE/SST
}

# =============================================================================
# ALE length
# =============================================================================
ale_length = function(pred, feature.name, grid.size) {
  ale = FeatureEffect$new(pred, feature = feature.name, method = "ale", grid.size = grid.size)
  print(plot(ale))
  cols = c(".ale", feature.name)
  diffs = ale$results[1:(nrow(ale$results) - 1), cols] -
    ale$results[2:nrow(ale$results),cols]
  distance = sum(sqrt(rowSums(diffs^2)))
  distance
  distance.linear = diff(range(ale$results[, feature.name]))
  distance / distance.linear
}

# =============================================================================
# ALE fanova
# =============================================================================

ale_fanova  = function(pred, grid.size  = 50){

  if(pred$task == "classification" & is.null(pred$class)) {
    stop("Please set class in Predictor")
  }

  #print(pred$model$learner.model)
  feature.names = pred$data$feature.names
  dat = data.frame(pred$data$get.x())
  # for all features:
  funs = lapply(feature.names, function(fname) {
    func = get_ale_function(pred, fname, grid.size = grid.size)
    func(dat[,fname])
  })
  funs = data.frame(funs)

  predictions = pred$predict(dat)[[1]]
  mean_prediction = mean(predictions)
  ale_predictions = mean_prediction + rowSums(funs)

  SST = var(predictions)
  if(SST == 0) {
    return(1)
  }
  SSE = var(ale_predictions - predictions)
  SSE/SST
}

get_ale_function = function(pred, feature.name, grid.size) {
  ale = FeatureEffect$new(pred, feature = feature.name, method = "ale", grid.size = grid.size)
  if(ale$feature.type == "numerical"){
    approxfun(x = ale$results[,feature.name], y = ale$results$.ale)
  } else {
    function(x){
      df = data.frame(as.character(x), stringsAsFactors = FALSE)
      colnames(df) = feature.name
      results = ale$results
      results[,feature.name] = as.character(results[,feature.name])
      dplyr::left_join(df, results, sort = FALSE, by = feature.name, all.x=TRUE)$.ale
    }
  }
}


# =============================================================================
# How similar go GAM
# =============================================================================
gam_alike = function(pred){
  X = pred$data$get.xy()
  X$.prediction = pred$predict(X)
  tsk = makeRegrTask(data = X, target = ".prediction")
  lrn = makeLearner("regr.gamboost")

  ps = makeParamSet(makeIntegerParam("mstop", lower = 1, upper = 4000))
  ctrl = makeTuneControlGrid()
  rdesc = makeResampleDesc("CV", iters = 3L)
  res = tuneParams(lrn, task = tsk, resampling = rdesc,
    par.set = ps, control = ctrl)
  lrn = setHyperPars(lrn, par.vals = res$x)

  mod = train(lrn, tsk)
  SST = var(X$.prediction)
  gam.pred = getPredictionResponse(predict(mod, newdata =X))
  SSE = var(gam.pred - X$.prediction)
  1 - SSE / SST
}


# =============================================================================
# Number of features
# =============================================================================

n.features = function(pred){
  scores = lapply(pred$data$feature.names, function(feature_name) {
    eff = FeatureEffect$new(pred, feature_name, method = "pdp")
    1 - (length(unique(eff$results$.y.hat)) == 1)
  })
  # NAs are from categorical features
  sum(unlist(scores), na.rm = TRUE)
}


# =============================================================================
# alpha-controlled degrees of freedom
# =============================================================================

sum_df = function(predictor) {
  scores = c()

  for (feature_name in predictor$data$feature.names) {
    scores = c(scores, feature_name = df_feature(predictor, feature_name))
  }

  # NAs are from categorical features
  sum(unlist(scores), na.rm = TRUE)
}


# take minimal degrees of freedom from tree and spline
df_feature = function(predictor, feature_name, eps = 0.05, max_cp = 10, plot = FALSE){
  min(df_tree_feature(predictor, feature_name, max_cp = max_cp, eps = eps, plot = plot),
    df_spline_feature(predictor, feature_name = feature_name, eps = eps, plot = plot),
    na.rm = TRUE)
}

# for tree splits
df_tree_feature = function(predictor, feature_name, max_cp = 10, eps = 0.05, plot = FALSE){
  require("rpart")
  # Fit ALE plot
  ale = FeatureEffect$new(predictor, feature_name, method = "ale", grid.size = 30)
  if(length(unique(ale$results$.ale)) == 1) return(0)
  feature_values = predictor$data$X[,feature_name,with=FALSE][[1]]
  feature_type = predictor$data$feature.types[feature_name]
  if(feature_type == "categorical") {
    w = table(feature_values)[ale$results[, feature_name]]
  } else {
    # Weight by density
    dens = density(feature_values)
    dens_fun = approxfun(dens$x, dens$y)
    w = dens_fun(ale$results[, feature_name])
  }

  # fully grow tree
  ctrl = rpart.control(maxdepth = 30, minsplit = 1, minbucket = 1, cp = 0)
  ale.tree = rpart(ale$results$.ale ~ ale$results[, feature_name], w=w, control = ctrl)
  ale.tree.pred = predict(ale.tree)


  calculate_R_squared = function(ale, tree, w){
    SST = weighted.var(ale$results$.ale, w = w)
    if(SST == 0){
      warning(paste('No variance for feature %s', feature_name))
      return(0)
    }
    SSE = weighted.var(ale$results$.ale - predict(tree), w = w)
    1 - SSE/SST
  }

  found_smallest_tree = FALSE
  cp = max_cp
  while(!found_smallest_tree){
    tree_new = prune.rpart(ale.tree, cp = cp)
    R_squared = calculate_R_squared(ale, tree_new, w)
    if(R_squared > (1 - eps)) found_smallest_tree = TRUE
    if(cp < 0.00000001) stop("Couldnt find a good tree")
    cp = 0.95 * cp
  }


  if(plot){
    tree.dat = data.table(cbind(predict(tree_new, ale$results), ale$results[, feature_name]))
    colnames(tree.dat) = c(".ale", feature_name)
    tree.dat[,.SD[1],by = .ale]

    # for plotting till the end
    tree.dat = rbind(tree.dat, tree.dat[nrow(tree.dat),])
    print(plot(ale) + geom_step(data = tree.dat, color = 'red'))
  }

  n_nodes = sum(tree_new$frame$var == "<leaf>")
  # degrees of freedom needed is reduced by one, because first category can be seen as reference
  n_nodes - 1
}



# for splines
df_spline_feature = function(predictor, feature_name,  eps = 0.05, plot = FALSE){
  feature_type = predictor$data$feature.types[feature_name]
  if(feature_type == "categorical") {
    NA
  } else {
    # Fit ALE plot
    ale = FeatureEffect$new(predictor, feature_name, method = "ale", grid.size = 30)
    if(length(unique(ale$results$.ale)) == 1) return(0)
    # Weight by density
    dens = density(predictor$data$X[,feature_name,with=FALSE][[1]])
    dens_fun = approxfun(dens$x, dens$y)
    w = dens_fun(ale$results[, feature_name])

    calculate_R_squared = function(ale, w, pred.mod){
      SST = weighted.var(ale$results$.ale, w = w)
      if(SST == 0){stop("No variance")}
      SSE = weighted.var(ale$results$.ale - pred.mod, w = w)
      1 - SSE/SST
    }

    # first try linear model
    ale.lm = lm(ale$results$.ale ~ ale$results[, feature_name], w=w)
    r_squared = calculate_R_squared(ale, w, predict(ale.lm))
    if(r_squared > (1-eps)) return(1)

    found_best_spline = FALSE
    spar = 1
    while(!found_best_spline){
      ale.inter = smooth.spline(ale$results[, feature_name], ale$results$.ale,
        w = dens_fun(ale$results[, feature_name]), spar = spar)
      R_squared = calculate_R_squared(ale,w, predict(ale.inter)$y)
      if(R_squared > (1 - eps)) found_best_spline = TRUE
      if(spar < 0.00000001) break()
      spar = 0.99 * spar
    }

    if(plot) {
      spline.dat = data.frame(ale.inter$y, ale.inter$x)
      colnames(spline.dat) = c(".ale", feature_name)
      print(plot(ale) + geom_line(data = spline.dat, color = 'red'))
    }

    # measure spline complexity
    ale.inter$df
  }
}

# =============================================================================
# Best of single split tree and lm for feature effect
# =============================================================================

lin_or_bin_tree = function(predictor) {
  scores = lapply(predictor$data$feature.names, function(feature_name) {
    lin_or_bin_tree_feature(predictor, feature_name)
  })
  # NAs are from categorical features
  sum(unlist(scores), na.rm = TRUE) / ncol(predictor$data$get.x())
}

lin_or_bin_tree_feature = function(predictor, feature_name){
  min(score_linearity_feature_lm(predictor, feature_name), score_split_feature(predictor, feature_name), na.rm = TRUE)
}


# =============================================================================
# Feature score based on R.squared of single split tree
# =============================================================================
score_split = function(predictor) {
  scores = lapply(predictor$data$feature.names, function(feature_name) {
    score_split_feature(predictor, feature_name)
  })
  # NAs are from categorical features
  mean(unlist(scores), na.rm = TRUE)
}


score_split_feature = function(predictor, feature_name){
  require("rpart")
  # Fit ALE plot
  ale = FeatureEffect$new(predictor, feature_name)
  feature_values = predictor$data$X[,feature_name,with=FALSE][[1]]
  feature_type = predictor$data$feature.types[feature_name]
  if(feature_type == "categorical") {
    w = table(feature_values)[ale$results[, feature_name]]
  } else {
    # Weight by density
    dens = density(feature_values)
    dens_fun = approxfun(dens$x, dens$y)
    w = dens_fun(ale$results[, feature_name])
  }
  ctrl = rpart.control(maxdepth = 1, minsplit = 1, minbucket = 1)
  ale.tree = rpart(ale$results$.ale ~ ale$results[, feature_name], w=w, control = ctrl)
  ale.lm.pred = predict(ale.tree)

  SST = weighted.var(ale$results$.ale, w = w)
  if(SST == 0){
    warning(paste('No variance for feature %s', feature_name))
    return(0)
  }
  SSE = weighted.var(ale$results$.ale - predict(ale.tree), w = w)
  SSE/SST
}

# =============================================================================
# Feature linearity score based on 2nd-order derivatives of ALE
# =============================================================================

# Measure linearity per feature
score_linearity_feature = function(predictor, feature_name) {
  feature_type = predictor$data$feature.types[feature_name]
  if(feature_type == "categorical") {
    NA
  } else {
    # Fit ALE plot
    ale = FeatureEffect$new(predictor, feature_name)

    # Weight by density
    dens = density(predictor$data$X[,feature_name,with=FALSE][[1]])
    dens_fun = approxfun(dens$x, dens$y)

    # make continuous spline function
    # https://en.wikipedia.org/wiki/Smoothing_spline
    ale.inter = smooth.spline(ale$results[, feature_name], ale$results$.ale, w = dens_fun(ale$results[, feature_name]))

    # measure spline complexity
    weighted.mean(abs(predict(ale.inter, dens$x, deriv = 2)$y), w = dens$y)
  }
}

# Linearity measure
score_linearity = function(predictor) {
  scores = lapply(predictor$data$feature.names, function(feature_name) {
    score_linearity_feature(predictor, feature_name)
  })
  # NAs are from categorical features
  mean(unlist(scores), na.rm = TRUE)
}


# =============================================================================
# Feature linearity score based on ALE approx with univariate lm
# =============================================================================
weighted.var <- function(x, w, na.rm = FALSE) {
  if (na.rm) {
    w <- w[i <- !is.na(x)]
    x <- x[i]
  }
  sum.w <- sum(w)
  sum.w2 <- sum(w^2)
  mean.w <- sum(x * w) / sum(w)
  (sum.w / (sum.w^2 - sum.w2)) * sum(w * (x - mean.w)^2, na.rm =
      na.rm)
}

score_linearity_feature_lm = function(predictor, feature_name) {
  feature_type = predictor$data$feature.types[feature_name]
  if(feature_type == "categorical") {
    NA
  } else {
    # Fit ALE plot
    ale = FeatureEffect$new(predictor, feature_name)
    # Weight by density
    dens = density(predictor$data$X[,feature_name,with=FALSE][[1]])
    dens_fun = approxfun(dens$x, dens$y)

    w = dens_fun(ale$results[, feature_name])
    ale.lm = lm(ale$results$.ale ~ ale$results[, feature_name], w=w)
    ale.lm.pred = predict(ale.lm)

    SST = weighted.var(ale$results$.ale, w = w)
    if(SST == 0){stop("No variance")}
    SSE = weighted.var(ale$results$.ale - predict(ale.lm), w = w)
    SSE/SST
  }
}

# Linearity measure
score_linearity_lm = function(predictor) {
  scores = lapply(predictor$data$feature.names, function(feature_name) {
    score_linearity_feature_lm(predictor, feature_name = feature_name)
  })
  # NAs are from categorical features
  mean(unlist(scores), na.rm = TRUE)
}
# =============================================================================
# How well does a surrogate model fit
# =============================================================================
surrogate_sim = function(pred, type = 'tree') {
  require(partykit)
  X = data.frame(pred$data$get.x())
  predictions = pred$predict(X)[[1]]
  X$predictions = predictions
  if(type == 'tree') {
    surrogate = rpart(predictions ~ ., data = X, control = rpart.control(maxdepth = 2))
  } else if (type == 'lm'){
    surrogate = lm(predictions ~ ., data = X)
  } else {
    stop("invalid type")
  }
  SST = var(predictions)
  SSE = var(predictions - predict(surrogate))
  1  - SSE/SST
}



# =============================================================================
# Interaction Strength based on Sobol Indices
# =============================================================================




interaction.strength = function(pred, sample.size = 3000){
  1 - sobol2(pred, n = sample.size)
}

sobol2 = function(pred, n){
  pred.fun = function(X){
    pred$predict(newdata = X)[[1]]
  }

  x = sensitivity::sobolmara(pred.fun,
    X1 = task.dat[sample(pred$data$n.rows, n, replace = TRUE),])
  sum(x$S)
}


