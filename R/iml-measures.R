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


interaction.strength = function(pred, sample.size = 50000){
  1 - sum(unlist(sobol(pred, n = sample.size)))
}


sobol = function(pred, n) {
  feature.names = pred$data$feature.names
  dat.intervened = intervene.sobol(generate.sobol, X = data.frame(pred$data$get.x()), n = n)
  res = aggregate.sobol.first(dat.intervened, pred$predict(dat.intervened)[[1]], n = n)
  res
}




generate.sobol = function(n, background){
  background[sample(1:nrow(background), size = n, replace = TRUE), ]
}

intervene.sobol = function(generate.fun, X, n){
  require(dplyr)
  X1 = generate.fun(n, X)
  X2 = generate.fun(n, X)

  n.features = ncol(X)
  feature.names = colnames(X)

  # The intervention
  Ab = lapply(1:n.features, function(feature.index){
    A = X1
    A[,feature.index] = X2[feature.index]
    A
  }) %>% data.table::rbindlist()
  rbind(X1, X2, Ab)
}


aggregate.sobol.first = function(X, y.hat, n, w=NULL,...){
  y.hat.A = y.hat[1:n]
  y.hat.B = y.hat[(n+1):(2*n)]
  Ab.index = (2*n + 1):nrow(X)
  var.y = var(y.hat.A)
  lapply(1:ncol(X), function(i){
    y.hat.Ab.by.feature = matrix(y.hat[Ab.index], nrow = n)
    S_i = (1/n) * sum(y.hat.B * (y.hat.Ab.by.feature[,i] - y.hat.A))
    S_i/ var.y
  })
}

aggregate.sobol.total = function(X, y.hat, n, w=NULL,...){
  y.hat.A = y.hat[1:n]
  y.hat.B = y.hat[(n+1):(2*n)]
  Ab.index = (2*n + 1):nrow(X)
  var.y = var(y.hat.A)
  lapply(1:ncol(X), function(i){
    y.hat.Ab.by.feature = matrix(y.hat[Ab.index], nrow = n)
    S_i = (1/(2*n)) * sum((y.hat.Ab.by.feature[,i] - y.hat.A)^2)
    S_i / var.y
  })
}
