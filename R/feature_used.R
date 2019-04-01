# TODO: Test
feature_used = function(pred, feature, sample_size = 500){
  dat = pred$data$get.x()
  fvalues = dat[, ..feature][[1]]
  # permute feature
  dat2 = dat[sample(1:nrow(dat), size = sample_size, replace = TRUE)]
  prediction1 = pred$predict(dat2)
  sampled_fvalues = sapply(dat2[,..feature][[1]], function(x){
    sample(setdiff(fvalues, x), size = 1)
  })
  dat2 = dat2[, (feature) := sampled_fvalues]
  prediction2 = pred$predict(dat2)
  if (any(( prediction1 - prediction2) != 0)) return(TRUE)
  FALSE
}
