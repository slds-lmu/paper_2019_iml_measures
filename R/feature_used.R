# TODO: Test
feature_used = function(pred, feature, sample_size = 500){
  dat = pred$data$get.x()
  # permute feature
  dat2 = dat[sample(1:nrow(dat), size = sample_size)]
  prediction1 = pred$predict(dat2)
  fvalues = sample(dat2[, ..feature][[1]])
  dat2 = dat2[, (feature) := fvalues]
  prediction2 = pred$predict(dat2)
  if (any(( prediction1 - prediction2) != 0)) return(TRUE)
  FALSE
}
