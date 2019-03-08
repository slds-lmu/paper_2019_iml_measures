# TODO: Test
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
