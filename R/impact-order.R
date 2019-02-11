

# TODO: Write tests
impact_order_feature = function(x, y) {
  ordr = names(sort(tapply(y, x, function(y) mean(y))))
  ordered(x, levels = ordr)
}

impact_order_all = function(dat, y) {
  data.frame(lapply(dat, function(x){
    if(inherits(x, "factor") & !inherits(x, "ordered")) {
      impact_order_feature(x, y)
    } else {
      x
    }
  }))
}
