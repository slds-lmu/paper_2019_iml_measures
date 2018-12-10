prune_tree_1n = function (tree, flag = FALSE) {
  #browser()
  if(width(tree) == 1) stop("Can't prune root node")
  node <- tree$node
  nd <- as.list(node)
  kids <- lapply(nd, "[[", "kids")
  id <- seq_along(nd)

  is_prunable = sapply(kids, function(node_kids) {
    if(is.null(node_kids)) {
      FALSE
    } else {
      # both kids have no further kids
      is.null(kids[[node_kids[1]]]) & is.null(kids[[node_kids[2]]])
    }
  })
  # only terminal nodes as kids
  ps = sapply(nd, function(x) {
    res = x$info$p.value
    ifelse(is.null(res), NA, res)
  })
  nd_df = data.frame(id = id, prunable = is_prunable, pvalue = ps)
  nd_df = nd_df[nd_df$prunable,]
  to_be_pruned = nd_df$id[nd_df$pvalue == max(nd_df$pvalue)]
  nodeprune.party2(tree, to_be_pruned)
}


nodeprune.party2 = function (x, ids, ...)  {
  if (!is.numeric(ids))
    ids <- match(ids, names(x))
  stopifnot(ids %in% nodeids(x))
  idxs <- lapply(ids, partykit:::.get_path, obj = node_party(x))
  cls <- class(x)
  x <- unclass(x)
  ni <- which(names(x) == "node")
  for (i in 1:length(idxs)) {
    idx <- c(ni, idxs[[i]])
    tmp <- try(x[[idx]], silent = TRUE)
    if (inherits(tmp, "try-error"))
      (next)()
    idrm <- nodeids(x[[idx]])[-1]
    x[[idx]] <- partynode(id = id_node(x[[idx]]), info = info_node(x[[idx]]))
    if (length(idrm) > 0) {
      if (!is.null(x$fitted) && "(fitted)" %in% names(x$fitted)) {
        j <- x$fitted[["(fitted)"]] %in% idrm
        x$fitted[["(fitted)"]][j] <- ids[i]
      }
    }
  }
  class(x) <- cls
  oldids <- nodeids(x)
  newids <- 1:length(nodeids(x))
  x = partykit:::`nodeids<-.party`(x, newids)
  for (i in seq_along(oldids)) {
    if (oldids[i] != newids[i]) {
      x$fitted[["(fitted)"]][x$fitted[["(fitted)"]] == oldids[i]] <- newids[i]
    }
  }
  return(x)
}

