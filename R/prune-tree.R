prune_tree_1n = function (tree) {
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
  nodeprune.party(tree, to_be_pruned)
}
