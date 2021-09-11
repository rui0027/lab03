dijkstra <- function(graph, init_node) {
  # get all nodes
  nodes = unique(graph$v1, fromLast = TRUE)
  # matrix m, fill it with w in graph
  m = matrix(c(Inf), nrow = length(nodes), ncol = length(nodes))
  for (i in 1:nrow(graph)) {
    m[graph[i, 1], graph[i, 2]] = graph[i, 3]
  }
  # node to itselves distance = 0
  for (i in 1:nrow(m)) {
    m[i, i] = 0
  }
  for (j in 2:nrow(m)) {
    # keep sort, always starts from the next minimum num
    v = sort(m[init_node,])
    a = v[j]
    for (i in 1:nrow(m)) {
      if (m[init_node, i] == a) {
        index_a = i
      }
    }
    for (i in 1:nrow(m)) {
      if (m[index_a, i] + a < m[init_node, i]) {
        m[init_node, i] = m[index_a, i] + a
      }
    }
  }
  return(m[init_node, ])
}


# 
# wiki_graph <- data.frame(
#   v1 = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 5, 5, 6, 6, 6),
#   v2 = c(2, 3, 6, 1, 3, 4, 1, 2, 4, 6, 2, 3, 5, 4, 6, 1, 3, 5),
#   w = c(7, 9, 14, 7, 10, 15, 9, 10, 11, 2, 15, 11, 6, 6, 9, 14, 2, 9)
# )
# dijkstra(wiki_graph, 1)
