#'dijkstra algorithm
#'
#'@description
#'This function uses dijkstra algorithms to find the shortest distance between
#'two nodes in a graph. Starting from the init_node,the algorithm creates two
#'sets for invisited nodes and visited nodes.To every node,set zero to init_node
#'and infinity to other nodes. For the current node,calculate the distance to
#'its neighbors and compare with the known distances,assign the shortest.Once
#'after finding the shortest distance to a node,divide the node into visited set
#'and set a new init_node.Reverse the step until all nodes are visited.
#'@param graph A data.frame.
#'@param init_node A number.
#'@return the shortest path to every other node from the starting node as a vector.
#'@examples
#'dijkstra(wiki_graph,3)
#'dijkstra(wiki_graph,1)
#'@references https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm#Algorithm
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
