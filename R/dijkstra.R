#'dijkstra algorithms
#'
#'@description Let the starting nodebe be called the initial node. Let the
#'distance of node Y be the distance from the initial node to Y. Dijkstra's
#'algorithm will initially start with infinite distances and will try to improve
#'them step by step.Mark all nodes unvisited. Create a set of all the unvisited
#'nodes called the unvisited set. Assign to every node a tentative distance
#'value: set it to zero for initial node and to infinity for all other
#'nodes. Set the initial node as current.For the current node, consider all of
#'its unvisited neighbours and calculate their tentative distances through the
#'current node. Compare the newly calculated tentative distance to the current
#'assigned value and assign the smaller one. When we are done considering all of
#'the unvisited neighbours of the current node, mark the current node as visited
#'and remove it from the unvisited set. A visited node will never be checked
#'again.If the destination node has been marked visited (when planning a route
#'between two specific nodes) or if the smallest tentative distance among the
#'nodes in the unvisited set is infinity (when planning a complete traversal;
#'occurs when there is no connection between the initial node and remaining
#'unvisited nodes), then stop. The algorithm has finished. Otherwise, select the
#'unvisited node that is marked with the smallest tentative distance, set it as
#'the new "current node", and go back to step 3.
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
