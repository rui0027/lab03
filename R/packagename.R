#'lab03:a package to implement Dijkstra algorithm and Euclidean algorithm.
#'
#'The package provides two function:dijkstra and euclidean
#'
#'@section The dijkstra function: This function uses dijkstra algorithms to find
#'the shortest distance between two nodes in a graph. Starting from the
#'init_node,the algorithm creates two sets for invisited nodes and visited
#'nodes.To every node,set zero to init_node and infinity to other nodes. For the
#'current node,calculate the distance to its neighbors and compare with the
#'known distances,assign the shortest.Once after finding the shortest distance
#'to a node,divide the node into visited set and set a new init_node.Reverse the
#'step until all nodes are visited. The euclidean function:This function is a
#'loop algorithm to find the greatest common divisor (GCD) of two nonzero number
#'x and y using euclidean algorithm. If the lager number devided by the smaller
#'number has a nonzero remainder,replace the lager number with remainder.Reverse
#'the step until the remainder is zero, then the denominator is GCD of the
#'original two numbers.
#'
#'@docType package
#'@name lab03
NULL
