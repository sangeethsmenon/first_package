#' Dijkstra's Algorithm 
#'
#' The algorithm takes a graph and an initial node and calculates the shortest path from the
#"initial node to every other node in the graph
#'
#' @param graph A data frame representing the weighted graph with columns:
#'  - v1: The starting node of an edge.
#'  - v2: The ending node of an edge.
#'  - w: The weight of the edge.
#' @param init_node The initial node from which to find the shortest paths.
#'
#' @return A numeric vector of length equal to the number of nodes in the graph.
#'  Each element represents the shortest distance from the initial node to the
#'  corresponding node in the graph.
#'
#' @references
#' Wikipedia: Dijkstra's Algorithm - https://en.wikipedia.org/wiki/Dijkstra%27s_algorithm
#'
#' @export

dijkstra <- function(graph, init_node) {
  stopifnot(is.data.frame(graph), all(c("v1", "v2", "w") %in% colnames(graph)))
  stopifnot(is.numeric(init_node), length(init_node) == 1)
  actual_vertices <- unique(c(graph$v1, graph$v2))
  no_vertices <- length(actual_vertices)
  short <- rep(Inf, no_vertices)
  short[init_node] <- 0
  checked_vertices <- rep(FALSE, no_vertices)
  for (no in 1:(no_vertices - 1)) {
    initial_length <- Inf
    start_vertex <- -1
    for (i in 1:no_vertices) {
      if (!checked_vertices[i] && short[i] < initial_length) {
        initial_length <- short[i]
        start_vertex <- i
      }
    }
    checked_vertices[start_vertex] <- TRUE
    for (element in 1:nrow(graph)) {
      if (!checked_vertices[graph$v2[element]] && graph$v1[element] == actual_vertices[start_vertex] &&
          (short[start_vertex] + graph$w[element] < short[graph$v2[element]])) {
        short[graph$v2[element]] <- short[start_vertex] + graph$w[element]
      }
    }
  }
  
  return(short)
}
