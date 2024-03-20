#' Title: sim_f_net.R
#' Author: Steven Lauterwasser
#' 
#' Summary: a function to simulate the final node degrees of a purely
#' node fitness driven network.
#' 


sim_f_net <- function(node_fitness, node_history, m_t, node_ids = NULL) {
  #' Simulate node fitness network growth
  #' 
  #' @param node_fitness a vector of node fitnesses. If named, the names will be
  #' used as node ids.
  #' @param node_history a matrix with one row per time-step to simulate and one  
  #' column per node, with a cell value of 1 indicating a given node exists at a 
  #' given time step and value of 0 indicating the opposite.
  #' @param m_t a vector where the t-th element is the number of new edges at time t.
  #' @param node_ids a vector of node ids. If `NULL` (the default) then either the 
  #' names (if present) or indices of the `node_fitness` vector are used instead.
  #' 
  #' @returns A three column dataframe containing the node ids, the final final degree
  #' of each node, and the fitness of each node.
  #' 
  
  # set node ids if not provided
  if (is_empty(node_ids)) {
    if (!is_empty(names(node_fitness))) {
      node_ids <- names(node_fitness)
    } else {
      node_ids <- seq_along(node_fitness)
    }
  }
  
  
  edge_history <- map(seq_along(node_history[, 1]), function(x) {
    # create a set of node probabilities for the time step by setting 
    # the node fitness of any non-existent node to zero
    node_probs <- node_history[x, ] * node_fitness
    
    # then sample from the existing nodes using those probabilities, 
    # drawing a number of edges equal to m_t[x] (the new edges at this time step).
    edges <- sample(node_ids, size = m_t[x], replace = TRUE, prob = node_probs)
    edges <- as.data.frame(table(edges)) # get counts per node
    
    # create a dataframe to hold the new edge info
    out <- tibble("node_id" = node_ids, 
                  "new_edges" = 0)
    
    # and set the count of new edges based on the drawn sample
    out[match(edges[["edges"]], out[["node_id"]]), ][["new_edges"]] <- edges[["Freq"]]
    
    return(out[["new_edges"]]) # drop the column of node_ids to allow for easy row binding and row summing
  })
  
  # from a list of vectors to a matrix
  edge_history <- matrix(unlist(edge_history), nrow = ncol(node_history), ncol = nrow(node_history))
  
  edge_distribution <- tibble("node_id" = node_ids, 
                              "edge_count" = rowSums(edge_history))
  
  return(edge_distribution)
  
}