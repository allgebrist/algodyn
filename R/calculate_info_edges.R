


#' Calculate information contribution of edges
#'
#' @param x an igraph object or a valid adjacency matrix.
#' @param block_size block size required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM).
#' @param offset offset required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM).
#'
#' @details The information contribution of an edge \eqn{e} to a graph \eqn{G} is given by \eqn{I(G,e)=C(G)-C(G-e)}, where \eqn{C(G)} denotes the information content of \eqn{G} and \eqn{C(G-e)} the information content of \eqn{G} after removing \eqn{e}.
#' This function performs sequential perturbations (deletions) on the edges of \eqn{G} to calculate their information contribution.
#'
#' @import igraph
#' @return A data frame containing the perturbations on the original graph's edges, and their corresponding BDM values and estimated information contributions.
#'
#' @examples
#' \dontrun{
#' coxeter_graph <- make_graph("Coxeter")
#' calculate_info_edges(coxeter_graph)
#' }
#'
#'@export
calculate_info_edges <- function(x, block_size = 4, offset = 4) {

    x_adj_matrix <- c()

    if (is.igraph(x)) {
        x_adj_matrix <- as.matrix(as_adjacency_matrix(x))
    } else if (is.matrix(x)) {
        x_adj_matrix <- x
    } else {
        stop("ERROR: The object should be a graph or adjacency matrix.")
    }

    x_bdm_value <- bdm2D(x_adj_matrix, block_size, offset)

    edge_deletions_df <- as_data_frame(x, what = "edges")
    deletion_columns  <- c("bdm_value", "bdm_difference")

    edge_deletions_df[, deletion_columns] <- NA

    for (i in 1:nrow(edge_deletions_df)) {
        if (is.matrix(x)) {
          x <- graph_from_adjacency_matrix(x)
        }

        x_deleted_edge  <- delete_edges(x, paste0(edge_deletions_df[i, ]$from,
                                                  "|",edge_deletions_df[i, ]$to))

        deleted_edge_matrix <- as.matrix(as_adjacency_matrix(x_deleted_edge))
        deleted_edge_bdm_value <- bdm2D(deleted_edge_matrix, block_size, offset)

        edge_deletions_df[i, ]$bdm_value <- deleted_edge_bdm_value
        edge_deletions_df[i, ]$bdm_difference <- x_bdm_value - deleted_edge_bdm_value
    }

    return(edge_deletions_df)
}
