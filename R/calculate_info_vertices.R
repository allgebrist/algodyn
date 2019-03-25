


#' Calculate information contribution of vertices
#'
#' @param x an igraph object or a valid adjacency matrix.
#' @param block_size block size required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM).
#' @param offset offset required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM).
#'
#' @details The information contribution of a vertex \eqn{v} to a graph \eqn{G} is given by \eqn{I(G,v)=C(G)-C(G-v)}, where \eqn{C(G)} denotes the information content of \eqn{G} and \eqn{C(G-v)} the information content of \eqn{G} after removing \eqn{v}.
#' This function performs sequential perturbations (deletions) on the vertices of \eqn{G} to calculate their information contribution.
#'
#' @return A data frame containing the perturbations on the original graph's vertices, and their corresponding BDM values and estimated information contributions.
#' @export
#'
#' @examples
#'

calculate_info_vertices <- function(x, block_size = 4, offset = 4) {

    x_adj_matrix <- c()

    if (is.igraph(x)) {
        x_adj_matrix <- as.matrix(as_adjacency_matrix(x))
    } else if (is.matrix(x)) {
        x_adj_matrix <- x
    } else {
        stop("ERROR: The object should be a graph or adjacency matrix.")
    }

    x_bdm_value <- bdm2D(x_adj_matrix, block_size, offset)

    vertex_deletions_df <- as_data_frame(x, what = "vertices")
    deletion_columns    <- c("bdm_value", "bdm_difference")

    vertex_deletions_df[, deletion_columns] <- NA

    for (i in 1:nrow(vertex_deletions_df)){
        if (is.matrix(x)) {
            x <- graph_from_adjacency_matrix(x)
        }

        x_deleted_vertex <- delete_vertices(x, V(x)[i])

        deleted_vertex_matrix <- as.matrix(as_adjacency_matrix(x_deleted_vertex))
        deleted_vertex_bdm_value <- bdm2d(deleted_vertex_matrix, block_size, offset)

        vertex_deletions_df[i, ]$bdm_value <- deleted_vertex_bdm_value
        vertex_deletions_df[i, ]$bdm_difference <- x_bdm_value - deleted_vertex_bdm_value
    }

    return(vertex_deletions_df)
}
