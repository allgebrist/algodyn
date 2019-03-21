
#' Compute the information signature
#'
#' @param x an igraph object
#' @param what graph elements to be removed (i.e. edges or vertices)
#' @param block_size block size required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM)
#' @param offset offset required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM)
#'
#' @details
#'
#' @return
#' @export
#'
#' @examples
#'

info_signature <- function(x, what, block_size = 4, offset = 4) {

    signature <- c()

    if (missing(what) || what == 'edges') {
        edge_deletions_df <- calculate_info_edges(x, block_size, offset)
        # display elements with bdm_difference > 0 (information loss)
        edge_deletions_df <- edge_deletions_df[edge_deletions_df$bdm_difference > 0, ]
        edge_deletions_df <- edge_deletions_df[order(-edge_deletions_df$bdm_difference), ]
        signature <- edge_deletions_df
    } else if (what == 'vertices') {
        vertex_deletions_df <- calculate_info_vertices(x, block_size, offset)
        # display elements with bdm_difference > 0 (information loss)
        vertex_deletions_df <- vertex_deletions_df[vertex_deletions_df$bdm_difference > 0, ]
        vertex_deletions_df <- vertex_deletions_df[order(-vertex_deletions_df$bdm_difference), ]
        signature <- vertex_deletions_df
    } else {
        stop("ERROR: A valid 'what' parameter should be given (edges or vertices).")
    }

    return(signature)
}
