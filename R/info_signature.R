
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
