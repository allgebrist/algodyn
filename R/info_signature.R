
info_signature <- function(x, block_size = 4, offset = 4) {

    edge_deletions_df <- calculate_info_edges(x, block_size, offset)

    # display elements with bdm_difference > 0 (information loss)
    edge_deletions_df <- edge_deletions_df[edge_deletions_df$bdm_difference > 0, ]
    edge_deletions_df <- edge_deletions_df[order(-edge_deletions_df$bdm_difference), ]

    return(edge_deletions_df)

}
