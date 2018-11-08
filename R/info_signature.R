
info_signature <- function(x, block_size = 4, offset = 4) {

    edge_deletions_df <- calculate_info_edges(x, block_size, offset)

    edge_deletions_df <- edge_deletions_df[edge_deletions_df$information_loss > 0, ]
    edge_deletions_df <- edge_deletions_df[order(-edge_deletions_df$information_loss), ]

    return(edge_deletions_df)

}
