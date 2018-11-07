
info_signature <- function(x, block_size = 4, offset = 4) {

    x_adj_matrix <- c()

    if(is.igraph(x)) {
        x_adj_matrix <- as.matrix(as_adjacency_matrix(x))
    } else if(is.matrix(x)) {
        x_adj_matrix <- x
    } else {
        stop("The object should be a graph or adjacency matrix")
    }

    x_bdm_value <- bdm2D(x_adj_matrix, block_size, offset)

    edge_deletions_df <- as_data_frame(x, what = "edges")
    deletion_columns  <- c("bdm_value", "information_loss")

    edge_deletions_df[, deletion_columns] <- NA

    for(i in 1:nrow(edge_deletions_df)) {

        x_deleted_edge  <- delete_edges(x, paste0(edge_deletions_df[i, ]$from,
                                                   "|",edge_deletions_df[i, ]$to))

        deleted_edge_matrix <- as.matrix(as_adjacency_matrix(x_deleted_edge))

        deleted_edge_bdm_value <- bdm2D(deleted_edge_matrix, block_size, offset)


        edge_deletions_df[i, ]$bdm_value <- deleted_edge_bdm_value
        edge_deletions_df[i, ]$information_loss <- x_bdm_value - deleted_edge_bdm_value

    }

    edge_deletions_df <- edge_deletions_df[edge_deletions_df$information_loss > 0, ]
    edge_deletions_df <- edge_deletions_df[order(-edge_deletions_df$information_loss), ]

    return(edge_deletions_df)

}
