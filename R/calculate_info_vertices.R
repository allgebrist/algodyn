
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
