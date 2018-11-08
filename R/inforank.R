
inforank <- function(x, what = 'default', block_size = 4, offset = 4) {

    inforank_df <- c()

    if(is.igraph(x) || is.matrix(x)) {
        x_adj_matrix <- c()
        if(is.igraph(x)) {
            x_adj_matrix <- as.matrix(as_adjacency_matrix(x))
        } else {
            x_adj_matrix <- x
        }

        # Perturbations on graphs
        if(what = 'vertices') {
            inforank_df <- calculate_info_vertices(x, block_size, offset)
            inforank_df$inforank <- rank(-as.numeric(inforank_df$bdm_difference), ties.method="min")
        } else if(what = 'edges' || what = 'default') {
            inforank_df <- calculate_info_edges(x, block_size, offset)
            inforank_df$inforank <- rank(-as.numeric(inforank_df$bdm_difference), ties.method="min")
        } else {
            stop("The ranking should be performed according to the contributions of either vertices or edges.")
        }

    } else if(is.character(x)) {

        # Include here perturbations on strings
        if(what = 'default') {
            # Remove characters
        } else {
            stop("Only characters can be removed from the string. Please leave the 'what' parameter unaltered.")
        }

    } else {
        stop("The object should be either a graph, adjacency matrix or string.")
    }

    return(inforank_df)

}
