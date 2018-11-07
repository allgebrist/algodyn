
inforank <- function(x) {

    if(is.igraph(x) || is.matrix(x)) {
        x_adj_matrix <- c()
        if(is.igraph(x)) {
            x_adj_matrix <- as.matrix(as_adjacency_matrix(x))
        } else {
            x_adj_matrix <- x
        }

        # Include here perturbations on graphs

    } else if(is.character(x)) {

        # Include here perturbations on strings

    } else {
        stop("The object should be either a graph, adjacency matrix or string")
    }

    return(inforank_df)

}
