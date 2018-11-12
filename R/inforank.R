
inforank <- function(x, what = NULL, block_size = NULL, offset = NULL,  base = NULL) {

    inforank_df <- c()

    if (is.igraph(x) || is.matrix(x)) {
        x_adj_matrix <- c()
        if (is.igraph(x)) {
            x_adj_matrix <- as.matrix(as_adjacency_matrix(x))
        } else {
            x_adj_matrix <- x
        }
        if (is.null(block_size) && is.null(offset)) {
            block_size <- 4
            offset <- 4
        } else if (xor(is.null(block_size), is.null(offset))) {
            stop("A block size and offset should be provided to the function.")
        }
        # Perturbations on graphs
        if (what = 'vertices') {
            inforank_df <- calculate_info_vertices(x, block_size, offset)
            inforank_df$inforank <- rank(-as.numeric(inforank_df$bdm_difference), ties.method = "min")
        } else if (what = 'edges' || is.null(what)) {
            inforank_df <- calculate_info_edges(x, block_size, offset)
            inforank_df$inforank <- rank(-as.numeric(inforank_df$bdm_difference), ties.method = "min")
        } else {
            stop("The ranking should be performed according to the contributions of either vertices or edges.")
        }!
    } else if (is.character(x)) {
        # Include here perturbations on strings
        if (is.null(what)) {
            if (!is.null(base)) {
                if (is.null(block_size) && is.null(offset)) {
                    block_size <- 4
                    offset <- 4
                } else if (xor(is.null(block_size), is.null(offset))) {
                    stop("A block size and offset should be provided to the function.")
                }
                # Remove characters
                x_characters <- unlist(str_split(x, pattern = ""))
                inforank_df <- calculate_info_characters(x, block_size, offset, base)
                inforank_df$inforank <- rank(-as.numeric(inforank_df$bdm_difference), ties.method = "min")
            } else {
                stop("The 'base' parameter cannot be ommited.")
            }
        } else {
            stop("Only characters can be removed from the string. Please leave the 'what' parameter unaltered.")
        }
    } else {
        stop("The object should be either a graph, adjacency matrix or string.")
    }

    return(inforank_df)
}
