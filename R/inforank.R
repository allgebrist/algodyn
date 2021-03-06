


#' Algorithmic Information Ranking (InfoRank)
#'
#' @param x string, igraph object or valid adjacency matrix.
#' @param what graph elements to be removed (i.e. edges or vertices).
#' @param block_size block size required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM).
#' @param offset offset required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM).
#' @param base number of distinct characters in \code{x} when \code{x} is a string.
#'
#' @details This function produces a ranking of the input graph's edges from least informative to most informative edge, i.e. a list of edges sorted in increasing order by their information contribution to the graph.
#'
#' @import igraph
#' @import stringr
#' @return A data frame corresponding to a ranking of the elements of the input object according to their information contribution.
#'
#' @examples
#' \dontrun{
#' # For strings
#' inforank("ababbab", base = 2)
#' # For graphs
#' g <- make_graph("Smallestcyclicgroup")
#' inforank(g, what = 'edges')
#' }
#'
#' @export
inforank <- function(x, block_size, offset, base, what) {

    inforank_df <- c()

    if (is.igraph(x) || is.matrix(x)) {
        x_adj_matrix <- c()
        if (is.igraph(x)) {
            x_adj_matrix <- as.matrix(as_adjacency_matrix(x))
        } else {
            x_adj_matrix <- x
        }
        if (missing(block_size) && missing(offset)) {
            block_size <- 4
            offset <- 4
        } else if (xor(missing(block_size), missing(offset))) {
            stop("ERROR: A block size and offset should be provided to the function.")
        }
        # Perturbations on graphs
        if (what == 'vertices') {
            inforank_df <- calculate_info_vertices(x, block_size, offset)
            inforank_df$inforank <- rank(-as.numeric(inforank_df$bdm_difference), ties.method = "min")
        } else if (what == 'edges' || missing(what)) {
            inforank_df <- calculate_info_edges(x, block_size, offset)
            inforank_df$inforank <- rank(-as.numeric(inforank_df$bdm_difference), ties.method = "min")
        } else {
            stop("ERROR: The ranking should be performed according to the contributions of either vertices or edges.")
        }
    } else if (is.character(x)) {
        # Perturbations on strings
        if (missing(what)) {
            if (!missing(base)) {
                if (missing(block_size) && missing(offset)) {
                    block_size <- 12
                    offset <- 11
                } else if (xor(missing(block_size), missing(offset))) {
                    stop("ERROR: A block size and offset should be provided to the function.")
                }
                # Get characters from string
                x_characters <- unlist(str_split(x, pattern = ""))
                inforank_df <- calculate_info_symbols(x, block_size, offset, base)
                inforank_df$inforank <- rank(-as.numeric(inforank_df$bdm_difference), ties.method = "min")
            } else {
                stop("ERROR: The 'base' parameter cannot be ommited.")
            }
        } else {
            stop("ERROR: Only characters can be removed from the string. Please leave the 'what' parameter unaltered.")
        }
    } else {
        stop("ERROR: The object should be either a graph, adjacency matrix or string.")
    }

    return(inforank_df)
}
