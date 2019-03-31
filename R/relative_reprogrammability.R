


#' Calculate Relative Reprogrammability
#'
#' @param x an igraph object.
#' @param what graph elements to be removed (i.e. edges or vertices).
#' @param block_size block size required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM).
#' @param offset offset required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM).
#'
#' @details The Relative Reprogrammability index measures the shape of a graph's information signature.
#'
#' @import igraph
#' @return A numeric corresponding to the Relative Reprogrammability index of the input graph.
#'
#' @examples
#' \dontrun{
#' robertson_graph <- make_graph("Robertson")
#' relative_reprogrammability(robertson_graph, 'edges')
#' }
#'
#' @export
relative_reprogrammability <- function(x, what, block_size = 4, offset = 4) {

    is <- info_signature(x, what, block_size, offset)

    mad <- median_absolute_deviation(is$bdm_difference)
    maxima <- max(is$bdm_difference)

    if (maxima != 0) {
        return(mad / maxima)
    } else {
        return(0)
    }

}


# Compute the Median Absolute Deviation (MAD)
median_absolute_deviation <- function(x) {
    return(median(abs(x-median(x))))
}
