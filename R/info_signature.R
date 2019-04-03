


#' Compute the information signature
#'
#' @param x an igraph object.
#' @param what graph elements to be removed (i.e. edges or vertices).
#' @param block_size block size required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM).
#' @param offset offset required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM).
#'
#' @details The information signature \eqn{\sigma(G)} of graph \eqn{G} is the distribution of information values for either the edges or vertices of \eqn{G}, sorted decreasingly by information contribution.
#' Information contributions above and below zero respectively correspond to elements whose removal makes the graph lose or gain information.
#' Neutral elements are, on the other hand, those whose removal has no effect on the graph's information content.
#' This function is simply a ranking of \code{info_spectra}.
#'
#' @import igraph
#' @return A data frame corresponding to the information signature of the input graph.
#'
#' @examples
#' \dontrun{
#' # Compute the information signature of the Folkman graph
#' folkman <- make_graph("Folkman")
#' is <- info_signature(folkman)
#' }
#'
#' @export
info_signature <- function(x, what, block_size = 4, offset = 4) {

    signature <- c()

    if (missing(what) || what == 'edges') {
        signature <- info_spectra(x, what, block_size, offset)
        # Rank according to the information contribution (bdm_difference)
        signature <- signature[order(-signature$bdm_difference), ]
    } else if (what == 'vertices') {
        signature <- info_spectra(x, what, block_size, offset)
        # Rank according to the information contribution (bdm_difference)
        signature <- signature[order(-signature$bdm_difference), ]
    } else {
        stop("ERROR: A valid 'what' parameter should be given (edges or vertices).")
    }

    return(signature)
}
