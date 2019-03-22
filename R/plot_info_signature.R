


#' Plot information signature
#'
#' This function plots the information signature of a given graph.
#'
#' @param x an igraph object
#' @param what graph elements to be removed (i.e. edges or vertices)
#' @param block_size block size required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM)
#' @param offset offset required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM)
#'
#' @details
#'
#' @export
#'
#' @examples
#' \dontrun{
#' require(igraph)
#' frucht_graph = make_graph('Frucht')
#' plot_info_signature(frucht_graph, 'edges')
#' }
#'

plot_info_signature <- function(x, what, block_size = 4, offset = 4) {

    is <- info_signature(x, what, block_size, offset)
    # We only take into account those elements with bdm_difference > 0 (see info_signature.R)
    plot(log(is$bdm_difference), xlab = "edges sorted by max info value", ylab = "log info values",
         main = "Information signature", col = "red")

    lines(log(is$bdm_difference), col = "red")

}



#' Plot the cutting places
#'
#' This function plots the differences of consecutive values of the information signature
#'
#' @param x an igraph object
#' @param what graph elements to be removed (i.e. edges or vertices)
#' @param block_size block size required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM)
#' @param offset offset required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM)
#'
#' @details
#'
#' @export
#'
#' @examples
#' \dontrun{
#' require(igraph)
#' meredith = make_graph('Meredith')
#' plot_cutting_places(make_graph('Meredith'), 'vertices')
#' }
#'

plot_cutting_places <- function(x, what, block_size = 4, offset = 4) {

    is <- info_signature(x, what, block_size, offset)

    diffs <- c()

    for (i in 1:nrow(is)) {
      if (i != nrow(is)) {
        diffs <- c(diffs, is$bdm_difference[i] - is$bdm_difference[i + 1])
      }
    }

    plot(diffs, xlab = "edges sorted by max info value", ylab = "sequential info differences",
         main = "Cutting places", col = "blue")

    lines(diffs, col = "blue")

    curve(log2(2) * x ^ 0, col = "purple", add = TRUE)

    legend("topright", "log(2)", lty = 1, col = "purple")

}
