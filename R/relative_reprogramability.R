


#' Calculate Relative Reprogrammability
#'
#' @param x an igraph object
#' @param what graph elements to be removed (i.e. edges or vertices)
#' @param block_size block size required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM)
#' @param offset offset required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM)
#'
#' @return
#' @export
#'
#' @examples
#'
#'

relative_reprogramability <- function(x, what, block_size = 4, offset = 4) {

  is <- info_signature(x, what, block_size, offset)

  mad <- median_absolute_deviation(is$bdm_difference)
  maxima <- max(is$bdm_difference)

  if (maxima != 0) {
    return(mad / maxima)
  } else {
    return(0)
  }

}



#' Median Absolute Deviation (MAD)
#'
#' @param x numeric vector
#'
#' @return
#' @export
#'
#' @examples
#'
#'

median_absolute_deviation <- function(x) {
    return(median(abs(x-median(x))))
}
