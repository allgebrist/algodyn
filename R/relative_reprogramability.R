
#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#'
#'

median_absolute_deviation <- function(x) {
    dist_from_median <- c()
    for(i in 1:length(x)) {
        dist_from_median <- abs(x[i]-median(x))
    }

    return(median(dist_from_median))
}



#' Title
#'
#' @param x
#' @param what
#' @param block_size
#' @param offset
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
