


#' Entry shuffle
#'
#' @param x
#' @param row_position
#' @param column_position
#' @param replacement
#'
#' @details
#'
#' @return
#' @export
#'
#' @examples
#'

entry_shuffle <- function(x, row_position, column_position, replacement) {

    if (!missing(replacement)) {
        x[row_position, column_position] <- replacement
    }

    return(x)
}
