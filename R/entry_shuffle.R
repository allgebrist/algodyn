


#' Entry shuffle
#'
#' @param x
#' @param row_position
#' @param column_position
#' @param replacement
#'
#' @details Replace a matrix entry.
#'
#' @return A matrix with
#'
#' @examples
#' \dontrun{
#' B <- matrix(numeric(15), nrow = 5, ncol = 3, byrow = TRUE)
#' entry_shuffle(B, 3, 2, -1)
#' }
#'
#' @export
entry_shuffle <- function(x, row_position, column_position, replacement) {

    if(is.matrix(x)) {
        if (!missing(replacement)) {
            x[row_position, column_position] <- replacement
        }
    } else {
        stop("ERROR: A valid matrix object must be provided.")
    }

    return(x)
}
