


#' Entry shuffle
#'
#' @param x a matrix.
#' @param row_position an integer indicating the row's index.
#' @param column_position an integer indicating the column's index.
#' @param replacement the entry to be inserted into the matrix.
#'
#' @details Replace a matrix entry.
#'
#' @return A matrix with a modified entry.
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
