


#' Random shift
#'
#' @param x string or matrix.
#' @param position positions of elements to be replaced.
#' @param seed seed state.
#' @param elements vector corresponding to the alphabet from which the replacement elements are taken from.
#'
#' @details This function replaces one or more bits of a given string or rows of a given matrix with pseudorandom elements from an alphabet.
#' If the alphabet is not specified in advance, the default binary alphabet, consisting of "0" and "1", will be used.
#'
#' @return A string or matrix with pseudorandomly generated replacements of symbols or rows.
#
#' @examples
#' \dontrun{
#' # For a string
#' x <- "hola amigo"
#' random_shift(x, c(2,4,6,8), 157)
#' # For a matrix
#' A <- matrix(seq(1:100), nrow = 10, ncol = 10, byrow = TRUE)
#' random_shift(A, c(1, 3), 56)
#' }
#'
#' @export
random_shift <- function(x, position, seed, elements) {

    set.seed(seed)

    if (missing(elements)) {
        elements <- c(0, 1)
        if (is.character(x)) {
            elements <- unlist(lapply(elements, toString))
        }
    }

    if (is.matrix(x)) {
        new_row <- sample(elements, size = ncol(x), replace = TRUE)
        x[position, ] <- new_row
    } else if (is.character(x)) {
        x <- unlist(str_split(x, pattern = ""))
        new_characters <- sample(elements, size = length(position), replace = TRUE)
        x[position] <- new_characters
        new_characters
    } else {
        stop("ERROR: The object should be either a graph, adjacency matrix or string.")
    }

    return(x)
}
