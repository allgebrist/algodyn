


#' Mutate elements of a string
#'
#' @param x string
#' @param index vector containing the positions of the elements to be replaced
#' @param mutations vector containing the replacements
#'
#' @details
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' # For a binary string
#' element_mutate("1001", c(1,2,3,4))
#' # For a non-binary string:
#' element_mutate("abcd", c(1,2), c("b","a"))
#' }
#'

element_mutate <- function(x, index, mutations) {

    x_characters <- unlist(str_split(x, pattern = ""))

    if (!missing(mutations)) {
        if (length(index) == length(mutations)) {
            for (i in 1:length(index)) {
                x_characters[index[i]] <- mutations[i]
            }
        } else {
            stop("ERROR: The number of indices and elements to be mutated should be the same.")
        }
    } else {
        # The inserted string is binary
        if (all(x_characters %in% c("0", "1"))) {
            for (i in 1:length(index)) {
                x_characters[index[i]] <- as.numeric(!as.integer(x_characters[index[i]]))
            }
        } else {
            stop("ERROR: A vector of replacement characters should be provided.")
        }
    }

    x_mutated <- paste(x_characters, sep = "", collapse = "")

    return(x_mutated)
}

