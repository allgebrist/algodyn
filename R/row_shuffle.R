
row_shuffle <- function(x, row_position, replacement = NULL) {

    if (!is.null(replacement)) {
        if(length(replacement) == nrow(x)) {
            x[row_position, ] <- replacement
        } else {
            stop("ERROR: The inserted row should have as many columns as the matrix")
        }
    }

    return(x)
}
