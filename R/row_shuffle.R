
row_shuffle <- function(x, row_position, replacement = NULL) {

    if (!is.null(replacement)) {
        if(length(replacement) == nrow(x)) {
            x[row_position, ] <- replacement
            return(x)
        } else {
            stop("ERROR: The inserted row should have as many columns as the matrix")
        }
    } else {
        return(x)
    }
}
