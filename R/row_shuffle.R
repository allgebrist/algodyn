
row_shuffle <- function(x, position, replacement) {

    if (!missing(replacement)) {
        if(length(replacement) == nrow(x)) {
            x[position, ] <- replacement
        } else {
            stop("ERROR: The inserted row should have as many columns as the matrix")
        }
    }

    return(x)
}
