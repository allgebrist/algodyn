
entry_shuffle <- function(x, row_position, column_position, replacement) {

    if (!missing(replacement)) {
        x[row_position, column_position] <- replacement
    }

    return(x)
}
