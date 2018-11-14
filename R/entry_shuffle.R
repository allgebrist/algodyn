
entry_shuffle <- function(x, row_position, column_position, replacement = NULL) {

    if (!is.null(replacement)) {
        x[row_position, column_position] <- replacement
    }
    return(x)
}
