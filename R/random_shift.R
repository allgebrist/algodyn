
random_shift <- function(x, position, elements) {

    if (is.matrix(x)) {
        set.seed(123)
        if (missing(elements)) {
            elements <- c(0, 1)
        } else {
            elements <- elements
        }
        new_row <- sample(elements, size = ncol(x), replace = TRUE)
        x[position, ] <- new_row
    }

    return()
}
