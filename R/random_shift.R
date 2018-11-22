
random_shift <- function(x, position, seed, elements) {

    if (is.matrix(x)) {
        set.seed(seed)
        if (missing(elements)) {
            elements <- c(0, 1)
        } else {
            elements <- elements
        }
        new_row <- sample(elements, size = ncol(x), replace = TRUE)
        x[position, ] <- new_row
    }

    return(x)
}
