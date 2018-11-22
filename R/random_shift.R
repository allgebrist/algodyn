
random_shift <- function(x, position, seed, elements) {

    set.seed(seed)

    if (missing(elements)) {
        elements <- c(0, 1)
    }

    if (is.matrix(x)) {
        new_row <- sample(elements, size = ncol(x), replace = TRUE)
        x[position, ] <- new_row
    } else if (is.character(x)) {
        new_characters <- sample(elements, size = length(position), replace = TRUE)
        x[position] <- new_characters
    } else {
        stop("ERROR: The object should be either a graph, adjacency matrix or string.")
    }

    return(x)
}
