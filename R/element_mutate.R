
element_mutate <- function(x, index, mutations = NULL) {

    x_characters <- unlist(str_split(x, pattern = ""))

    if (!is.null(mutations)) {
        if (length(index) == length(mutations)) {
            for (i in 1:length(index)) {
                x_characters[index[i]] <- mutations[i]
            }
        } else {
            stop("The number of indices and elements to be mutated should be the same.")
        }
    } else {
        # The inserted string is binary
        if (all(x_characters %in% c("0","1"))) {
            for (i in 1:length(index)) {
                x_characters[index[i]] <- as.numeric(!as.integer(x_characters[index[i]]))
            }
        } else {
            stop("A vector of replacement characters should be provided.")
        }
    }

    x_mutated <- paste(x_characters, sep = "", collapse = "")

    return(x_mutated)
}
