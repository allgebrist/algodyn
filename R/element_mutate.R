
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

# TESTS
# Binary string: element_mutate("1001", c(1,2,3,4))
# Non-binary string: element_mutate("abcd", c(1,2), c("b","a"))
