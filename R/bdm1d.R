

library(acss)

maxKnownKs <- read.csv("data/maxKnownKs.csv")
maxKnownKs$X <- NULL

ldData <- read.csv("data/logicalDepthsBinaryStrings.csv",
                   colClasses = c('character',"numeric"))

colnames(ldData)        <- c('string','ld')
logicalDepths           <- data.frame(ldData$ld)
rownames(logicalDepths) <- ldData$string


count_symbols <- function(x) {
    return(length(table(strsplit(x, NULL))))
}

number_to_binary <- function(x, no_bits) {

    binary_vector = rev(as.numeric(intToBits(x)))

    if (missing(no_bits)) {
        return(binary_vector)
    } else {
        binary_vector[-(1:(length(binary_vector) - no_bits))]
    }
}

get_bits_in_string <-function(x) {

    x <- utf8ToInt(x)

    bit_list <- lapply(x, number2binary, no_bits = 8)
    bit_list2 <- lapply(bit_list, paste0, collapse = "")

    bits_in_string <- paste0(bit_list2, collapse = "")

    return(bits_in_string)
}

split_string <- function(x, block_size, offset) {

    if (block_size > nchar(x)) {
        return(x)
    }

    if (offset > block_size) {
        return ("ERROR: Offset cannot be greater than block size.")
    }

    subs <- character()
    start_indices <- seq(1, nchar(x), offset)

    for (i in start_indices){
        first <- i
        last <- -1

        if (last > nchar(x)) {
            last <- nchar(x) - 1
        } else {
            last <- i + block_size - 1
        }

        sub <- substr(x, first, last)
        subs <- append(subs, sub)

        last_step = FALSE
        if (nchar(sub) == block_size && last == nchar(x)) {
            last_step = TRUE
        }
        if (last_step) {
            break
        }
    }
    return(subs)
}

#receives the already splitted vector of input strings
stringBDM <- function(strings_vector, base) {

    string_counts <- as.data.frame(table(strings_vector))
    string_counts["ks"] <- acss(as.vector(string_counts[["stringsVector"]]), base)[, 1]

    na_indices <- as.integer(which(is.na(string_counts$ks)))
    na_strings <- as.vector(string_counts$strings_vector[na_indices])
    na_lengths <- unlist(lapply(na_strings, nchar))

    # more complex (+1) than the highest known values
    naKs <- maxKnownKs[, paste0("K.", toString(base))] + 1

    string_counts[is.na(string_counts)] <- naKs
    bdm <- sum(log2(string_counts$Freq)) + sum(string_counts$ks)

    return(bdm)
}

#receives the already splitted vector of input strings
stringBDMLD <- function(strings_vector, base) {

    string_counts <- as.data.frame(table(strings_vector))
    bdmld <- sum(logicalDepths[as.character(string_counts$strings_vector), ] * (log2(string_counts$Freq) + 1))

    return(bdmld)
}

# ## should print 80.12 bits
#testBDM <- stringBDM(c("000110100111","111001011000"),2)
#testBDM

# ## should print 1002 steps
# testLD <- stringBDMLD(c("000110100111","111001011000"),2)
# testLD

# ##should print 31 * (log2(3) + 1) = 80.13 steps
#testLD2 <- stringBDMLD(c("010101010101", "010101010101", "010101010101"),2)
#testLD2
