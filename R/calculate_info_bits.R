
#' Calculate information contribution of bits
#'
#' @param x character string
#' @param block_size block size required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 1-dimensional Block Decomposition Method (BDM)
#' @param offset offset required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 1-dimensional Block Decomposition Method (BDM)
#' @param base number of symbols in the alphabet over which \code{x} is defined
#'
#' @details
#'
#' @return
#' @export
#'
#' @examples
#'

calculate_info_bits <- function(x, block_size, offset, base) {

    x_bdm_value <- bdm1D(x, block_size, offset, base)
    x_char_vector <- unlist(str_split(x, pattern = ""))

    perturbations <- c()

    for (i in 1:length(x_char_vector)) {
        index <- !logical(length(x_char_vector))
        index[i] <- FALSE

        back <- paste(x_char_vector[index], sep = "", collapse = "")
        perturbations <- c(perturbations, back)
    }

    perturbations_bdm_values <- unlist(lapply(perturbations, bdm1D, block_size, offset, base))

    bdm_difference <- x_bdm_value - perturbations_bdm_values

    bdm_df <- data.frame(perturbations = perturbations, bdm_difference = bdm_difference, stringsAsFactors = FALSE)

    return(bdm_df)
}
