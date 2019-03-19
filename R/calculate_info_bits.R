
#' Calculate information contribution of bits
#'
#' @param x character string
#' @param block_size block size required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 1-dimensional Block Decomposition Method (BDM)
#' @param offset offset required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 1-dimensional Block Decomposition Method (BDM)
#' @param base
#'
#' @return
#' @export
#'
#' @examples

calculate_info_bits <- function(x, block_size, offset, base) {

    k_values <- get_k_values(alphabet_size = base)
    x_bdm_value <- bdm1D(x, block_size, offset, base, k_values)
    x_char_vector <- unlist(str_split(x, pattern = ""))

    perturbations <- c()

    for (i in 1:length(x_char_vector)) {
        index <- !logical(length(x_char_vector))
        index[i] <- FALSE

        back <- paste(x_char_vector[index], sep = "", collapse = "")
        perturbations <- c(perturbations, back)
    }

    perturbations_bdm_values <- unlist(lapply(perturbations, bdm1d, block_size, offset, base, k_values))

    bdm_differences <- x_bdm_value - perturbations_bdm_values

    bdm_df <- data.frame(perturbations = perturbations, bdm_differences = bdm_differences, stringsAsFactors = FALSE)

    return(bdm_df)
}
