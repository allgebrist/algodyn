


#' Calculate the Kolmogorov-Complexity of a matrix
#'
#' @param mat a valid matrix
#' @param block_size block size required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM)
#' @param offset offset required to estimate the Kolmogorov-Chaitin complexity of \code{x} using the 2-dimensional Block Decomposition Method (BDM)
#'
#' @details This function gives an estimation of the Kolmogorov-Chaitin complexity of a matrix using the 2-dimensional Block Decomposition Method (BDM).
#'
#' @return A number corresponding to the estimated Kolmogorov-Chaitin complexity of the input matrix.
#'
#' @examples
#' \dontrun{
#' require(purrr)
#' set.seed(42)
#' m99 <- apply(matrix(0, 9, 9), c(1,2), function(x) sample(c(0,1),1))
#' bdm2D(m99, 3, 3)
#' }
#'
#' @export
bdm2D <- function(mat, block_size, offset) {

    parts <- my_partition(mat, block_size, offset)
    flat_squares <- unlist(lapply(parts, stringify))

    squares_tally <- as.data.frame(table(flat_squares))
    rownames(squares_tally) <- squares_tally$flat_squares
    squares_tally$flat_squares <- NULL

    if (block_size == 4) {
        bdm <- (sum(four_by_four_ctm[rownames(squares_tally), ])
                + sum(log2(squares_tally$Freq)))
    } else {
        bdm <- (sum(three_by_three_ctm[rownames(squares_tally), ])
                + sum(log2(squares_tally$Freq)))
    }

    return(bdm)
}


# Used to look up entries in the tables
# four_by_four_ctm and three_by_three_ctm
stringify <- function(small_block) {
    paste0(c(t(small_block)), collapse = "")
}


# This is a helper function that generates subset indexing
# of the matrix according to its dimension
my_partition <- function(mat, block_size, offset) {

    lapply(cross2(ind(nrow(mat), block_size, offset),
                  ind(ncol(mat), block_size, offset)),
           function(i) mat[i[[1]], i[[2]]])
}


# Split matrices in blocks
ind <- function(mat_dim, block_size, offset) {

    Map(`:`, seq(1, mat_dim - block_size + 1, by = offset),
        seq(block_size, mat_dim, by = offset))
}


# Load 3 x 3 CTM values
three_by_three_ctm <- read.csv("data/K-3x3.csv",
                               stringsAsFactors = FALSE,
                               colClasses = c("character", "numeric"),
                               header = FALSE)

colnames(three_by_three_ctm) <- c("square", "CTM")
rownames(three_by_three_ctm) <- three_by_three_ctm$square
three_by_three_ctm$square <- NULL


# Load 4 x 4 CTM values
four_by_four_ctm <- read.csv("data/K-4x4.csv",
                             stringsAsFactors = FALSE,
                             colClasses = c("character", "numeric"),
                             header = FALSE)

colnames(four_by_four_ctm) <- c("square", "CTM")
rownames(four_by_four_ctm) <- four_by_four_ctm$square
four_by_four_ctm$square <- NULL
