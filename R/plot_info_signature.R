
plot_info_signature <- function(x, block_size = 4, offset = 4) {

    is <- info_signature(x, block_size, offset)

    diffs <- c()

    for(i in 1:nrow(is)) {
        if(i != nrow(is)) {
            diffs <- c(diffs, is$bdm_difference[i]-is$bdm_difference[i+1])
        }
    }

    plot(diffs, xlab = "edges sorted by max info value", ylab = "sequential info differences",
         main = "Cutting points plot", col = "blue")

    lines(diffs, col = "blue")

    curve(log2(2)*x^0, col = "purple", add = TRUE)

    legend("topright", "log(2)", lty = 1, col = "purple")

}
