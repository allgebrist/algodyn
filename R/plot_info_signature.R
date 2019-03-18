
plot_info_signature <- function(x, what, block_size = 4, offset = 4) {

    is <- info_signature(x, what, block_size, offset)

    diffs <- c()

    for (i in 1:nrow(is)) {
        if (i != nrow(is)) {
            diffs <- c(diffs, is$bdm_difference[i] - is$bdm_difference[i + 1])
        }
    }

    plot(log(is$bdm_difference) + 80, xlab = "edges sorted by max info value", ylab = "log info values (+80)",
         main = "Information signature", col = "red")
    lines(log(is$bdm_difference) + 80, col = "red")

}

plot_cutting_places <- function(x, what, block_size = 4, offset = 4) {

    is <- info_signature(x, what, block_size, offset)

    diffs <- c()

    for (i in 1:nrow(is)) {
      if (i != nrow(is)) {
        diffs <- c(diffs, is$bdm_difference[i] - is$bdm_difference[i + 1])
      }
    }

    plot(diffs, xlab = "edges sorted by max info value", ylab = "sequential info differences",
         main = "Cutting places", col = "blue")

    lines(diffs, col = "blue")

    curve(log2(2) * x ^ 0, col = "purple", add = TRUE)

    legend("topright", "log(2)", lty = 1, col = "purple")

}
