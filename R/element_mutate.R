
element_mutate <- function(x, index) {
    ## a element_mutate le das una coordenada del objeto que quieres mutar,
    ## si es binario le hace flip, si no lo es entonces necesita un input o
    ## lo hace aleatorio. Es básicamente la función que usarías para las
    ## perturbaciones para calculate_info_nodes y la misma para calculate_info_edges.

    x_characters <- unlist(str_split(x, pattern = ""))

    if (all(x_characters %in% c("0","1"))) {
        for (i in 1:length(index)) {
            x_characters[index[i]] <- as.numeric(!as.integer(x_characters[index[i]]))
        }
    }

    x_mutated <- paste(x_characters, sep = "", collapse = "")

    return(x_mutated)
}
