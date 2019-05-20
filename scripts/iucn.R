require('rredlist')

data <- read.csv('../data/20190510-museum-objects.tsv', header=T, sep="\t", skip=1)


get_iucn_data <- function(name) {
    cat("Querying IUCN for ", name, "\n")
    result <- c(iucn_id=NA, iucn_category=NA)
    if (name != "tba" & name != "") {
        s <- rl_search(name)
        if (length(s$result > 0)) {
            result['iucn_id'] <- s$result$taxonid
            result['iucn_category'] <- s$result$category
        }
    }    
    return (result)
}

iucn_data <- do.call(rbind, lapply(as.character(data$SCname), get_iucn_data))
