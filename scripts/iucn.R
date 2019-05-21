require('rredlist')
require('nbaR')

data <- read.csv('../data/20190510-2-museum-objects.tsv', header=T, sep="\t", skip=1)

## sc <- SpecimenClient$new()


get_iucn_data <- function(row) {
    name <- row["SCname"]
    cat("Querying IUCN for ", name, "\n")
    ## parse out characters that make icun api error
    name <- gsub("/", "", name)
    result <- c(iucn_id=NA, iucn_category=NA)
    if (name != "tba" & name != "") {
        s <- rl_search(name)
        if (length(s$result > 0)) {          
            result['iucn_id'] <- s$result$taxonid
            result['iucn_category'] <- s$result$category
            cat("\tFound IUCN data\n")
        } else {
            regnr <- row["Registratienummer"]
            cat("\tIUCN query gives empty result, checking NBA for regnr ", regnr, "\n")
            exists <- specimen_exists(regnr)

            if (exists) {                
                cat("\tRegnr exists, getting name from identifications\n")
                tab <- specimen_find_by_unit_id(regnr)
                cat("\tFound ", length(tab$identifications), " identifications\n)")
                for(i in seq_along(tab$identifications)) {
                    id <- tab$identifications[[i]]
                    new_name <- paste(id$defaultClassification$genus, id$defaultClassification$specificEpithet)
                    cat("\tQuerying IUCN with new name ", new_name, "\n")
                    s <- rl_search(new_name)
                    if (length(s$result > 0)) {          
                        result['iucn_id'] <- s$result$taxonid
                        result['iucn_category'] <- s$result$category
                    }
                }
            }           
        }
    }    
    return (result)
}

iucn_data <- do.call(rbind, apply(data, 1, get_iucn_data))
