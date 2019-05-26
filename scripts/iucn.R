require('rredlist')
require('nbaR')

data <- read.csv('../data/20190510-2-museum-objects.tsv', header=T, sep="\t", skip=1)
counter <<- 0
sc <- SpecimenClient$new()


get_iucn_data <- function(row) {
    counter <<- counter + 1
    cat("Processing item # ", counter, " of ", nrow(data), "\n")
    name <- unname(row["SCname"])
    regnr <- unname(row["Registratienummer"])
    regnr <- gsub("\\s", "", regnr)
    cat("Querying IUCN for ", name, " regnr: ", regnr, "\n")
    ## parse out characters that make icun api error
    name <- gsub("/", "", name)
    result <- c(sciname=name, regnr=regnr, iucn_id=NA, iucn_category=NA,
                matched_sciname=FALSE, matched_nba_identific=FALSE, matched_col_syn=FALSE, name_tba=FALSE, found=FALSE)
    found <- FALSE
    if (name != "tba" | name != "" | regnr != "") {
        iucn_res <- get_IUCN_for_sciname(name)
        if(! all(is.na(iucn_res))) {
            result[c('iucn_id', 'iucn_category')] <- iucn_res[c('iucn_id', 'iucn_category')]
            result['matched_sciname'] <- TRUE
            cat("\tFound IUCN data\n")
            found <- TRUE
        } else {
            cat("\tIUCN query gives empty result, checking NBA for regnr ", regnr, "\n")
            exists <- specimen_exists(regnr)

            if (exists) {
                cat("\tRegnr exists, getting name from identifications\n")
                ## tab <- specimen_find_by_unit_id(regnr)
                res <- sc$find_by_unit_id(regnr)
                if (length(res$content) < 0) {
                    cat("\tNo specimen found for regnr ", regnr, "\n")
                } else {
                    sp <- res$content[[1]]
                    cat("\tFound ", length(sp$identifications), " identifications\n)")
                    for(i in seq_along(sp$identifications)) {
                        id <- sp$identifications[[i]]
                        new_name <- paste(id$defaultClassification$genus, id$defaultClassification$specificEpithet)
                        cat("\tQuerying IUCN from identifications with name: ", new_name, "\n")
                        if (length(new_name) == 0) {
                            cat("\tName is empty, skipping\n")
                            next
                        }
                        iucn_res <- get_IUCN_for_sciname(new_name)
                        if( ! all(is.na(iucn_res))) {
                            result[c('iucn_id', 'iucn_category')] <- iucn_res[c('iucn_id', 'iucn_category')]
                            result['matched_nba_identific'] <- TRUE
                            cat("Found IUCN data from identification!\n")
                            found <- TRUE
                            break
                        }
                    }
                }
            }
            if (! found) {
                cat("\tStill no record in IUCN found\n")
                cat("\tSearching for sciname in catalogue of life\n")
                synonyms <- get_COL_synonyms(name)
                cat("\tNumber of synonyms: ", length(synonyms), "\n")
                for (s in synonyms) {
                    n <- s$fullScientificName
                    iucn_res <- get_IUCN_for_sciname(n)
                    if( ! all(is.na(iucn_res))) {
                        result[c('iucn_id', 'iucn_category')] <- iucn_res[c('iucn_id', 'iucn_category')]
                        result['matched_col_syn'] <- TRUE
                        cat("Found IUCN data from synonym!\n")
                        found <- TRUE
                        break
                    }
                }

            }
        }
    } else {
        result['name_tba'] <- TRUE
    }
    result['found'] <- found
    cat("Data for Specimen found at IUCN : ", found, "\n\n")

    return (result)
}

get_IUCN_for_sciname<- function(sciname) {
    result <- c(iucn_id=NA, iucn_category=NA)
    s <- rl_search(sciname)
    if (length(s$result > 0)) {
        result['iucn_id'] <- s$result$taxonid
        result['iucn_category'] <- s$result$category
    }
    return(result)
}

get_COL_synonyms <- function(sciname) {
    result <- list()

    ## try to get synonyms from the NBA COL
    tc <- TaxonClient$new()
    qc <- QueryCondition$new(field='acceptedName.fullScientificName',
                             operator='EQUALS',
                             value=sciname)
    qc2 <- QueryCondition$new(field='sourceSystem.code',
                                  operator='EQUALS',
                              value='COL')
    qs <- QuerySpec$new(conditions=list(qc, qc2))
    res <- tc$query(querySpec=qs)
    if (res$content$totalSize > 0) {
        tax <- res$content$resultSet[[1]]$item
        ## cat ("Number of synonyms : ", length(tax$synonyms), "\n")
        syn <- tax$synonyms
        result <- syn

    }
    return(result)
}

##iucn_data <- do.call(rbind, apply(data[1:20,], 1, get_iucn_data))

iucn_data <- t(apply(data, 1, get_iucn_data))

##sum(as.logical(qqq$matched_sciname)) + sum(as.logical(qqq$matched_col_syn)) + sum(as.logical(qqq$matched_nba_identific))
