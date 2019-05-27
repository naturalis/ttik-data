require('rredlist')
require('nbaR')

data <- read.csv('../data/20190510-2-museum-objects.tsv', header=T, sep="\t", skip=1)[672:3597,]
sc <- SpecimenClient$new()

get_IUCN_for_sciname <- function(sciname) {
    result <- c(iucn_id=NA, iucn_category=NA)

    tryCatch({
        s <- rl_search(sciname) 
        if (length(s$result > 0)) {
            result['iucn_id'] <- s$result$taxonid
            result['iucn_category'] <- s$result$category
        }
    }, error=function(cond) {
        message(cond, "\n")
    })
        
    return(result)
}

get_all_scinames <- function(regnr, sciname) {
    cat("Collecting more sc. names for name: ", sciname, " regnr: ", regnr, "\n")
    scinames <- c(sciname)
    sc <- SpecimenClient$new()

    ## First, lookup the registration number
    ## and collect scientific names from identifications
    exists <- specimen_exists(regnr)        
    if (! is.logical(exists)) {
        exists <- FALSE
    }
    
    if (exists) {
        res <- sc$find_by_unit_id(regnr)
        if (length(res$content) < 0) {
            cat("No specimen found for regnr ", regnr, "\n")
        } else {
            sp <- res$content[[1]]
            cat("Found ", length(sp$identifications), " identifications\n)")
            for(i in seq_along(sp$identifications)) {
                id <- sp$identifications[[i]]
                name <- paste(id$defaultClassification$genus, id$defaultClassification$specificEpithet)
                scinames <- c(scinames, name)                
                ## add also name from ScientificName field
                sn <- id$scientificName$fullScientificName
                if (sn != "") {
                    scinames <- c(scinames, sn)
                }
            }
        }
    }

    ## Second, look for all synonyms for the scinames found in COL,
    ## and return also these scinames
    scinames_col <- c()
    for (sc in scinames) {
        scinames_col <- c(scinames_col, get_scinames_from_COL_synonyms(sc))        
    }
    ## remove first one as this was the argument    
    result <- unique(c(scinames, scinames_col))[-1]

    return(result)
}

get_scinames_from_COL_synonyms <- function(sciname) {
    result <- c()

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
        for (syn in tax$synonyms) {
            result <- c(result, syn$fullScientificName)
            ## also store Genus,Species
            name <- paste(syn$genusOrMonomial, syn$specificEpithet)
            result <- c(result, name)
        }        
    }
    return(unique(result))
}

##iucn_data <- do.call(rbind, apply(data[1:20,], 1, get_iucn_data))

## iucn_data <- t(apply(data, 1, get_iucn_data))

##sum(as.logical(qqq$matched_sciname)) + sum(as.logical(qqq$matched_col_syn)) + sum(as.logical(qqq$matched_nba_identific))
## dd <- apply(data[10:100,], 1, function(row){
##    get_all_scinames(row['Registratienummer'], row['SCname'])
##})


data[,"iucn_id"] <- NA
data[,"iucn_category"] <- NA
data[,"found_via_nba"] <- FALSE

for (i in seq_along(rownames(data))) {
    cat("Processing #", i, " of ", nrow(data), "\n")

    row <- data[i,]
    sciname <- as.character(row$SCname)
    sciname <- gsub('/', '', sciname)
    regnr <- as.character(row$Registratienummer)
    regnr <- gsub("\\s", "", regnr)
    
    cat("Regnr: ", regnr, " sciname: ", sciname, "\n")    

    ## query IUCN with name first
    cat("Trying to retrieve IUCN data for sciname ", sciname, "\n")
    
    iucn_res <- get_IUCN_for_sciname(sciname)
            
    if(! all(is.na(iucn_res))) {
        cat("Found sciname via IUCN API\n")        
        data[, c("iucn_id", "iucn_category")] <- iucn_res[c("iucn_id", "iucn_category")]
    } else {
        cat("IUCN data for sciname ", sciname, "not found, looking for alternative names in NBA\n")
        nba_names <- get_all_scinames(regnr, sciname)
        cat("Found ", length(nba_names), " more names in NBA\n")

        for (sc in nba_names) {
            cat("Searching in IUCN for name ", sc, "\n")
            iucn_res <- get_IUCN_for_sciname(sc)            
            if(! all(is.na(iucn_res))) {
                cat("Found sciname via IUCN API\n")        
                data[, c("iucn_id", "iucn_category")] <- iucn_res[c("iucn_id", "iucn_category")]
                data[,"found_via_nba"] <- TRUE
                break
            }        
        }                
    }
    cat("\n\n")
}
