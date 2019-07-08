## extract registrationnumbers
## from "moederlijst" for queries in CRS

data <- read.csv('../data/20190607-museum-objects.tsv', header=T, sep="\t", skip=1)
regnrs <- as.character(data$Registratienummer)
regnrs <- regnrs[which(regnrs != "")]
## filter out the long ones (doesn't work for query)
regnrs <- regnrs[-which(sapply(regnrs, nchar) > 20)]
regnrs <- regnrs[-which(startsWith(regnrs, "tba"))]
regnrs <- regnrs[-which(startsWith(regnrs, "leen"))]

regnrs <- unique(regnrs)

##
l <- split(regnrs, sort(rep_len(1:4, length(regnrs))))
regnrstr_1 <- paste(paste0("\'", l[[1]], "\'"), collapse=",")
regnrstr_2 <- paste(paste0("\'", l[[2]], "\'"), collapse=",")
regnrstr_3 <- paste(paste0("\'", l[[3]], "\'"), collapse=",")
regnrstr_4 <- paste(paste0("\'", l[[4]], "\'"), collapse=",")

## make query

query <- paste("SELECT s.xmlbeschrijvingid\n",
               "FROM ncrs_specimen s", 
               "where s.registrationnumber IN\n",
               paste("(", regnrstr_1, ")")#,
#              "OR s.registrationnumber IN\n",
#              paste("(", regnrstr_2, ")"),
#              "OR s.registrationnumber IN\n",
#              paste("(", regnrstr_3, ")"), ";"
               )
cat(query, file="../query-1.sql")

query <- paste("SELECT s.xmlbeschrijvingid\n",
               "FROM ncrs_specimen s", 
               "where s.registrationnumber IN\n",
               paste("(", regnrstr_2, ")")
               )
cat(query, file="../query-2.sql")

query <- paste("SELECT s.xmlbeschrijvingid\n",
               "FROM ncrs_specimen s", 
               "where s.registrationnumber IN\n",
               paste("(", regnrstr_3, ")")
               )
cat(query, file="../query-3.sql")

query <- paste("SELECT s.xmlbeschrijvingid\n",
               "FROM ncrs_specimen s", 
               "where s.registrationnumber IN\n",
               paste("(", regnrstr_4, ")")
               )
cat(query, file="../query-4.sql")
