## extract registrationnumbers
## from "moederlijst" for queries in CRS

data <- read.csv('../data/20190429-museum-objects.tsv', header=T, sep="\t", skip=1)
regnrs <- as.character(data$Registrationnumber)
regnrs <- regnrs[which(regnrs != "")]
## filter out the long ones (doesn't work for query)
regnrs <- regnrs[-which(sapply(regnrs, nchar) > 20)]
regnrs <- regnrs[-which(startsWith(regnrs, "tba"))]
regnrs <- regnrs[-which(startsWith(regnrs, "leen"))]

##
l <- split(regnrs, sort(rep_len(1:3, length(regnrs))))
regnrstr_1 <- paste(paste0("\'", l[[1]], "\'"), collapse=",")
regnrstr_2 <- paste(paste0("\'", l[[2]], "\'"), collapse=",")
regnrstr_3 <- paste(paste0("\'", l[[3]], "\'"), collapse=",")

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
