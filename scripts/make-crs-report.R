require('jsonlite')

## report voor Ruud en Maarten

tab1 <- read.csv('../data/crs-20-06-19/crs-1.csv', header=T, stringsAsFactors=F)
tab2 <- read.csv('../data/crs-20-06-19/crs-2.csv', header=T, stringsAsFactors=F)
tab3 <- read.csv('../data/crs-20-06-19/crs-3.csv', header=T, stringsAsFactors=F)
tab4 <- read.csv('../data/crs-20-06-19/crs-4.csv', header=T, stringsAsFactors=F)

tab <- rbind(tab1, tab2, tab3, tab4)

regnrs <- unique(tab$REGISTRATIONNUMBER)

l <- lapply(regnrs, function(x){
    res <- list()
    res$REGISTRATIONNUMBER=x
    urls <- tab$URL[which(tab$REGISTRATIONNUMBER==x)]
    res$URLS=urls[-which(urls=="")]
    fsn <- tab$FULLSCIENTIFICNAME[which(tab$REGISTRATIONNUMBER==x)]
    res$FULLSCIENTIFICNAME <- fsn[1]
    return(res)
})

cat(jsonlite::toJSON(l, simplifyVector=T, pretty=T, auto_unbox=T), file="../data/crs-images-2019-20-06.json")

write.table(tab[, c('REGISTRATIONNUMBER', 'FULLSCIENTIFICNAME', 'URL')], file="../data/crs-images-2019-20-06.tsv", sep="\t", quote=F, row.names=F)

