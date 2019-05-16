tab1 <- read.csv('../data/hannes-ttik-1.csv', header=T, stringsAsFactors=F)
tab2 <- read.csv('../data/hannes-ttik-2.csv', header=T, stringsAsFactors=F)
tab3 <- read.csv('../data/hannes-ttik-3.csv', header=T)

tab <- rbind(tab1, tab2, tab3)

##moeder <- data.frame(read.csv('../data/20190429-museum-objects.tsv', header=T, sep="\t", skip=1))

##for (c in colnames(tab)) {
##    moeder[,c] <- ""
##}

##for (i in seq_len(nrow(tab))) {
##    id <- tab$REGISTRATIONNUMBER[i]
##    row <- which(as.character(moeder$Registrationnumber) == id)

##    for (col in colnames(tab)) {
##        moeder[row, col] <- as.character(tab[i,col])
##    }
##}


##write.table(moeder, row.names=F, file="../data/20190429-museum-objects-images.tsv", sep="\t", quote=F)

#missing <- moeder$Registrationnumber[which(! moeder$Registrationnumber %in% tab$REGISTRATIONNUMBER)]

#for (i in seq_along(missing)) {
#    tab[i, 'REGISTRATIONNUMBER'] <- missing[i]
                                        #}

tab <- tab[order(trimws(tab$GENUS), trimws(tab$EPITHET), trimws(tab$PRESERVEDPART), trimws(tab$REGISTRATIONNUMBER),  trimws(tab$URL)), ]
tab <- tab[, c("REGISTRATIONNUMBER", "INST_COLL_SUBCOLL", "TRANSFERREASON", "FULLSCIENTIFICNAME", "GENUS", "EPITHET", "PRESERVEDPART", "ICOONURL", "URL")]
write.table(tab, row.names=F, file="../test.tsv", sep="\t")
