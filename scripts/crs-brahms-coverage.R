## check coverage of museum object list in CRS/BRAHMS
require('nbaR')

data <- read.csv('../data/20190510-museum-objects.tsv', header=T, sep="\t", skip=1)
regnrs <- as.character(data$Registratienummer)

## get also CRS ids from previous file, maybe some are not yet in the NDS
crsdata <- read.csv('../data/museum-objects-crs-images.tsv', header=T, sep="\t")
regnrs_crs <- crsdata$REGISTRATIONNUMBER

regnr_in_crs <- function(x) {
    if (x=="") 
        FALSE
    else        
        x %in% regnrs_crs
}

regnr_in_nds <- function(x) {
    if (x=="") 
        FALSE
    else
        specimen_exists(x)
}
    
in_crs <- sapply(regnrs, regnr_in_crs)
in_nds <- sapply(regnrs, regnr_in_nds)

## write to table with regist
df <- data.frame(Registratienummer=regnrs,
                 Zaal=data$Zaal,
                 Zaaldeel=data$Zaaldeel,
                 SCname=data$SCname,
                 NLname=data$NLname,
                 Locatie=data$Locatie,
                 Foto=data$Foto,
                 present_in_crs=in_crs,
                 present_in_nds=in_nds)

write.table(df, row.names=F, quote=F, file="../data/crs-brahms-coverage.tsv", sep="\t")
