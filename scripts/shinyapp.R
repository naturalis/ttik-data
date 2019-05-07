tab1 <- read.csv('../data/hannes-ttik-1.csv', header=T)
tab2 <- read.csv('../data/hannes-ttik-2.csv', header=T)
tab3 <- read.csv('../data/hannes-ttik-3.csv', header=T)

tab <- rbind(tab1, tab2, tab3)
t <- tab %>%
    group_by(REGISTRATIONNUMBER) %>%
    summarize(FULLSCIENTIFICNAME=first(FULLSCIENTIFICNAME),
              GENUS=first(GENUS),
              EPITHET=first(EPITHET),
              TRANSFERREASON=first(TRANSFERREASON),
              ICOONURL=paste(ICOONURL, collapse=","),
              URL=paste(URL, collapse=",")              
              )


qq = tab %>% group_by(REGISTRATIONNUMBER) %>% summarize(URL=sort(toString(unique(URL))))


## summarize
##regnrs <- unique(tab$REGISTRATIONNUMBER)
##idx <- sapply(regnrs, function(r)which(tab$REGISTRATIONNUMBER==r)[1])

##lapply(regnrs, function(x){
    
##})



shinyApp(
    ui = fluidPage(
        fluidRow(
            column(12,
                   tableOutput('table')
                   )
        )
    ),
    server = function(input, output) {
    output$table <- renderDataTable(data.table(qq))
}
)
