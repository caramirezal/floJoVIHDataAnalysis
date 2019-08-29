## Data analysis on data from flowjo
setwd('~/sc/floJoVIHDataAnalysis/sc/')
library(mice)
library(VIM)
library(dplyr)

vih <- read.table('../data/PolarizationANDcommitmentCD4Tc20180911.csv',
                  sep = '\t', 
                  header = TRUE)

md.pattern(vih)

aggr(vih, 
     col=c('navyblue','yellow'),
     numbers=TRUE, 
     sortVars=TRUE,
     labels=names(vih), 
     cex.axis=.7,
     gap=3, 
     ylab=c("Missing data","Pattern"))



## Getting variables with < 0.3 NAs
naThresholdVar <- sapply(vih, function(x) sum(is.na(x))/nrow(vih) < 0.3)
vihProcessed <- vih[, naThresholdVar]

## dropping ID, Date and ID 
vihProcessed <- vihProcessed[, ! colnames(vihProcessed) %in% c('ID', 
                                                             'Date',
                                                             'IDnumber')]

vihMatrix <- as.matrix(select(vihProcessed, -Group))

cols <- ifelse(as.character(vihProcessed$Group) == ' HIV', 'red', 'green')
heatmap(vihMatrix, RowSideColors = cols)

heatmap(cor(vihMatrix, use = 'complete.obs'), 
        labCol = NA)
