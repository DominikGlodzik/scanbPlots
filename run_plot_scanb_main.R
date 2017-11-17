info.df <- read.csv('data/SCANB.table.V12.csv', stringsAsFactors=FALSE)
cohort.df <- subset(info.df, !isWGSfail & !is.na(info.df$WGS.Normal.Coverage.X ) )


# Figure 2: HRDetect distribution and characterstics of sample groups
source('utils/plotFigure2.R')
plotFigure2(cohort.df, 'plots/Figure2.pdf')

# Figure 3: 
source('utils/plotHRDCharacteristics.R')
plotHRDCharacteristics(cohort.df, 'plots/Figure3.pdf') 
