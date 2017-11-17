info.df <- read.csv('../samples/SCANB.table.V12.csv')

seqd.df <- subset(info.df, !is.na(WGS.Normal.Coverage.X))
seqd.df$adjv_cyto[is.na(seqd.df$adjv_cyto)] <- 'No'
seqd.df <- seqd.df[order(seqd.df$adjv_cyto, seqd.df$ASCAT_TUM_FRAC),]

                                        # plot a histogram of cellularity
source('utils/plotCoverageHistogram.R')
plotCoverageHistogram(info.df, 'plots/coverageByTherapy.pdf')
    
                                        # plot the horizontal barplots for overview of the cohort: cellularity, coverage etc
source('utils/plotMutBarplots.R')
plotMutBarplots(seqd.df, 'plots/scanb.somatic.summary.pdf')


                                        # plot HRDetect high by cellularity
source('utils/plotHRDetectByCellularity.R')
plotHRDetectByCellularity(info.df, 'plots/HRDvsCellularity.pdf') 

                                        # plot mutation counts by cellularity: subs, indels, rearrangements
source('utils/plotMutationBoxplots.R')
plotMutationBoxplots(seqd.df, 'plots/MutationsVsCellularity.pdf', quantiles=c(0, 0.29, 0.41, 0.54, 1))



                                        # plot HRDetect vs ER percentage
source('utils/plotERbyHRD.R')
plotERbyHRD(info.df, 'plots/ERprocByHRD.pdf')
