

plotHRDetectByCellularity <- function(info.df, fn) {

# divide samples into 4 bins by cellularity x coverage
    info.df$cell.x.cov <- info.df$ASCAT_TUM_FRAC # * info.df$ASCAT_TUM_FRAC
    info.df.adj <- subset(info.df, adjv_cyto=='No' & !is.na(noSubs))

pdf(fn, height=12, width=8)
par(mfrow=c(2,1))
info.df.seq <- subset(info.df, !is.na(noSubs) & !isWGSfail)
plotHRbyCellularity(subset(info.df.seq , adjv_cyto=='No' |  is.na( adjv_cyto)), 'patients who did not received chemotherapy (15x)', c(0, 0.29, 0.41, 0.54, 1))
plotHRbyCellularity(subset(info.df.seq , adjv_cyto=='Yes'), 'patients who did  received chemotherapy (30x)', c(0, 0.29, 0.41, 0.54, 1))
dev.off()

}

getQuantile <- function(v, quantiles) {
	binV <- rep('4thQuantile', length(v))
	binV[v < quantiles[4]] <- '3rdQuantile'
	binV[v < quantiles[3]] <- '2ndQuantile'
	binV[v < quantiles[2]] <- '1stQuantile'
	binV	
}

plotHRbyCellularity <- function(info.df.adj, maint, quantiles=NULL) {

    if (is.null(quantiles)) {
    quantiles <- c(min(info.df.adj$cell.x.cov),
                   quantile(info.df.adj$cell.x.cov,prob=0.25, na.rm=TRUE),
                   quantile(info.df.adj$cell.x.cov,prob=0.5, na.rm=TRUE),
                   quantile(info.df.adj$cell.x.cov,prob=0.75, na.rm=TRUE),
                   max(info.df.adj$cell.x.cov)
                   )
    }
    info.df.adj$cellularityXcoverageBin <- getQuantile(info.df.adj$cell.x.cov, quantiles)
    quantile.splits <- round(quantiles,2)

    ms <- split(info.df.adj, info.df.adj$cellularityXcoverageBin)
    bin1.highHRD <- sum(ms[[1]]$HRDetect.prob > 0.7)
    bin2.highHRD <- sum(ms[[2]]$HRDetect.prob > 0.7)
    bin3.highHRD <- sum(ms[[3]]$HRDetect.prob > 0.7)
    bin4.highHRD <- sum(ms[[4]]$HRDetect.prob > 0.7)
    v <- c(bin1.highHRD, bin2.highHRD, bin3.highHRD, bin4.highHRD)
    
    names(v) <- c(paste(quantile.splits[1], quantile.splits[2], sep='..'),
                  paste(quantile.splits[2], quantile.splits[3], sep='..'),
                  paste(quantile.splits[3], quantile.splits[4], sep='..'),
                  paste(quantile.splits[4], quantile.splits[5], sep='..'))

    no.samples <- paste ('\n (', sapply(ms, nrow), 'samples )')
    names(v) <- paste0(names(v), no.samples )
    
    barplot(v/sapply(ms, nrow), col='darkblue', border=NA, 
            ylab='Number of HRDetect high samples', xlab='Cellularity bin',
            main=paste(nrow(info.df.adj), maint))		
}
