plotMutationBoxplots <- function(info.df.seq, fn, quantiles) {

	   info.df.seq30 <- subset(info.df.seq , adjv_cyto=='Yes')
	   info.df.seq30$cellularityBin <- getQuantile(info.df.seq30$ASCAT_TUM_FRAC, quantiles)
	   info.df.seq15 <- subset(info.df.seq , adjv_cyto=='No' )
	   info.df.seq15$cellularityBin <- getQuantile(info.df.seq15$ASCAT_TUM_FRAC, quantiles)

	   pdf(fn, height=10, width=15)
	   par(mfrow=c(2,3)) 
	   boxplot(noSubs ~ cellularityBin, data=info.df.seq15, main='Subs, Patients who did not receive chemo (15x)', ylim=c(0,20000))
	   boxplot(noIndels ~ cellularityBin, data=info.df.seq15, main='Indels, Patients who did not receive chemo (15x)', ylim=c(0,1000))
	   boxplot(noRearrs ~ cellularityBin, data=info.df.seq15, main='Rearrangements, Patients who did not receive chemo (15x)', ylim=c(0,600))
	   boxplot(noSubs ~ cellularityBin, data=info.df.seq30, main='Subs, Patients who received chemo (30x)', ylim=c(0,20000))
	   boxplot(noIndels ~ cellularityBin, data=info.df.seq30, main='Indels, Patients who received chemo (30x)', ylim=c(0,1000))
	   boxplot(noRearrs ~ cellularityBin, data=info.df.seq30, main='Rearrangements, Patients who received chemo (30x)', ylim=c(0,600))
	   dev.off()

}
