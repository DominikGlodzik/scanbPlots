plotMutBarplots <- function(seqd.df, fn) {


pdf(fn, width=20, height=25)
par(fig=c(0,1,0.85,1))
v <- seqd.df$noSubs
#names(v) <- seqd.df$SangerID.Tumour 
barplot(v, main='Number of substitutions per sample', border=NA, las=2, log="y", col='darkblue', xlab=paste(length(v), 'samples'))
abline(h=1000)

par(fig=c(0,1,0.75,0.85), new=TRUE)
v <- seqd.df$noRearrs
barplot(v, main='Number of rearrangements per sample', border=NA, las=2, log="y", col='darkblue')


par(fig=c(0,1,0.65,0.75), new=TRUE)
v <- seqd.df$noIndels+1
barplot(v, main='Number of indels per sample', border=NA, las=2, log="y", col='darkblue')

par(fig=c(0,1,0.55,0.65), new=TRUE)
v <- seqd.df$HRDetect.prob
barplot(v, main='HRDetect', border=NA, las=2, col='darkblue')



par(fig=c(0,1,0.45,0.55), new=TRUE)
v <- seqd.df$WGS.Tumour.Coverage.X
barplot(v, main='Tumour Coverage (X)', border=NA, las=2,  col='darkred')

par(fig=c(0,1,0.35,0.45), new=TRUE)

isAfail <- !is.na(seqd.df$ASCAT.fail) & seqd.df$ASCAT.fail==1
v <- seqd.df$ASCAT_TUM_FRAC
bp <- barplot(v, main='Ascat tumour fraction', border=NA, las=2,  col='darkred', ylim=c(0,1))
text(bp[isAfail], v[isAfail],labels=rep('F', sum(isAfail)), pos=3)


par(fig=c(0,1,0.25,0.35), new=TRUE)
v <- seqd.df$BATTENBERG_TUMOUR_FRAC
barplot(v, main='Battenberg tumour fraction', border=NA, las=2,  col='darkred', ylim=c(0,1))

par(fig=c(0,1,0.15,0.25), new=TRUE)
v <- seqd.df$prop.sub.germ
barplot(v, main='Proportion of subs likely germline (dbSNP, 1000genomes, EcAC)', border=NA, las=2,  col='darkgreen', ylim=c(0,0.5))

par(fig=c(0,1,0.05,0.15), new=TRUE)
v <- seqd.df$prop.indel.germ
barplot(v, main='Proportion of indels likely germline (dbSNP, 1000genomes, EcAC)', border=NA, las=2,  col='darkgreen', ylim=c(0,0.5))




dev.off()
}
