plotCoverageHistogram <- function(info.df, fn) {

breaks=seq(from=0, to=50, by=2)

    
pdf(fn, height=8, width=8)
par(mfrow=c(2,1)) 
info.adj.cyt <- subset(info.df, adjv_cyto=='Yes')
hist(info.adj.cyt$WGS.Tumour.Coverage.X, breaks=breaks, main='Coverage for patient who received adjuvant cytoxic therapy', xlim=c(0,45), border=NA, col='darkgray', xlab='coverage', )
# Act=TRUE
# coverage

info.no.adj.cyt <- subset(info.df, adjv_cyto=='No')
hist(info.no.adj.cyt$WGS.Tumour.Coverage.X,  breaks=breaks, main='Coverage for patient who did NOT received adjuvant cytoxic therapy', xlim=c(0,45), border=NA, col='darkgray', xlab='coverage')
# Act=FALSE
# coverage
dev.off()
}
