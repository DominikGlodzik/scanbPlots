plotFigure2 <- function(hrdetect.table, fn) {


    hrdetect.table$BRCA.label <- 'aanon'
    hrdetect.table$BRCA.label[ hrdetect.table$isBRCA1Biallelic | hrdetect.table$isBRCA1Monoallelic] <- 'bBRCA1'
    hrdetect.table$BRCA.label[ hrdetect.table$isBRCA2Biallelic | hrdetect.table$isBRCA2Monoallelic] <- 'bBRCA2'
    hrdetect.table$BRCA.label[ hrdetect.table$isPALB2Biallelic | hrdetect.table$isPALB2Monoallelic] <- 'bPALB2'
    
    hrdetect.table$BRCA.status <- hrdetect.table$isBRCA1Monoallelic |
        hrdetect.table$isBRCA1Biallelic |
        hrdetect.table$isBRCA2Monoallelic |
        hrdetect.table$isBRCA2Biallelic |
        hrdetect.table$isPALB2Monoallelic |
        hrdetect.table$isPALB2Biallelic

    
    hrdetect.table <- hrdetect.table[order(hrdetect.table$BRCA.label, hrdetect.table$HRDetect.prob), ]
    hrdetect.table$index <- 1:nrow(hrdetect.table )

    
    pdf(fn, width=20, height=8)

	par(fig=c(0.1,1,0.4,1))
	v <- hrdetect.table$HRDetect.prob
	names(v) <- as.character(hrdetect.table$SangerID.Tumour)
	bp <- barplot(v, col='darkblue', border=NA, main='SCANB',
				  las=2,  cex.names=0.5, ylim=c(0,1.1), las=2, ylab='HRD probability', xlab=paste(nrow(hrdetect.table), 'samples'))


    
	par(fig=c(0.1,1,0,0.4), new=TRUE)
    
    plot(bp[hrdetect.table$isBRCA1Biallelic], rep(5, sum(hrdetect.table$isBRCA1Biallelic) ), col='red', ylim=c(0,6), ,  yaxt="n",  xaxt="n", xlab='', ylab='', pch=19 , xlim=c(0, max(bp)+1))
    points(bp[hrdetect.table$isBRCA1Monoallelic], rep(5, sum(hrdetect.table$isBRCA1Monoallelic) ), col='black', pch=19)

    points(bp[hrdetect.table$isBRCA2Biallelic], rep(4, sum(hrdetect.table$isBRCA2Biallelic) ), col='red', pch=19)
    points(bp[hrdetect.table$isBRCA2Monoallelic], rep(4, sum(hrdetect.table$isBRCA2Monoallelic) ), col='black', pch=19)

    points(bp[hrdetect.table$isPALB2Biallelic], rep(3, sum(hrdetect.table$isPALB2Biallelic) ), col='red', pch=19)
    points(bp[hrdetect.table$isPALB2Monoallelic], rep(3, sum(hrdetect.table$isPALB2Monoallelic) ), col='black', pch=19)
    
	label.text <- c('BRCA1', 'BRCA2','PALB2')
	label.pos <- 5 - (0:(length(label.text)-1))
	#events.brca1 <- subset(event.count, Gene=='BRCA1')
	#plot( bp[hrdetect.table[as.character(events.brca1$Sample),'index'] ],rep(20,nrow(events.brca1)), col=events.brca1$freqLoh, ylim=c(0,20),  yaxt="n",  xaxt="n", xlab='', ylab='', pch=19, xlim=c(0, max(bp)+1))
	#for (gi in 2:length(label.pos)) {
	#	events <- subset(event.count, Gene==label.text[gi])
	#	abline(h=label.pos[gi], col='gray')
	#		points( bp[hrdetect.table[as.character(events$Sample),'index'] ],rep(label.pos[gi],nrow(events)),pch=19, col=events$freqLoh)
	#}
    
    axis(2, at=label.pos,labels=label.text, col.axis="black", las=2)
    dev.off()    
}
