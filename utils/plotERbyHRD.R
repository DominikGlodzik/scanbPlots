plotERbyHRD <- function (info.df, fn) {

# display ER score, for HRD low and HRD high samples
info.df$HRDbin <- NA
info.df$HRDbin[ !is.na(info.df$HRDetect.prob) & info.df$HRDetect.prob <= 0.2] <- 'HRDlow'
info.df$HRDbin[ !is.na(info.df$HRDetect.prob) & info.df$HRDetect.prob > 0.2 & info.df$HRDetect.prob <= 0.7] <- 'HRD0.2to0.7'
info.df$HRDbin[ !is.na(info.df$HRDetect.prob) & info.df$HRDetect.prob > 0.7] <- 'HRDover0.7'
pdf(fn, height=16, width=10)
par(mfrow=c(2,1)) 
boxplot(Clinical.ERperc~HRDbin,data=info.df, main="ER percentage stratified by HRD", 
  	xlab="HRDetect bins", ylab="ER percentage")
  	boxplot(Clinical.ERperc~HRDbin,data=info.df, main="ER percentage stratified by HRD", 
  	xlab="HRDetect bins", ylab="ER percentage", ylim=c(0,10))
dev.off()

}
