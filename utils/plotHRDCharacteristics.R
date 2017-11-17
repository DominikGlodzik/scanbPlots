plotHRDCharacteristics <- function(cohort.df, fn) {

pdf(fn, width=14, height=14)
par(mfrow=c(4,4))

cohort.df$isHRDetectHigh <- cohort.df$HRDetect.prob>0.7

cohort.df$Clinical.TumSize <- as.numeric(cohort.df$Clinical.TumSize)
boxplot(Clinical.TumSize~isHRDetectHigh,
            data=cohort.df, main="tumour size",
            xlab="HRDetect tumour size", ylab="tumour size")

cohort.df$Age <- as.numeric(cohort.df$Age)
      boxplot(Age~isHRDetectHigh,
            data=cohort.df, main="age",
            xlab="HRDetect score and patient age", ylab="age")

cohort.df$Lipid <- as.numeric(cohort.df$Lipid)
      boxplot(Lipid~isHRDetectHigh,
            data=cohort.df, main="Lipid",
            xlab="HRDetect score and Lipid", ylab="Lipid")

cohort.df$ImmuneResponse <- as.numeric(cohort.df$ImmuneResponse)
      boxplot(ImmuneResponse~isHRDetectHigh,
            data=cohort.df, main="ImmuneResponse",
            xlab="HRDetect score and ImmuneResponse", ylab="ImmuneResponse")

cohort.df$Checkpoint <- as.numeric(cohort.df$Checkpoint)
      boxplot(Checkpoint~isHRDetectHigh,
            data=cohort.df, main="Checkpoint",
            xlab="HRDetect score and Checkpoint", ylab="Checkpoint")

cohort.df$Basal <- as.numeric(cohort.df$Basal)
      boxplot(Basal~isHRDetectHigh,
            data=cohort.df, main="Basal",
            xlab="HRDetect score and Basal", ylab="Basal")

cohort.df$Steroid <- as.numeric(cohort.df$Steroid)
      boxplot(Steroid~isHRDetectHigh,
            data=cohort.df, main="Steroid",
            xlab="HRDetect score and Steroid", ylab="Steroid")


cohort.df$ASCAT_TUM_FRAC<- as.numeric(cohort.df$ASCAT_TUM_FRAC)
      boxplot(ASCAT_TUM_FRAC~isHRDetectHigh,
            data=cohort.df, main="ASCAT_TUM_FRAC",
            xlab="HRDetect score and ASCAT_TUM_FRAC", ylab="ASCAT_TUM_FRAC")

cohort.df$BATTENBERG_TUMOUR_FRAC<- as.numeric(cohort.df$BATTENBERG_TUMOUR_FRAC)
      boxplot(BATTENBERG_TUMOUR_FRAC~isHRDetectHigh,
            data=cohort.df, main="BATTENBERG_TUMOUR_FRAC",
            xlab="HRDetect score and BATTENBERG_TUMOUR_FRAC", ylab="BATTENBERG_TUMOUR_FRAC")




    y = xtabs(~ Clinical.Grade + isHRDetectHigh, cohort.df)
    barplot(y/rbind(colSums(y),colSums(y)), main='Grade', legend.text = TRUE, col=terrain.colors(nrow(y)),
    args.legend = list(x = "topleft"))

  y = xtabs(~ IC10 + isHRDetectHigh, cohort.df)
barplot(y/matrix(colSums(y), nrow=nrow(y), ncol=2, byrow=TRUE), main='IC50', legend.text = TRUE, col=terrain.colors(nrow(y)),
    args.legend = list(x = "topleft"))

  y = xtabs(~ AIMS + isHRDetectHigh, cohort.df)
    barplot(y/matrix(colSums(y), nrow=nrow(y), ncol=2, byrow=TRUE), main='AIMS', legend.text = TRUE, col=terrain.colors(nrow(y)),
            args.legend = list(x = "topleft"))

  y = xtabs(~ PAM50_Ref + isHRDetectHigh, cohort.df)
    barplot(y/matrix(colSums(y), nrow=nrow(y), ncol=2, byrow=TRUE), main='PAM50_Ref', legend.text = TRUE, col=terrain.colors(nrow(y)),
            args.legend = list(x = "topleft"))


  y = xtabs(~ Clinical.ER + isHRDetectHigh, cohort.df)
    barplot(y/matrix(colSums(y), nrow=nrow(y), ncol=2, byrow=TRUE), main='Clinical.ER', legend.text = TRUE, col=terrain.colors(nrow(y)),
            args.legend = list(x = "topleft"))



dev.off()

}
