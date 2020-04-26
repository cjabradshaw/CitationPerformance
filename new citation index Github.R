## A new method to rank researchers for citation performance
## Corey J. A. Bradshaw
## April 2020

# call libraries
library(car)

# set working directory
setwd("~/.../") # update to directory with .csv data file

## import
citind <- read.csv("newcitationindexsample.csv", header=T)

# transforms
citind$y.e <- as.numeric(format(Sys.Date(), "%Y")) - citind$firstyrpub
citind$lc10 <- log10(citind$i10)
citind$lch <- log10(citind$h)
citind$lcmax <- log10(1)
citind$li10 <- log10(10)
citind$lih <- log10(citind$h)
citind$limax <- log10(citind$maxcit)
citind$mi <- citind$h/citind$y.e
citind

# power-law relationship
plot(as.numeric(citind[1,11:13]), as.numeric(citind[1,8:10]), lty=2, xlim=c(1,max(citind$limax)), ylim=c(0,max(citind$lc10)), col="white", xlab="log10 index", ylab="log10 frequency")

citind$a <- citind$b <- NA
for (p in 1:dim(citind)[1]) {
  lis <- as.numeric(citind[p, 11:13])
  lcs <- as.numeric(citind[p, 8:10])
  fitp <- lm(lcs ~ lis)
  citind$a[p] <- coef(fitp)[1]
  citind$b[p] <- coef(fitp)[2]
  points(lis,lcs,pch=3,cex=0.5, col="black")
  abline(fitp, lty=2, col="grey")
}
citind

# area under the curve
citind$Alin <- NA
for (q in 1:dim(citind)[1]) {
  li.cont <- seq(1, citind$limax[q], 0.05)
  pred.lin <- citind$a[q] + citind$b[q]*(li.cont)
  citind$Alin[q] <- sum(pred.lin)/(length(li.cont)*(max(citind$limax)))
}
citind

# gender
citindF <- subset(citind, gender=="F")
citindM <- subset(citind, gender=="M")

# histograms
hist(citind$h, main="", xlab="h-index", col="black", border="grey")
hist(citind$mi, main="", xlab="m-index", col="black", border="grey")
hist(citind$Alin, main="", xlab="AUC", col="black", border="grey")

# AUC vs. m-index
plot((citind$mi), citind$Alin, pch=19, xlab="m-index", ylab="AUC")

# residual ranking
plot(log10(citind$y.e), citind$Alin, pch=19, xlab="log10 years since 1st publication", ylab="AUC")
points(log10(citindF$y.e), citindF$Alin, pch=19, col="red")
fit.yAlin <- lm(citind$Alin~log10(citind$y.e))
abline(fit.yAlin, lty=2, col="red")
summary(fit.yAlin)
qqPlot(fit.yAlin, pch=19)
plot(fit.yAlin)

expectation <- ifelse(resid(fit.yAlin) > 0, "above", "below")
dat.out <- data.frame(citind$person, citind$gender, citind$y.e, resid(fit.yAlin), expectation, citind$mi, citind$h)
dat.sort <- dat.out[order(dat.out[,4],decreasing=T),1:7]
dat.sort$rank <- seq(1,length(citind$person),1)
colnames(dat.sort) <- c("person","gender","yrs.publ", "residual","expectation","m-index","h-index", "rank")
dat.sort
