#May 27
#Plotting practice...Take II 
#(working through tutorial)
library(vcfR)
clarkiamodified <- read.vcfR("Data/clarkiamodified.vcf", verbose = FALSE)
clarkiamodified
head(clarkiamodified)
queryMETA(clarkiamodified, element = 'DP')
#numeric=true because data are numeric
dp3 <- extract.gt(clarkiamodified, element='DP', as.numeric=TRUE)
par(mar=c(8, 4, 1, 1))
boxplot(dp, las=3, col=c("#C0C0C0", "#808080"), ylab="Depth", las=2)
abline(h=seq(0,1e4, by=100), col="#C0C0C088")
#testing with smaller dataset
clarkiatest <- read.vcfR("Data/clarkiamodified.vcf", verbose = FALSE, nrows = 100)
dp2 <- extract.gt(clarkiatest, element='DP', as.numeric=TRUE)
par(mar=c(8, 4, 1, 1))
boxplot(dp2, las=3, col=c("#C0C0C0", "#808080"), ylab="Depth", las=2)
abline(h=seq(0,1e4, by=100), col="#C0C0C088")
dpf2 <- reshape2::melt(dp2, varnames=c('Index', 'Sample'), value.name='Depth', na.rm=TRUE)
library(tidyverse)
library(reshape2)
dpf2 <- dpf2[dpf2$Depth>0,]
ggplot(dpf2, aes(x=Sample, y=Depth))+
  geom_violin(adjust=1.0, scale="count", trim=TRUE)
#going to make dataset with fewer columns for ease of plotting
clarkiatest3 <- read.vcfR("Data/clarkiamodified.vcf", verbose=FALSE)
dp3 <-extract.gt(clarkiatest2, element='DP', as.numeric=TRUE)
head(clarkiatest3)
vcf_file <- system.file("extdata", "pinf_sc50.vcf.gz", package = "pinfsc50")
vcf <- read.vcfR(vcf_file, verbose = FALSE)
dp <- extract.gt(vcf, element='DP', as.numeric=TRUE)
boxplot(dp)
dpf <- reshape2::melt(dp, varnames=c('Index', 'Sample'), value.name='Depth', na.rm=TRUE)
library(tidyverse)
ggplot(dpf, aes(x=Sample, y=Depth))+
  geom_violin()
sums <- apply(dp, MARGIN=2, quantile, probs=c(0.05, 0.95), na.rm=TRUE)
dp2 <- sweep(dp, MARGIN=2, FUN = "-", sums[1,])
dp[dp2 < 0] <- NA
dp2 <- sweep(dp, MARGIN=2, FUN = "-", sums[2,])
dp[dp2 > 0] <- NA
dp[dp < 4] <- NA
par(mar=c(8,4,1,1))
boxplot(dp, las=3, col=c("#C0C0C0", "#808080"), ylab="Depth")
abline(h=seq(0,200, by=20), col="#C0C0C088")
par(mar=c(12,4,4,2))
boxplot(dp, col=2:8, las=3)
title(ylab = "Depth (DP)")
#I think it works. not great with huge file (too many columns) but effective enough ig
