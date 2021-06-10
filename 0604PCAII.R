#PCA part 2
#June 4
getwd()
setwd("/Users/racheledidin/Desktop/plink/")
library(tidyverse)
library(readr)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
library(plotly)
#read in data
pca <- read_table2("./clarkia.eigenvec", col_names=FALSE)
eigenval <- scan("./clarkia.eigenval")
#sort out data
#remove "nuisance column"
pca <- pca[,-1]
#set names
names(pca)[1] <- "ind"
names(pca)[2:ncol(pca)] <- paste0("PC", 1:(ncol(pca)-1))
#sort out individual species/populations
#spp
spp <- rep(NA, length(pca$ind))
spp[grep("x",pca$ind)] <- "xantiana"
spp[grep("p", pca$ind)] <- "parviflora"
#location
loc <- rep(NA, length(pca$ind))
loc[grep("S", pca$ind)] <- "Sawmill Rd"
loc[grep("S22", pca$ind)] <- "Site 22"
loc[grep("SM", pca$ind)] <- "Squirrel Mountain"
#combine for plotting in different colors
spp_loc <- paste0(spp, "_", loc)
#remake as data frame
pca <- as.tibble(data.frame(pca, spp, loc, spp_loc))
#convert eigenvalues to percentage variance explained (pve)
pve <- data.frame(PC=1:20, pve=eigenval/sum(eigenval)*100)
#make skree plot
a <- ggplot(pve, aes(PC, pve))+
  geom_bar(stat="identity")
a + ylab("Percentage variance explained")+
  theme_light()
#calculate cumulative sum of percentage variance explained
cumsum(pve$pve)
#make plots
#all spp, all locations
p0 <- ggplot(pca, aes(PC1, PC2, col=loc, shape=spp, label=ind))+
  geom_point(size=2, alpha=.6, position="jitter")+
  scale_shape_manual(values=c(0, 1, 2))
p0 <- p0+coord_equal()+theme_light()
p0+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
ggplotly(p0)
#Xantiana, all locations
p1 <- ggplot(pca, aes(PC1, PC2, col=loc, shape=spp))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp=="xantiana"))
p1 <- p1+coord_equal()+theme_light()
p1+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
#Parviflora, all locations
p2 <- ggplot(pca, aes(PC1, PC2, col=loc, shape=spp))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp=="parviflora"))
p2 <- p2+coord_equal()+theme_light()
p2+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
#Sawmill Rd, all spp
p3 <- ggplot(pca, aes(PC1, PC2, col=spp, shape=loc))+
  geom_point(size=2, alpha=.6, data=subset(pca, loc=="Sawmill Rd"))
p3 <- p3+coord_equal()+theme_light()
p3+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
#Site 22, all spp
p4 <- ggplot(pca, aes(PC1, PC2, col=spp, shape=loc))+
  geom_point(size=2, alpha=.6, data=subset(pca, loc=="Site 22"))
p4 <- p4+coord_equal()+theme_light()
p4+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
#Squirrel Mountain, all spp
p5 <- ggplot(pca, aes(PC1, PC2, col=spp, shape=loc))+
  geom_point(size=2, alpha=.6, data=subset(pca, loc=="Squirrel Mountain"))
p5 <- p5+coord_equal()+theme_light()
p5+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
#Sawmill Rd, Xantiana
p6 <- ggplot(pca, aes(PC1, PC2, col=spp, shape=loc))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp_loc=="xantiana_Sawmill Rd"))
p6 <- p6+coord_equal()+theme_light()
p6+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
#Site 22, Xantiana
p7 <- ggplot(pca, aes(PC1, PC2, col=spp, shape=loc))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp_loc=="xantiana_Site 22"))
p7 <- p7+theme_light()
p7+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
#Squirrel Mountain,Xantiana
p8 <- ggplot(pca, aes(PC1, PC2, col=spp, shape=loc))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp_loc=="xantiana_Squirrel Mountain"))
p8 <- p8+coord_equal()+theme_light()
p8+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
#Sawmill Rd, Parviflora
p9 <- ggplot(pca, aes(PC1, PC2, col=spp, shape=loc))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp_loc=="parviflora_Sawmill Rd"))
p9 <- p9+coord_equal()+theme_light()
p9+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
#Site 22, Parviflora
p10 <- ggplot(pca, aes(PC1, PC2, col=spp, shape=loc))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp_loc=="parviflora_Site 22"))
p10 <- p10+coord_equal()+theme_light()
p10+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
#Squirrel Mountain, Parviflora
p11 <- ggplot(pca, aes(PC1, PC2, col=spp, shape=loc))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp_loc=="parviflora_Squirrel Mountain"))
p11 <- p11+theme_light()
p11+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))

#Print plots
grid.arrange(p1, p2)
grid.arrange(p3, p4, p5)
grid.arrange(p6, p7, p8, p9, p10, p11)
#need to work on this...improve aesthetics




