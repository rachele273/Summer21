#June 2
#Making PCA plots
getwd()
setwd("/Users/racheledidin/Desktop/plink_mac_20210416/")
getwd()
setwd("/Users/racheledidin/Desktop/plink/")
library(tidyverse)
library(readr)
install.packages("ggpubr")
library(ggpubr)
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
#make plot
a <- ggplot(pve, aes(PC, pve))+
  geom_bar(stat="identity")
a + ylab("Percentage variance explained")+
  theme_light()
#calculate cumulative sum of percentage variance explained
cumsum(pve$pve)
#plot pca
b <- ggplot(pca, aes(PC1, PC2, col=loc, shape=spp))+
  geom_point(size=2, alpha=.6, position="jitter")
b <- b+scale_color_manual(values=c("red", "blue"))
b <- b+coord_equal()+theme_light()
b+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
#idk which I like more: the color being location and shape being spp, or vice versa
c <- ggplot(pca, aes(PC1, PC2, col=spp, shape=loc))+
  geom_point(size=1, alpha=.8, position="jitter")
c <- c+scale_color_manual(values=c("red", "blue"))
c <- c+coord_equal()+theme_light()
c+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
#attempt at interactive plot 
install.packages("plotly")
library(plotly)
ggplotly(c)
#jitter
c <- ggplot(pca, aes(PC1, PC2,  color=spp, shape=loc))+
  geom_jitter(size=1, alpha=.8, width=.01)+
  scale_shape_manual(values=c(0, 1, 2))
c <- c+scale_color_manual(values=c("red", "blue"))
c <- c+coord_equal()+theme_light()
c+
  xlab(paste0("PC1 (", signif(pve$pve[1], 3), "%)")) +
  ylab(paste0("PC2 (", signif(pve$pve[2], 3), "%)"))
