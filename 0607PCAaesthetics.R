#June 7
#working on PCA display
setwd("/Users/racheledidin/Desktop/plink/")
library(tidyverse)
library(readr)
library(gridExtra)
library(ggpubr)
library(plotly)
pca <- read_table2("./clarkia.eigenvec", col_names=FALSE)
eigenval <- scan("./clarkia.eigenval")
pca <- pca[,-1]
names(pca)[1] <- "ind"
names(pca)[2:ncol(pca)] <- paste0("PC", 1:(ncol(pca)-1))
spp <- rep(NA, length(pca$ind))
spp[grep("x",pca$ind)] <- "xantiana"
spp[grep("p", pca$ind)] <- "parviflora"
loc <- rep(NA, length(pca$ind))
loc[grep("S", pca$ind)] <- "Sawmill Rd"
loc[grep("S22", pca$ind)] <- "Site 22"
loc[grep("SM", pca$ind)] <- "Squirrel Mountain"
spp_loc <- paste0(spp, "_", loc)
pca <- as.tibble(data.frame(pca, spp, loc, spp_loc))
pve <- data.frame(PC=1:20, pve=eigenval/sum(eigenval)*100)
#Xantiana, all locations
p1 <- ggplot(pca, aes(PC1, PC2, col=loc, label=ind))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp=="xantiana"))
p1 <- p1+coord_equal()+theme_light()
p1 <- p1+
  xlab(paste0("PC1")) +
  ylab(paste0("PC2"))+
  ggtitle("Xantiana")
ggplotly(p1)
p1
#Parviflora, all locations
p2 <- ggplot(pca, aes(PC1, PC2, col=loc, label=ind))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp=="parviflora"))
p2 <- p2+coord_equal()+theme_light()
p2+
  xlab(paste0("PC1")) +
  ylab(paste0("PC2"))
ggplotly(p2)
#Display location plots
figure1 <- ggarrange(p1, p2, labels=c("Xantiana", "Parviflora"), nrow=1, common.legend=TRUE, legend="bottom")
figure1
#Sawmill Rd, all spp
p3 <- ggplot(pca, aes(PC1, PC2, col=spp, label=ind))+
  geom_point(size=2, alpha=.6, data=subset(pca, loc=="Sawmill Rd"))
p3 <- p3+coord_equal()+theme_light()
p3 <- p3+
  xlab(paste0("PC1")) +
  ylab(paste0("PC2")) +
  ggtitle(label="Sawmill Rd")
ggplotly(p3)
#Site 22, all spp
p4 <- ggplot(pca, aes(PC1, PC2, col=spp, label=ind))+
  geom_point(size=2, alpha=.6, data=subset(pca, loc=="Site 22"))
p4 <- p4+coord_equal()+theme_light()
p4 <- p4+
  xlab(paste0("PC1")) +
  ylab(paste0("PC2")) +
  ggtitle("Site 22")
ggplotly(p4)
#Squirrel Mountain, all spp
p5 <- ggplot(pca, aes(PC1, PC2, col=spp, label=ind))+
  geom_point(size=2, alpha=.6, data=subset(pca, loc=="Squirrel Mountain"))
p5 <- p5+coord_equal()+theme_light()
p5 <- p5+
  xlab(paste0("PC1")) +
  ylab(paste0("PC2"))+
  ggtitle("Squirrel Mountain")
ggplotly(p5)
#Display species plots
figure2 <- ggarrange(p3, p4, p5, labels=c("Sawmill Rd", "Site 22", "Squirrel Mountain"), nrow=1, common.legend=TRUE, legend="bottom")
figure2
#Sawmill Rd, Xantiana
p6 <- ggplot(pca, aes(PC1, PC2, label=ind))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp_loc=="xantiana_Sawmill Rd"))
p6 <- p6+
  xlab(paste0("PC1")) +
  ylab(paste0("PC2")) +
  ggtitle("Sawmill Road, Xantiana")
ggplotly(p6)
#Site 22, Xantiana
p7 <- ggplot(pca, aes(PC1, PC2, label=ind))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp_loc=="xantiana_Site 22"))
p7 <- p7+
  xlab(paste0("PC1")) +
  ylab(paste0("PC2")) +
  ggtitle("Site 22, Xantiana")
ggplotly(p7)
#Squirrel Mountain,Xantiana
p8 <- ggplot(pca, aes(PC1, PC2, label=ind))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp_loc=="xantiana_Squirrel Mountain"))
p8 <- p8+
  xlab(paste0("PC1")) +
  ylab(paste0("PC2")) +
  ggtitle("Squirrel Mountain, Xantiana")
ggplotly(p8)
#Sawmill Rd, Parviflora
p9 <- ggplot(pca, aes(PC1, PC2, label=ind))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp_loc=="parviflora_Sawmill Rd"))
p9 <- p9+
  xlab(paste0("PC1")) +
  ylab(paste0("PC2")) +
  ggtitle("Sawmill Road, Parviflora")
ggplotly(p9)
#Site 22, Parviflora
p10 <- ggplot(pca, aes(PC1, PC2, label=ind))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp_loc=="parviflora_Site 22"))
p10 <- p10+
  xlab(paste0("PC1")) +
  ylab(paste0("PC2")) +
  ggtitle("Site 22, Parviflora")
ggplotly(p10)
#Squirrel Mountain, Parviflora
p11 <- ggplot(pca, aes(PC1, PC2, label=ind))+
  geom_point(size=2, alpha=.6, data=subset(pca, spp_loc=="parviflora_Squirrel Mountain"))
p11 <- p11+
  xlab(paste0("PC1")) +
  ylab(paste0("PC2")) +
  ggtitle ("Squirrel Mountain, Parviflora")
ggplotly(p11)
ggplotly
#Arrange plots
#xantiana
figure3 <- ggarrange(p6, p7, p8)
figure3 <- annotate_figure(figure3, top=text_grob("Xantiana"))
figure3
#parviflora
figure4 <- ggarrange(p9, p10, p11)
figure4 <- annotate_figure(figure4, top=text_grob("Parviflora"))
figure4
