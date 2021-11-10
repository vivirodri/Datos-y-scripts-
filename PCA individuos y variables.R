carne<-read.csv2("carnern.csv") 
library(FactoMineR)
summary(carne)
head(carne)
names(carne) 
pcacarne<-PCA(carne[,1:12], scale.unit = TRUE, ind.sup = NULL, quanti.sup = NULL, quali.sup = NULL,row.w = NULL,col.w = NULL,graph = TRUE,axes = c(1,2))

pcacarne<-PCA(carne[,1:17], scale.unit = TRUE, ind.sup = NULL, quanti.sup = NULL,quali.sup = 13:17,row.w = NULL,col.w = NULL, graph = TRUE,axes = c(1,2))
plot.PCA(pcacarne, axes=c(1, 2), choix="ind", xlim=c(-1.5,1.5), ylim=c(-1.5,1.5),invisible="ind",cex=c(0.5,1)) 
plot.PCA(pcacarne, axes=c(1, 2), choix="ind", habillage=13:17, cex=c(0.5,1) 
         