carne<-read.csv2("carne1.csv") 
library(FactoMineR)
summary(carne)
head(carne)
names(carne) 
pcacarne<-PCA(carne[,1:12], scale.unit = TRUE, ind.sup = NULL, quanti.sup = NULL,
              quali.sup = NULL,row.w = NULL,col.w = NULL,graph = TRUE,axes = c(1,2))

pcacarne<-PCA(carne[,1:13], scale.unit = TRUE, ind.sup = NULL, quanti.sup = NULL,
              quali.sup = 13,row.w = NULL,col.w = NULL,graph = TRUE,axes = c(1,2))
plot.PCA(pcacarne, axes=c(1, 2), choix="ind", xlim=c(-4,8), ylim=c(-4,4),invisible="ind") 
plot.PCA(pcacarne, axes=c(1, 2), choix="ind", habillage=13, cex=0.7) 

concat = cbind.data.frame(carne[,13], pcacarne$ind$coord)
ellipse.coord = coord.ellipse(concat, bary = TRUE, level.conf = 0.99)
plot.PCA(pcacarne, habillage = 13, ellipse = ellipse.coord, cex = 0.7,lwd=3)
concat = cbind.data.frame(carne[,13], pcacarne$ind$coord)
ellipse.coord = coord.ellipse(concat, bary = TRUE, level.conf = 0.95)
plot.PCA(pcacarne, habillage = 13, ellipse = ellipse.coord, cex = 0.7,lwd=3)
