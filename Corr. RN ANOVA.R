
carnern<-read.csv2("carnern2.csv")
library(FactoMineR)
summary(carnern)
head(carnern)
names(carnern) 
pcacarnern<-PCA(carnern[,1:13], scale.unit = TRUE, ind.sup = NULL, quanti.sup = NULL, quali.sup = 13,row.w = NULL,invisible(13),col.w = NULL,graph = TRUE,axes = c(1,2))
plot.PCA(pcacarnern, axes=c(1, 2), choix="ind", xlim=c(-4,8), ylim=c(-4,4),invisible="ind") 
plot.PCA(pcacarnern, axes=c(1, 2), choix="ind", habillage=13, cex=0.7)
pcacarner<-PCA(carner[,1:13], scale.unit = TRUE, ind.sup = NULL, quanti.sup = NULL,
               quali.sup = 13,row.w = NULL,col.w = FALSE,graph = TRUE,axes = c(1,2))
concat = cbind.data.frame(carnern[,13], pcacarnern$ind$coord)
ellipse.coord = coord.ellipse(concat, bary = TRUE, level.conf = 0.99)
plot.PCA(pcacarnern, habillage = 13, ellipse = ellipse.coord, cex = 0.7,lwd=3)
concat = cbind.data.frame(carnern[,13], pcacarnern$ind$coord)
ellipse.coord = coord.ellipse(concat, bary = TRUE, level.conf = 0.95)
plot.PCA(pcacarnern, habillage = 13, ellipse = ellipse.coord, cex = 0.3,lwd=3)

#TEST DE CORRELACIÓN ENTRE LOS PARÁMETROS DE CALIDAD DE CARNE

cor.test(carnern$pH45min,carnern$pH24h) 
cor.test(carnern$pH45min,carnern$H)
cor.test(carnern$pH45min,carnern$L.) 
cor.test(carnern$pH45min,carnern$a.)
cor.test(carnern$pH45min,carnern$b.) 
cor.test(carnern$pH45min,carnern$MG24h)
cor.test(carnern$pH45min,carnern$MG48h) 
cor.test(carnern$pH45min,carnern$MC)
cor.test(carnern$pH45min,carnern$MD) 
cor.test(carnern$pH45min,carnern$T)
cor.test(carnern$pH45min,carnern$M)
cor.test(carnern$pH24h,carnern$H)
cor.test(carnern$pH24h,carnern$L.) 
cor.test(carnern$pH24h,carnern$a)
cor.test(carnern$pH24h,carnern$b.) 
cor.test(carnern$pH24h,carnern$MG24h)
cor.test(carnern$pH24h,carnern$MG48h) 
cor.test(carnern$pH24h,carnern$MC)
cor.test(carnern$pH24h,carnern$MD) 
cor.test(carnern$pH24h,carnern$T)
cor.test(carnern$pH24h,carnern$M)
cor.test(carnern$L.,carnern$a.) 
cor.test(carnern$L.,carnern$b)
cor.test(carnern$L.,carnern$MG24h) 
cor.test(carnern$L.,carnern$MG48h)
cor.test(carnern$L.,carnern$MC)
cor.test(carnern$L.,carnern$MD) 
cor.test(carnern$L.,carnern$T)
cor.test(carnern$L.,carnern$M)
cor.test(carnern$a.,carnern$b)
cor.test(carnern$a.,carnern$MG24h) 
cor.test(carnern$a.,carnern$MG48h)
cor.test(carnern$a.,carnern$MC)
cor.test(carnern$a.,carnern$MD) 
cor.test(carnern$a.,carnern$T)
cor.test(carnern$a.,carnern$M)
cor.test(carnern$b.,carnern$MG24h) 
cor.test(carnern$b.,carnern$MG48h)
cor.test(carnern$b.,carnern$MC)
cor.test(carnern$b.,carnern$MD) 
cor.test(carnern$b.,carnern$T)
cor.test(carnern$b.,carnern$M)
cor.test(carnern$MG24h,carnern$MG48h)
cor.test(carnern$MG24h,carnern$MC)
cor.test(carnern$MG24h,carnern$MD) 
cor.test(carnern$MG24h,carnern$T)
cor.test(carnern$MG24h,carnern$M)
cor.test(carnern$MG48h,carnern$MC)
cor.test(carnern$MG48h,carnern$MD) 
cor.test(carnern$MG48h,carnern$T)
cor.test(carnern$MG48h,carnern$M)
cor.test(carnern$MC,carnern$MD) 
cor.test(carnern$MC,carnern$T)
cor.test(carnern$MC,carnern$M)
cor.test(carnern$MD,carnern$T)
cor.test(carnern$MD,carnern$M)


tapply(carnern$pH45min,factor(carnern$RN),mean,na.rm=TRUE)
hist(carnern$pH45min)


boxplot(pH45min~RN,data=carnern)

qqnorm(carnern$pH45min, pch=20,col="red",cex=1.5, main = "pH 45min")

barplot(tapply(carnern$pH45min,carnern$RN,mean),ylim=c(0,7),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="pH 45min",las=1)
boxplot(pH45min~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue"),range=0)

shapiro.test(carnern$pH45min)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(carnern$pH45min[carnern$RN=="rn+/rn+"]) 
shapiro.test(carnern$pH45min[carnern$RN=="rn+/rn*"]) 
shapiro.test(carnern$pH45min[carnern$RN=="rn*/rn*"]) 
shapiro.test(carnern$pH45min[carnern$RN=="RN-/rn+"]) 
shapiro.test(carnern$pH45min[carnern$RN=="RN-/rn*"]) 
#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(carnern$pH45min[carnern$RN== "rn+/rn+"], pch=19,col="purple")
plot(carnern$pH45min[carnern$RN=="rn+/rn*"],pch=19,col="blue")
plot(carnern$pH45min[carnern$RN=="rn*/rn*"],pch=19,col="green")
plot(carnern$pH45min[carnern$RN=="RN-/rn+"],pch=19,col="red")
plot(carnern$pH45min[carnern$RN=="RN-/rn*"],pch=19,col="black")

Box.test(carnern$pH45min[carnern$RN=="rn+/rn+"],lag=1,type=c("Ljung-Box"))
Box.test(carnern$pH45min[carnern$RN==" rn+/rn*"],lag=1,type=c("Ljung-Box"))
Box.test(carnern$pH45min[carnern$RN==" rn*/rn*"],lag=1,type=c("Ljung-Box"))
Box.test(carnern$pH45min[carnern$RN== "RN-/rn+"],lag=1,type=c("Ljung-Box"))
Box.test(carnern$pH45min[carnern$RN== "RN-/rn*"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovcarnern<-aov(pH45min~RN,data=carnern)   
summary(aovcarnern)

#ANÁLISIS DE PH24h Y RN

#Normalidad


mean(carnern$pH24h,na.rm=TRUE) 

tapply(carnern$pH24h,factor(carnern$RN),mean,na.rm=TRUE) 

plot(pH24h~RN,data=carnern)

boxplot(pH24h~RN,data=carnern)

barplot(tapply(carnern$pH24h,carnern$RN,mean),ylim=c(0,7),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="pH 24h",las=1)
hist(carnern$pH24h)
qqnorm(carnern$pH24h,pch=20,col="red",cex=1.5, main = "pH 24h")        

boxplot(pH24h~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue","darkblue","darkblue","darkblue"),range=0)

shapiro.test(carnern$pH24h)
#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01
shapiro.test(carnern$pH24h[carnern$RN=="rn+/rn+"]) 
shapiro.test(carnern$pH24h[carnern$RN=="rn+/rn*"]) 
shapiro.test(carnern$pH24h[carnern$RN=="rn*/rn*"]) 
shapiro.test(carnern$pH24h[carnern$RN=="RN-/rn+"]) 
shapiro.test(carnern$pH24h[carnern$RN=="RN-/rn*"]) 

#Independencia
plot(carnern$pH24h[carnern$RN=="rn+/rn+"], pch=19,col="purple")
plot(carnern$pH24h[carnern$RN=="rn+/rn*"],pch=19,col="blue")
plot(carnern$pH24h[carnern$RN=="rn*/rn*"],pch=19,col="green")
plot(carnern$pH24h[carnern$RN=="RN-/rn+"],pch=19,col="red")
plot(carnern$pH24h[carnern$RN=="RN-/rn*"],pch=19,col="black")


Box.test(carnern$pH24h[carnern$RN=="rn+/rn+"],lag=1,type=c("Ljung-Box"))
Box.test(carnern$pH24h[carnern$RN=="rn+/rn*"],lag=1,type=c("Ljung-Box"))
Box.test(carnern$pH24h[carnern$RN=="rn*/rn*"],lag=1,type=c("Ljung-Box"))
Box.test(carnern$pH24h[carnern$RN=="RN-/rn+"],lag=1,type=c("Ljung-Box"))
Box.test(carnern$pH24h[carnern$RN=="RN-/rn*"],lag=1,type=c("Ljung-Box"))

> plot(carnern$pH24h[carnern$RN=="rn+/rn+"], pch=19,col="purple")
> plot(carnern$pH24h[carnern$RN=="rn+/rn*"],pch=19,col="blue")
> plot(carnern$pH24h[carnern$RN=="rn*/rn*"],pch=19,col="green")
> plot(carnern$pH24h[carnern$RN=="RN-/rn+"],pch=19,col="red")
> plot(carnern$pH24h[carnern$RN=="RN-/rn*"],pch=19,col="black")

#ANOVA
aovcarnern<-aov(pH24h~RN,data=carnern)   
summary(aovcarnern)

L* y RN
mean(carnern$L.,na.rm=TRUE) 
tapply(carnern$L.,factor(carnern$RN),mean,na.rm=TRUE) 
plot(L.~RN,data=carnern)
boxplot(L.~RN,data=carnern)
barplot(tapply(carnern$L.,factor(carnern$RN,mean))
        hist(carnern$L.,ylim=c(0,20),col=c("black","red","green","blue","purple"),border=("darkblue"))
        qqnorm(carnern$L.,pch=20,col="red",cex=1.5, main = "L*")
        
        barplot(tapply(carnern$L.,carnern$RN,mean),ylim=c(0,70),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="L*",las=1)
        boxplot(L.~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue","darkblue","darkblue","darkblue"),range=0)
        
        #Normalidad
        shapiro.test(carnern$L.)
        #Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01
        shapiro.test(carnern$L.[carnern$RN=="rn+/rn+"]) 
        shapiro.test(carnern$L.[carnern$RN=="rn+/rn*"]) 
        shapiro.test(carnern$L.[carnern$RN=="rn*/rn*"]) 
        shapiro.test(carnern$L.[carnern$RN=="RN-/rn+"]) 
        shapiro.test(carnern$L.[carnern$RN=="RN-/rn*"]) 
        #Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
        
        
        
        #Independencia
        plot(carnern$L.[carnern$RN=="rn+/rn+"], pch=19,col="purple")
        plot(carnern$L.[carnern$RN=="rn+/rn*"],pch=19,col="blue")
        plot(carnern$L.[carnern$RN=="rn*/rn*"],pch=19,col="green")
        plot(carnern$L.[carnern$RN=="RN-/rn+"],pch=19,col="red")
        plot(carnern$L.[carnern$RN=="RN-/rn*"],pch=19,col="black")
        
        
        Box.test(carnern$L.[carnern$RN=="rn+/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$L.[carnern$RN=="rn+/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$L.[carnern$RN=="rn*/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$L.[carnern$RN=="RN-/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$L.[carnern$RN=="RN-/rn*"],lag=1,type=c("Ljung-Box"))
        
        
        #Homogeneidad
        
        plot(L.~RN,data=carnern)
        tapply(carnern$L.,carnern$RN,var) 
        bartlett.test(list(carnern$L.[carnern$RN=="rn+/rn+",carnern$RN=="rn+/rn*",carnern$RN=="rn*/rn*",carnern$RN=="RN-/rn+",carnern$RN=="RN-/rn*"]))
        
        
        #ANOVA
        aovcarnern<-aov(L.~RN,data=carnern)   
        summary(aovcarnern)
        
        
        
        
        
        
        
        mean(carnern$a.,na.rm=TRUE) 
        
        RN Y a.
        tapply(carnern$a.,factor(carnern$RN),mean,na.rm=TRUE) 
        
        plot(a.~RN,data=carnern)
        
        boxplot(a.~RN,data=carnern)
        
        barplot(tapply(carnern$a.,factor(carnern$RN,mean)))
        hist(carnern$a.,ylim=c(0,20),col=c("black","red","green","blue","purple"),border=("darkblue"))
        qqnorm(carnern$L.,pch=20,col="red",cex=1.5, main = "L*")
        
        barplot(tapply(carnern$a.,carnern$RN,mean),ylim=c(0,30),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="a*",las=1)
        boxplot(a.~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue","darkblue","darkblue","darkblue"),range=0)
        shapiro.test(carnern$a.)
        #Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01
        shapiro.test(carnern$a.[carnern$RN=="rn+/rn+"]) 
        shapiro.test(carnern$a.[carnern$RN=="rn+/rn*"]) 
        shapiro.test(carnern$a.[carnern$RN=="rn*/rn*"]) 
        shapiro.test(carnern$a.[carnern$RN=="RN-/rn+"]) 
        shapiro.test(carnern$a.[carnern$RN=="RN-/rn*"]) 
        #Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
        
        
        
        #Independencia
        
        plot(carnern$a.[carnern$RN=="rn+/rn+"],pch=19,col="purple")
        plot(carnern$a.[carnern$RN=="rn+/rn*"],pch=19,col="blue")
        plot(carnern$a.[carnern$RN=="rn*/rn*"],pch=19,col="green")
        plot(carnern$a.[carnern$RN=="RN-/rn+"],pch=19,col="red")
        plot(carnern$a.[carnern$RN=="RN-/rn*"],pch=19,col="black")
        
        
        Box.test(carnern$a.[carnern$RN=="rn+/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$a.[carnern$RN=="rn+/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$a.[carnern$RN=="rn*/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$a.[carnern$RN=="RN-/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$a.[carnern$RN=="RN-/rn*"],lag=1,type=c("Ljung-Box"))
        
        
        
        
        mean(carnern$b.,na.rm=TRUE) 
        
        tapply(carnern$b.,factor(carnern$RN),mean,na.rm=TRUE) 
        
        plot(b.~RN,data=carnern)
        
        boxplot(b.~RN,data=carnern)
        
        barplot(tapply(carnern$b.,factor(carnern$RN,mean)))
        hist(carnern$b.,ylim=c(0,20),col=c("black","red","green","blue","purple"),border=("darkblue"))
        qqnorm(carnern$b.,pch=20,col="red",cex=1.5, main = "b*")
        
        barplot(tapply(carnern$b.,carnern$RN,mean),ylim=c(0,30),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="b*",las=1)
        boxplot(b.~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue","darkblue","darkblue","darkblue"),range=0)
        
        
        shapiro.test(carnern$b.)
        #Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01
        shapiro.test(carnern$b.[carnern$RN=="rn+/rn+"]) 
        shapiro.test(carnern$b.[carnern$RN=="rn+/rn*"]) 
        shapiro.test(carnern$b.[carnern$RN=="rn*/rn*"]) 
        shapiro.test(carnern$b.[carnern$RN=="RN-/rn+"]) 
        shapiro.test(carnern$b.[carnern$RN=="RN-/rn*"]) 
        #Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
        
        
        
        
        #Independencia
        plot(carnern$b.[carnern$RN=="rn+/rn+"], pch=19,col="purple")
        plot(carnern$b.[carnern$RN=="rn+/rn*"],pch=19,col="blue")
        plot(carnern$b.[carnern$RN=="rn*/rn*"],pch=19,col="green")
        plot(carnern$b.[carnern$RN=="RN-/rn+"],pch=19,col="red")
        plot(carnern$b.[carnern$RN=="RN-/rn*"],pch=19,col="black")
        
        
        Box.test(carnern$b.[carnern$RN=="rn+/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$b.[carnern$RN=="rn+/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$b.[carnern$RN=="rn*/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$b.[carnern$RN=="RN-/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$b.[carnern$RN=="RN-/rn*"],lag=1,type=c("Ljung-Box"))
        
        
        RN y MG24h
        mean(carnern$MG24h,na.rm=TRUE) 
        tapply(carnern$MG24h,factor(carnern$RN),mean,na.rm=TRUE) 
        plot(MG24h~RN,data=carnern)
        boxplot(MG24h~RN,data=carnern)
        barplot(tapply(carnern$MG24h,carnern$RN,mean),ylim=c(0,7),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="MG 24h",las=1)
        hist(carnern$MG24h)
        qqnorm(carnern$MG24h,pch=20,col="red",cex=1.5, main = "MG 24h")        
        boxplot(MG24h~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue","darkblue","darkblue","darkblue"),range=0)
        
        RN Y MG 48h
        mean(carnern$MG48h,na.rm=TRUE) 
        
        tapply(carnern$MG48h,factor(carnern$RN),mean,na.rm=TRUE) 
        
        plot(MG48h~RN,data=carnern)
        boxplot(MG48h~RN,data=carnern)
        barplot(tapply(carnern$MG48h,carnern$RN,mean),ylim=c(0,7),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="MG 48h",las=1)
        hist(carnern$MG48h)
        qqnorm(carnern$MG48h,pch=20,col="red",cex=1.5, main = "MG 48h")        
        
        boxplot(MG48h~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue","darkblue","darkblue","darkblue"),range=0)
        
        
        RN y MD
        carnern<-read.csv2("carnern.csv")
        library(FactoMineR)
        summary(carnern)
        head(carnern)
        names(carnern) 
        mean(carnern$MD,na.rm=TRUE) 
        tapply(carnern$MD,factor(carnern$RN),mean,na.rm=TRUE) 
        plot(MD~RN,data=carnern)
        boxplot(MD~RN,data=carnern)
        barplot(tapply(carnern$MD,factor(carnern$RN,mean)))
        hist(carnern$MD,ylim=c(0,25),col=c("black","red","green","blue","purple"),border=("darkblue"))
        qqnorm(carnern$MD,pch=20,col="red",cex=1.5, main = "MD") 
        barplot(tapply(carnern$MD,carnern$RN,mean),ylim=c(0,10),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="MD",las=1)
        boxplot(MD~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue","darkblue","darkblue","darkblue"),range=0)
        
        
        RN y MERMAS POR COCCIÓN MC
        carnern<-read.csv2("carnern.csv")
        library(FactoMineR)
        summary(carnern)
        head(carnern)
        names(carnern)
        mean(carnern$MC,na.rm=TRUE) 
        tapply(carnern$MC,factor(carnern$RN),mean,na.rm=TRUE) 
        tapply(carnern$MC,factor(carnern$RN),mean,na.rm=TRUE) 
        plot(MC~RN,data=carnern)
        boxplot(MC~RN,data=carnern)
        hist(carnern$MC,ylim=c(0,15),col=c("black","red","green","blue","purple"),border=("darkblue"))
        qqnorm(carnern$MC,pch=20,col="red",cex=1.5, main = "MC") 
        barplot(tapply(carnern$MC,carnern$RN,mean),ylim=c(0,30),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="MC",las=1)
        boxplot(MC~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue","darkblue","darkblue","darkblue"),range=0)
        shapiro.test(carnern$MC)
        #Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01
        shapiro.test(carnern$MC[carnern$RN=="rn+/rn+"]) 
        shapiro.test(carnern$MC[carnern$RN=="rn+/rn*"]) 
        shapiro.test(carnern$MC[carnern$RN=="rn*/rn*"]) 
        shapiro.test(carnern$MC[carnern$RN=="RN-/rn+"]) 
        shapiro.test(carnern$MC[carnern$RN=="RN-/rn*"]) 
        #Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
        
        #Independencia
        plot(carnern$MC[carnern$RN=="rn+/rn+"], pch=19,col="purple")
        plot(carnern$MC[carnern$RN=="rn+/rn*"],pch=19,col="blue")
        plot(carnern$MC[carnern$RN=="rn*/rn*"],pch=19,col="green")
        plot(carnern$MC[carnern$RN=="RN-/rn+"],pch=19,col="red")
        plot(carnern$MC[carnern$RN=="RN-/rn*"],pch=19,col="black")
        
        
        Box.test(carnern$MC[carnern$RN=="rn+/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$MC[carnern$RN=="rn+/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$MC[carnern$RN=="rn*/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$MC[carnern$RN=="RN-/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$MC[carnern$RN=="RN-/rn*"],lag=1,type=c("Ljung-Box"))
        
        
        #ANOVA
        aovcarnern<-aov(MC~RN,data=carnern)   
        summary(aovcarnern)
        
        RN Y Terneza T
        
        carnern<-read.csv2("carnern.csv")
        library(FactoMineR)
        summary(carnern)
        head(carnern)
        names(carnern)
        mean(carnern$T,na.rm=TRUE) 
        tapply(carnern$T,factor(carnern$RN),mean,na.rm=TRUE) 
        
        
        tapply(carnern$T,factor(carnern$RN),mean,na.rm=TRUE) 
        
        plot(T~RN,data=carnern)
        
        boxplot(T~RN,data=carnern)
        
        hist(carnern$T,ylim=c(0,15),col=c("black","red","green","blue","purple"),border=("darkblue"))
        qqnorm(carnern$MC,pch=20,col="red",cex=1.5, main = "T") 
        
        
        barplot(tapply(carnern$T,carnern$RN,mean),ylim=c(0,10),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="T",las=1)
        boxplot(T~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue","darkblue","darkblue","darkblue"),range=0)
        
        
        shapiro.test(carnern$T)
        
        shapiro.test(carnern$T[carnern$RN=="rn+/rn+"]) 
        
        shapiro.test(carnern$T[carnern$RN=="rn+/rn*"]) 
        
        
        shapiro.test(carnern$T[carnern$RN=="rn*/rn*"]) 
        
        shapiro.test(carnern$T[carnern$RN=="RN-/rn+"]) 
        
        shapiro.test(carnern$T[carnern$RN=="RN-/rn*"]) 
        
        >                 shapiro.test(carnern$T[carnern$RN=="RN-/rn*"]) 
        
        
        #Independencia
        plot(carnern$T[carnern$RN=="rn+/rn+"],pch=19,col="purple")
        plot(carnern$T[carnern$RN=="rn+/rn*"],pch=19,col="blue")
        plot(carnern$T[carnern$RN=="rn*/rn*"],pch=19,col="green")
        plot(carnern$T[carnern$RN=="RN-/rn+"],pch=19,col="red")
        plot(carnern$T[carnern$RN=="RN-/rn*"],pch=19,col="black")
        
        
        Box.test(carnern$T[carnern$RN=="rn+/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$T[carnern$RN=="rn+/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$T[carnern$RN=="rn*/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$T[carnern$RN=="RN-/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$T[carnern$RN=="RN-/rn*"],lag=1,type=c("Ljung-Box"))               
        #ANOVA
        aovcarnern<-aov(T~RN,data=carnern)   
        summary(aovcarnern)
        
        RN Y HUMEDAD H
        carnern<-read.csv2("carnern.csv")
        library(FactoMineR)
        summary(carnern)
        head(carnern)
        names(carnern)
        mean(carnern$H,na.rm=TRUE) 
        tapply(carnern$H,factor(carnern$RN),mean,na.rm=TRUE) 
        
        
        tapply(carnern$H,factor(carnern$RN),mean,na.rm=TRUE) 
        
        plot(H~RN,data=carnern)
        
        boxplot(H~RN,data=carnern)
        
        hist(carnern$H,ylim=c(0,25),col=c("black","red","green","blue","purple"),border=("darkblue"))
        qqnorm(carnern$H,pch=20,col="red",cex=1.5, main = "H") 
        
        
        barplot(tapply(carnern$H,carnern$RN,mean),ylim=c(0,80),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="H",las=1)
        boxplot(H~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue","darkblue","darkblue","darkblue"),range=0)
        
        
        shapiro.test(carnern$H)
        
        shapiro.test(carnern$H[carnern$RN=="rn+/rn+"]) 
        
        shapiro.test(carnern$H[carnern$RN=="rn+/rn*"]) 
        
        
        shapiro.test(carnern$H[carnern$RN=="rn*/rn*"]) 
        
        shapiro.test(carnern$H[carnern$RN=="RN-/rn+"]) 
        
        shapiro.test(carnern$H[carnern$RN=="RN-/rn*"]) 
        
        shapiro.test(carnern$H[carnern$RN=="RN-/rn*"]) 
        
        
        #Independencia
        plot(carnern$H[carnern$RN=="rn+/rn+"],pch=19,col="purple")
        plot(carnern$H[carnern$RN=="rn+/rn*"],pch=19,col="blue")
        plot(carnern$H[carnern$RN=="rn*/rn*"],pch=19,col="green")
        plot(carnern$H[carnern$RN=="RN-/rn+"],pch=19,col="red")
        plot(carnern$H[carnern$RN=="RN-/rn*"],pch=19,col="black")
        
        
        Box.test(carnern$H[carnern$RN=="rn+/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$H[carnern$RN=="rn+/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$H[carnern$RN=="rn*/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$H[carnern$RN=="RN-/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$H[carnern$RN=="RN-/rn*"],lag=1,type=c("Ljung-Box"))               
        #ANOVA
        aovcarnern<-aov(H~RN,data=carnern)   
        summary(aovcarnern)
        
        
        
        
        
        
        RN Y MARMOLADO M
        
        mean(carnern$M,na.rm=TRUE) 
        tapply(carnern$M,factor(carnern$RN),mean,na.rm=TRUE) 
        plot(M~RN,data=carnern)
        boxplot(M~RN,data=carnern)
        hist(carnern$M,ylim=c(0,20),col=c("black","red","green","blue","purple"),border=("darkblue"))
        qqnorm(carnern$M,pch=20,col="red",cex=1.5, main = "M") 
        barplot(tapply(carnern$M,carnern$RN,mean),ylim=c(0,10),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="M",las=1)
        boxplot(M~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue","darkblue","darkblue","darkblue"),range=0)
        
        
        shapiro.test(carnern$M)
        
        shapiro.test(carnern$M[carnern$RN=="rn+/rn+"]) 
        
        shapiro.test(carnern$M[carnern$RN=="rn+/rn*"]) 
        
        
        shapiro.test(carnern$M[carnern$RN=="rn*/rn*"]) 
        
        shapiro.test(carnern$M[carnern$RN=="RN-/rn+"]) 
        
        shapiro.test(carnern$M[carnern$RN=="RN-/rn*"]) 
        
        shapiro.test(carnern$M[carnern$RN=="RN-/rn*"]) 
        
        
        #Independencia
        plot(carnern$M[carnern$RN=="rn+/rn+"],pch=19,col="purple")
        plot(carnern$M[carnern$RN=="rn+/rn*"],pch=19,col="blue")
        plot(carnern$M[carnern$RN=="rn*/rn*"],pch=19,col="green")
        plot(carnern$M[carnern$RN=="RN-/rn+"],pch=19,col="red")
        plot(carnern$M[carnern$RN=="RN-/rn*"],pch=19,col="black")
        
        
        Box.test(carnern$M[carnern$RN=="rn+/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$M[carnern$RN=="rn+/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$M[carnern$RN=="rn*/rn*"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$M[carnern$RN=="RN-/rn+"],lag=1,type=c("Ljung-Box"))
        Box.test(carnern$M[carnern$RN=="RN-/rn*"],lag=1,type=c("Ljung-Box"))               
        #ANOVA
        aovcarnern<-aov(M~RN,data=carnern)   
        summary(aovcarnern)
        