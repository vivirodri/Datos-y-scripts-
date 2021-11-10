SOX6A <-read.csv2("SOX6A.csv")
library(FGAtoMineR)
summary(SOX6A)
head(SOX6A)
names(SOX6A) 

aovSOX6A<-aov(pH45min~SOX6GA,data=SOX6A)   
summary(aovSOX6A)

tapply(SOX6A$pH24h,fGAtor(SOX6A$SOX6GA),mean,na.rm=TRUE)
hist(SOX6A$pH24h)


boxplot(pH24h~SOX6GA,data=SOX6A)

qqnorm(SOX6A $pH24h, pch=20,col="red",cex=1.5, main = "pH24h")

barplot(tapply(SOX6A$pH24h,SOX6A$SOX6GA,mean),ylim=c(0,7),col=c("black","red"), spGAe=1,border="darkblue",main="pH24h",las=1)
boxplot(pH24h~ SOX6GA,data= SOX6A,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(SOX6A$pH24h)

#GAeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6A$pH24h[SOX6A$SOX6GA=="AA"]) 
shapiro.test(SOX6A$pH24h[SOX6A$SOX6GA=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6A$pH24h[SOX6A$SOX6GA=="AA"],pch=19,col="red")
plot(SOX6A$pH24h[SOX6A$SOX6GA=="GA"],pch=19,col="black")

Box.test(SOX6A$pH24h[SOX6A$SOX6GA=="AA"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6A$pH24h[SOX6A$SOX6GA=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6A<-aov(pH24h~SOX6GA,data=SOX6A)   
summary(aovSOX6A)



tapply(SOX6A$L.,fGAtor(SOX6A$SOX6GA),mean,na.rm=TRUE)
hist(SOX6A$L.)
boxplot(L.~SOX6GA,data=SOX6A)
qqnorm(SOX6A $L., pch=20,col="red",cex=1.5, main = "pH24h")
barplot(tapply(SOX6A$L.,SOX6A$SOX6GA,mean),ylim=c(0,60),col=c("black","red"), spGAe=1,border="darkblue",main="L*",las=1)
boxplot(L.~SOX6GA,data=SOX6A,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)

shapiro.test(SOX6A$L.)

#GAeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6A$L.[SOX6A$SOX6GA=="AA"]) 
shapiro.test(SOX6A$L.[SOX6A$SOX6GA=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6A$L.[SOX6A$SOX6GA=="AA"],pch=19,col="red")
plot(SOX6A$L.[SOX6A$SOX6GA=="GA"],pch=19,col="black")

Box.test(SOX6A$L.[SOX6A$SOX6GA=="AA"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6A$L.[SOX6A$SOX6GA=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6A<-aov(L.~SOX6GA,data=SOX6A)   
summary(aovSOX6A)

tapply(SOX6A$a.,fGAtor(SOX6A$SOX6GA),mean,na.rm=TRUE)
hist(SOX6A$a.)


boxplot(a.~SOX6GA,data=SOX6A)

qqnorm(SOX6A $a., pch=20,col="red",cex=1.5, main = "a*")

barplot(tapply(SOX6A$a.,SOX6A$SOX6GA,mean),ylim=c(0,20),col=c("black","red"), spGAe=1,border="darkblue",main="a*",las=1)
boxplot(a.~ SOX6GA,data= SOX6A,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(SOX6A$a.)

#GAeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6A$a.[SOX6A$SOX6GA=="AA"]) 
shapiro.test(SOX6A$a.[SOX6A$SOX6GA=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6A$a.[SOX6A$SOX6GA=="AA"],pch=19,col="red")
plot(SOX6A$a.[SOX6A$SOX6GA=="GA"],pch=19,col="black")

Box.test(SOX6A$a.[SOX6A$SOX6GA=="AA"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6A$a.[SOX6A$SOX6GA=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6A<-aov(a.~SOX6GA,data=SOX6A)   
summary(aovSOX6A)


tapply(SOX6A$b.,fGAtor(SOX6A$SOX6GA),mean,na.rm=TRUE)
hist(SOX6A$b.)


boxplot(b.~SOX6GA,data=SOX6A)

qqnorm(SOX6A $b., pch=20,col="red",cex=1.5, main = "b*")

barplot(tapply(SOX6A$b.,SOX6A$SOX6GA,mean),ylim=c(0,20),col=c("black","red"), spGAe=1,border="darkblue",main="b*",las=1)
boxplot(b.~ SOX6GA,data= SOX6A,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(SOX6A$b.)

#GAeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6A$b.[SOX6A$SOX6GA=="AA"]) 
shapiro.test(SOX6A$b.[SOX6A$SOX6GA=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6A$b.[SOX6A$SOX6GA=="AA"],pch=19,col="red")
plot(SOX6A$b.[SOX6A$SOX6GA=="GA"],pch=19,col="black")

Box.test(SOX6A$b.[SOX6A$SOX6GA=="AA"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6A$b.[SOX6A$SOX6GA=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6A<-aov(b.~SOX6GA,data=SOX6A)   
summary(aovSOX6A)

tapply(SOX6A$MG24h,fGAtor(SOX6A$SOX6GA),mean,na.rm=TRUE)
hist(SOX6A$MG24h)


boxplot(MG24h~SOX6GA,data=SOX6A)

qqnorm(SOX6A $MG24h, pch=20,col="red",cex=1.5, main = "MG24h")

barplot(tapply(SOX6A$MG24h,SOX6A$SOX6GA,mean),ylim=c(0,10),col=c("black","red"), spGAe=1,border="darkblue",main="MG24h",las=1)
boxplot(MG24h~ SOX6GA,data= SOX6A,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(SOX6A$MG24h)

#GAeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6A$MG24h[SOX6A$SOX6GA=="AA"]) 
shapiro.test(SOX6A$MG24h[SOX6A$SOX6GA=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6A$MG24h[SOX6A$SOX6GA=="AA"],pch=19,col="red")
plot(SOX6A$MG24h[SOX6A$SOX6GA=="GA"],pch=19,col="black")

Box.test(SOX6A$MG24h[SOX6A$SOX6GA=="AA"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6A$MG24h[SOX6A$SOX6GA=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6A<-aov(MG24h~SOX6GA,data=SOX6A)   
summary(aovSOX6A)

tapply(SOX6A$MG48h,fGAtor(SOX6A$SOX6GA),mean,na.rm=TRUE)
hist(SOX6A$MG48h)
boxplot(MG48h~SOX6GA,data=SOX6A)

qqnorm(SOX6A $MG48h, pch=20,col="red",cex=1.5, main = "MG24h")

barplot(tapply(SOX6A$MG48h,SOX6A$SOX6GA,mean),ylim=c(0,10),col=c("black","red"), spGAe=1,border="darkblue",main="MG48h",las=1)
boxplot(MG48h~ SOX6GA,data= SOX6A,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(SOX6A$MG48h)

#GAeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6A$MG48h[SOX6A$SOX6GA=="AA"]) 
shapiro.test(SOX6A$MG48h[SOX6A$SOX6GA=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6A$MG48h[SOX6A$SOX6GA=="AA"],pch=19,col="red")
plot(SOX6A$MG48h[SOX6A$SOX6GA=="GA"],pch=19,col="black")

Box.test(SOX6A$MG48h[SOX6A$SOX6GA=="AA"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6A$MG48h[SOX6A$SOX6GA=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6A<-aov(MG48h~SOX6GA,data=SOX6A)   
summary(aovSOX6A)

tapply(SOX6A$MD,fGAtor(SOX6A$SOX6GA),mean,na.rm=TRUE)
hist(SOX6A$MD)
boxplot(MD~SOX6GA,data=SOX6A)
qqnorm(SOX6A $MD, pch=20,col="red",cex=1.5, main = "MD")
barplot(tapply(SOX6A$MD,SOX6A$SOX6GA,mean),ylim=c(0,10),col=c("black","red"), spGAe=1,border="darkblue",main="MD",las=1)
boxplot(MD~ SOX6GA,data= SOX6A,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)
shapiro.test(SOX6A$MD)
#GAeptamos El supuesto de normalidad si el  p-value es mayor a 0.01
shapiro.test(SOX6A$MD[SOX6A$SOX6GA=="AA"]) 
shapiro.test(SOX6A$MD[SOX6A$SOX6GA=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6A$MD[SOX6A$SOX6GA=="AA"],pch=19,col="red")
plot(SOX6A$MD[SOX6A$SOX6GA=="GA"],pch=19,col="black")

Box.test(SOX6A$MD[SOX6A$SOX6GA=="AA"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6A$MD[SOX6A$SOX6GA=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6A<-aov(MD~SOX6GA,data=SOX6A)   
summary(aovSOX6A)

tapply(SOX6A$MC,fGAtor(SOX6A$SOX6GA),mean,na.rm=TRUE)
hist(SOX6A$MC)
boxplot(MC~SOX6GA,data=SOX6A)
qqnorm(SOX6A $MC, pch=20,col="red",cex=1.5, main = "MC")
barplot(tapply(SOX6A$MC,SOX6A$SOX6GA,mean),ylim=c(0,40),col=c("black","red"), spGAe=1,border="darkblue",main="MC",las=1)
boxplot(MC~ SOX6GA,data= SOX6A,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)
shapiro.test(SOX6A$MC)
#GAeptamos El supuesto de normalidad si el  p-value es mayor a 0.01
shapiro.test(SOX6A$MC[SOX6A$SOX6GA=="AA"]) 
shapiro.test(SOX6A$MC[SOX6A$SOX6GA=="GA"]) 
#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6A$MC[SOX6A$SOX6GA=="AA"],pch=19,col="red")
plot(SOX6A$MC[SOX6A$SOX6GA=="GA"],pch=19,col="black")

Box.test(SOX6A$MC[SOX6A$SOX6GA=="AA"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6A$MC[SOX6A$SOX6GA=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6A<-aov(MC~SOX6GA,data=SOX6A)   
summary(aovSOX6A)

tapply(SOX6A$T,fGAtor(SOX6A$SOX6GA),mean,na.rm=TRUE)
hist(SOX6A$T)


boxplot(T~SOX6GA,data=SOX6A)
qqnorm(SOX6A $T, pch=20,col="red",cex=1.5, main = "T")
barplot(tapply(SOX6A$T,SOX6A$SOX6GA,mean),ylim=c(0,10),col=c("black","red"), spGAe=1,border="darkblue",main="T",las=1)
boxplot(T~ SOX6GA,data= SOX6A,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)
shapiro.test(SOX6A$T)
#GAeptamos El supuesto de normalidad si el  p-value es mayor a 0.01
shapiro.test(SOX6A$T[SOX6A$SOX6GA=="AA"]) 
shapiro.test(SOX6A$T[SOX6A$SOX6GA=="GA"]) 
#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6A$T[SOX6A$SOX6GA=="AA"],pch=19,col="red")
plot(SOX6A$T[SOX6A$SOX6GA=="GA"],pch=19,col="black")

Box.test(SOX6A$T[SOX6A$SOX6GA=="AA"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6A$T[SOX6A$SOX6GA=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6A<-aov(T~SOX6GA,data=SOX6A)   
summary(aovSOX6A)

tapply(SOX6A$M,fGAtor(SOX6A$SOX6GA),mean,na.rm=TRUE)
hist(SOX6A$M)


boxplot(M~SOX6GA,data=SOX6A)

qqnorm(SOX6A $M, pch=20,col="red",cex=1.5, main = "M")

barplot(tapply(SOX6A$M,SOX6A$SOX6GA,mean),ylim=c(0,10),col=c("black","red"), spGAe=1,border="darkblue",main="M",las=1)
boxplot(M~ SOX6GA,data= SOX6A,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(SOX6A$M)

#GAeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6A$M[SOX6A$SOX6GA=="AA"]) 
shapiro.test(SOX6A$M[SOX6A$SOX6GA=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6A$M[SOX6A$SOX6GA=="AA"],pch=19,col="red")
plot(SOX6A$M[SOX6A$SOX6GA=="GA"],pch=19,col="black")

Box.test(SOX6A$M[SOX6A$SOX6GA=="AA"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6A$M[SOX6A$SOX6GA=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6A<-aov(M~SOX6GA,data=SOX6A)   
summary(aovSOX6A)

tapply(SOX6A$H,fGAtor(SOX6A$SOX6GA),mean,na.rm=TRUE)
hist(SOX6A$H)


boxplot(H~SOX6GA,data=SOX6A)

qqnorm(SOX6A $H, pch=20,col="red",cex=1.5, main = "H")

barplot(tapply(SOX6A$H,SOX6A$SOX6GA,mean),ylim=c(0,10),col=c("black","red"), spGAe=1,border="darkblue",main="H",las=1)
boxplot(H~ SOX6GA,data= SOX6A,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(SOX6A$M)

#GAeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6A$H[SOX6A$SOX6GA=="AA"]) 
shapiro.test(SOX6A$H[SOX6A$SOX6GA=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6A$H[SOX6A$SOX6GA=="AA"],pch=19,col="red")
plot(SOX6A$M[SOX6A$SOX6GA=="GA"],pch=19,col="black")

Box.test(SOX6A$H[SOX6A$SOX6GA=="AA"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6A$H[SOX6A$SOX6GA=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6A<-aov(H~SOX6GA,data=SOX6A)   
summary(aovSOX6A)
