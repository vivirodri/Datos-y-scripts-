SOX6B <-read.csv2("SOX6B.csv")
library(FactorMineR)
summary(SOX6B)
head(SOX6B)
names(SOX6B) 
tapply(SOX6B$pH45min,factor(SOX6B$SOX6CG),mean,na.rm=TRUE)
hist(SOX6B$pH45min)


boxplot(pH45min~SOX6CG,data=SOX6B)

qqnorm(SOX6B $pH45min, pch=20,col="red",cex=1.5, main = "pH 45min")

barplot(tapply(SOX6B$pH45min,SOX6B$SOX6CG,mean),ylim=c(0,7),col=c("black","red", "blue"), space=1,border="darkblue",main="pH 45min",las=1)
boxplot(pH45min~ SOX6CG,data= SOX6B,main="Gráfica boxplot",col=c("black","red", "blue"),border=c("darkblue","darkblue"),range=0)



shapiro.test(SOX6B$pH45min)

#CGeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6B$pH45min[SOX6B$SOX6CG=="CC"]) 
shapiro.test(SOX6B$pH45min[SOX6B$SOX6CG=="CG"]) 
shapiro.test(SOX6B$pH45min[SOX6B$SOX6CG=="GG"]) 


#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6B$pH45min[SOX6B$SOX6CG=="CC"],pch=19,col="red")
plot(SOX6B$pH45min[SOX6B$SOX6CG=="CG"],pch=19,col="black")
plot(SOX6B$pH45min[SOX6B$SOX6CG=="GG"],pch=19,col="blue")

Box.test(SOX6B$pH45min[SOX6B$SOX6CG=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$pH45min[SOX6B$SOX6CG=="CG"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$pH45min[SOX6B$SOX6CG=="GG"],lag=1,type=c("Ljung-Box"))


#ANOVA
aovSOX6B<-aov(pH45min~SOX6CG,data=SOX6B)   
summary(aovSOX6B)

tapply(SOX6B$pH24h, factor(SOX6B$SOX6CG),mean,na.rm=TRUE)
hist(SOX6B$pH24h)


boxplot(pH24h~SOX6CG,data=SOX6B)

qqnorm(SOX6B $pH24h, pch=20,col="red",cex=1.5, main = "pH24h")

barplot(tapply(SOX6B$pH24h,SOX6B$SOX6CG,mean),ylim=c(0,7),col=c("black","red","blue"), space=1,border="darkblue",main="pH24h",las=1)
boxplot(pH24h~ SOX6CG,data= SOX6B,main="Gráfica boxplot",col=c("black","red","blue"),border=c("darkblue","darkblue"),range=0)


shapiro.test(SOX6B$pH24h)

#CGeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6B$pH24h[SOX6B$SOX6CG=="CC"]) 
shapiro.test(SOX6B$pH24h[SOX6B$SOX6CG=="CG"]) 
shapiro.test(SOX6B$Ph24h[SOX6B$SOX6CG=="GG"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6B$pH24h[SOX6B$SOX6CG=="CC"],pch=19,col="red")
plot(SOX6B$pH24h[SOX6B$SOX6CG=="CG"],pch=19,col="black")
plot(SOX6B$pH24h[SOX6B$SOX6CG=="GG"],pch=19,col="blue")


Box.test(SOX6B$pH24h[SOX6B$SOX6CG=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$pH24h[SOX6B$SOX6CG=="CG"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$pH24h[SOX6B$SOX6CG=="GG"],lag=1,type=c("Ljung-Box"))


#ANOVA
aovSOX6B<-aov(pH24h~SOX6CG,data=SOX6B)   
summary(aovSOX6B)



tapply(SOX6B$L.,factor(SOX6B$SOX6CG),mean,na.rm=TRUE)
hist(SOX6B$L.)
boxplot(L.~SOX6CG,data=SOX6B)
qqnorm(SOX6B $L., pch=20,col="red",cex=1.5, main = "pH24h")
barplot(tapply(SOX6B$L.,SOX6B$SOX6CG,mean),ylim=c(0,60),col=c("black","red","blue"), space=1,border="darkblue",main="L*",las=1)
boxplot(L.~SOX6CG,data=SOX6B,main="Gráfica boxplot",col=c("black","red","blue"),border=c("darkblue","darkblue"),range=0)

shapiro.test(SOX6B$L.)

#CGeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6B$L.[SOX6B$SOX6CG=="CC"]) 
shapiro.test(SOX6B$L.[SOX6B$SOX6CG=="CG"]) 
shapiro.test(SOX6B$L.[SOX6B$SOX6CG=="GG"]) 


#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6B$L.[SOX6B$SOX6CG=="CC"],pch=19,col="red")
plot(SOX6B$L.[SOX6B$SOX6CG=="CG"],pch=19,col="black")
plot(SOX6B$L.[SOX6B$SOX6CG=="GG"],pch=19,col="blue")

Box.test(SOX6B$L.[SOX6B$SOX6CG=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$L.[SOX6B$SOX6CG=="CG"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$L.[SOX6B$SOX6CG=="GG"],lag=1,type=c("Ljung-Box"))


#ANOVA
aovSOX6B<-aov(L.~SOX6CG,data=SOX6B)   
summary(aovSOX6B)

tapply(SOX6B$a.,factor(SOX6B$SOX6CG),mean,na.rm=TRUE)
hist(SOX6B$a.)


boxplot(a.~SOX6CG,data=SOX6B)

qqnorm(SOX6B $a., pch=20,col="red",cex=1.5, main = "a*")

barplot(tapply(SOX6B$a.,SOX6B$SOX6CG,mean),ylim=c(0,20),col=c("black","red","blue"), space=1,border="darkblue",main="a*",las=1)
boxplot(a.~ SOX6CG,data= SOX6B,main="Gráfica boxplot",col=c("black","red","blue"),border=c("darkblue","darkblue"),range=0)


shapiro.test(SOX6B$a.)

#CGeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6B$a.[SOX6B$SOX6CG=="CC"]) 
shapiro.test(SOX6B$a.[SOX6B$SOX6CG=="CG"]) 
shapiro.test(SOX6B$a.[SOX6B$SOX6CG=="GG"]) 



#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6B$a.[SOX6B$SOX6CG=="CC"],pch=19,col="red")
plot(SOX6B$a.[SOX6B$SOX6CG=="CG"],pch=19,col="black")
plot(SOX6B$a.[SOX6B$SOX6CG=="GG"],pch=19,col="blue")

Box.test(SOX6B$a.[SOX6B$SOX6CG=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$a.[SOX6B$SOX6CG=="CG"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$a.[SOX6B$SOX6CG=="GG"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6B<-aov(a.~SOX6CG,data=SOX6B)   
summary(aovSOX6B)


tapply(SOX6B$b.,factor(SOX6B$SOX6CG),mean,na.rm=TRUE)
hist(SOX6B$b.)


boxplot(b.~SOX6CG,data=SOX6B)

qqnorm(SOX6B $b., pch=20,col="red",cex=1.5, main = "b*")

barplot(tapply(SOX6B$b.,SOX6B$SOX6CG,mean),ylim=c(0,20),col=c("black","red","blue"), space=1,border="darkblue",main="b*",las=1)
boxplot(b.~ SOX6CG,data= SOX6B,main="Gráfica boxplot",col=c("black","red","blue"),border=c("darkblue","darkblue"),range=0)


shapiro.test(SOX6B$b.)

#CGeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6B$b.[SOX6B$SOX6CG=="CC"]) 
shapiro.test(SOX6B$b.[SOX6B$SOX6CG=="CG"]) 
shapiro.test(SOX6B$b.[SOX6B$SOX6CG=="GG"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6B$b.[SOX6B$SOX6CG=="CC"],pch=19,col="red")
plot(SOX6B$b.[SOX6B$SOX6CG=="CG"],pch=19,col="black")
plot(SOX6B$b.[SOX6B$SOX6CG=="GG"],pch=19,col="blue")


Box.test(SOX6B$b.[SOX6B$SOX6CG=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$b.[SOX6B$SOX6CG=="CG"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$b.[SOX6B$SOX6CG=="GG"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6B<-aov(b.~SOX6CG,data=SOX6B)   
summary(aovSOX6B)

tapply(SOX6B$MG24h,factor(SOX6B$SOX6CG),mean,na.rm=TRUE)
hist(SOX6B$MG24h)


boxplot(MG24h~SOX6CG,data=SOX6B)

qqnorm(SOX6B $MG24h, pch=20,col="red",cex=1.5, main = "MG24h")

barplot(tapply(SOX6B$MG24h,SOX6B$SOX6CG,mean),ylim=c(0,10),col=c("black","red","blue"), space=1,border="darkblue",main="MG24h",las=1)
boxplot(MG24h~ SOX6CG,data= SOX6B,main="Gráfica boxplot",col=c("black","red","blue"),border=c("darkblue","darkblue"),range=0)


shapiro.test(SOX6B$MG24h)

#CGeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6B$MG24h[SOX6B$SOX6CG=="CC"]) 
shapiro.test(SOX6B$MG24h[SOX6B$SOX6CG=="CG"]) 
shapiro.test(SOX6B$MG24h[SOX6B$SOX6CG=="GG"]) 
#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6B$MG24h[SOX6B$SOX6CG=="CC"],pch=19,col="red")
plot(SOX6B$MG24h[SOX6B$SOX6CG=="CG"],pch=19,col="black")
plot(SOX6B$MG24h[SOX6B$SOX6CG=="GG"],pch=19,col="blue")

Box.test(SOX6B$MG24h[SOX6B$SOX6CG=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$MG24h[SOX6B$SOX6CG=="CG"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$MG24h[SOX6B$SOX6CG=="GG"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6B<-aov(MG24h~SOX6CG,data=SOX6B)   
summary(aovSOX6B)

tapply(SOX6B$MG48h,factor(SOX6B$SOX6CG),mean,na.rm=TRUE)
hist(SOX6B$MG48h)
boxplot(MG48h~SOX6CG,data=SOX6B)

qqnorm(SOX6B $MG48h, pch=20,col="red",cex=1.5, main = "MG24h")

barplot(tapply(SOX6B$MG48h,SOX6B$SOX6CG,mean),ylim=c(0,10),col=c("black","red","blue"), space=1,border="darkblue",main="MG48h",las=1)
boxplot(MG48h~ SOX6CG,data= SOX6B,main="Gráfica boxplot",col=c("black","red","blue"),border=c("darkblue","darkblue"),range=0)

shapiro.test(SOX6B$MG48h)

#CGeptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(SOX6B$MG48h[SOX6B$SOX6CG=="CC"]) 
shapiro.test(SOX6B$MG48h[SOX6B$SOX6CG=="CG"]) 
shapiro.test(SOX6B$MG48h[SOX6B$SOX6CG=="GG"]) 
#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6B$MG48h[SOX6B$SOX6CG=="CC"],pch=19,col="red")
plot(SOX6B$MG48h[SOX6B$SOX6CG=="CG"],pch=19,col="black")
plot(SOX6B$MG48h[SOX6B$SOX6CG=="GG"],pch=19,col="blue")

Box.test(SOX6B$MG48h[SOX6B$SOX6CG=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$MG48h[SOX6B$SOX6CG=="CG"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$MG48h[SOX6B$SOX6CG=="GG"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6B<-aov(MG48h~SOX6CG,data=SOX6B)   
summary(aovSOX6B)

tapply(SOX6B$MD,factor(SOX6B$SOX6CG),mean,na.rm=TRUE)
hist(SOX6B$MD)
boxplot(MD~SOX6CG,data=SOX6B)
qqnorm(SOX6B $MD, pch=20,col="red",cex=1.5, main = "MD")
barplot(tapply(SOX6B$MD,SOX6B$SOX6CG,mean),ylim=c(0,10),col=c("black","red","blue"), space=1,border="darkblue",main="MD",las=1)
boxplot(MD~ SOX6CG,data= SOX6B,main="Gráfica boxplot",col=c("black","red","blue"),border=c("darkblue","darkblue"),range=0)
shapiro.test(SOX6B$MD)
#CGeptamos El supuesto de normalidad si el  p-value es mayor a 0.01
shapiro.test(SOX6B$MD[SOX6B$SOX6CG=="CC"]) 
shapiro.test(SOX6B$MD[SOX6B$SOX6CG=="CG"]) 
shapiro.test(SOX6B$MD[SOX6B$SOX6CG=="GG"]) 
#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6B$MD[SOX6B$SOX6CG=="CC"],pch=19,col="red")
plot(SOX6B$MD[SOX6B$SOX6CG=="CG"],pch=19,col="black")
plot(SOX6B$MD[SOX6B$SOX6CG=="GG"],pch=19,col="blue")


Box.test(SOX6B$MD[SOX6B$SOX6CG=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$MD[SOX6B$SOX6CG=="CG"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$MD[SOX6B$SOX6CG=="GG"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6B<-aov(MD~SOX6CG,data=SOX6B)   
summary(aovSOX6B)

tapply(SOX6B$MC,factor(SOX6B$SOX6CG),mean,na.rm=TRUE)
hist(SOX6B$MC)
boxplot(MC~SOX6CG,data=SOX6B)
qqnorm(SOX6B $MC, pch=20,col="red",cex=1.5, main = "MC")
barplot(tapply(SOX6B$MC,SOX6B$SOX6CG,mean),ylim=c(0,40),col=c("black","red","blue"), space=1,border="darkblue",main="MC",las=1)
boxplot(MC~ SOX6CG,data= SOX6B,main="Gráfica boxplot",col=c("black","red","blue"),border=c("darkblue","darkblue"),range=0)
shapiro.test(SOX6B$MC)
#CGeptamos El supuesto de normalidad si el  p-value es mayor a 0.01
shapiro.test(SOX6B$MC[SOX6B$SOX6CG=="CC"]) 
shapiro.test(SOX6B$MC[SOX6B$SOX6CG=="CG"]) 
shapiro.test(SOX6B$MC[SOX6B$SOX6CG=="GG"]) 
#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6B$MC[SOX6B$SOX6CG=="CC"],pch=19,col="red")
plot(SOX6B$MC[SOX6B$SOX6CG=="CG"],pch=19,col="black")
plot(SOX6B$MC[SOX6B$SOX6CG=="GG"],pch=19,col="blue")

Box.test(SOX6B$MC[SOX6B$SOX6CG=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$MC[SOX6B$SOX6CG=="CG"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$MC[SOX6B$SOX6CG=="GG"],lag=1,type=c("Ljung-Box"))


#ANOVA
aovSOX6B<-aov(MC~SOX6CG,data=SOX6B)   
summary(aovSOX6B)
tapply(SOX6B$T,factor(SOX6B$SOX6CG),mean,na.rm=TRUE)
hist(SOX6B$T)
boxplot(T~SOX6CG,data=SOX6B)
qqnorm(SOX6B $T, pch=20,col="red",cex=1.5, main = "T")
barplot(tapply(SOX6B$T,SOX6B$SOX6CG,mean),ylim=c(0,10),col=c("black","red","blue"), space=1,border="darkblue",main="T",las=1)
boxplot(T~ SOX6CG,data= SOX6B,main="Gráfica boxplot",col=c("black","red","blue"),border=c("darkblue","darkblue"),range=0)
shapiro.test(SOX6B$T)
#CGeptamos El supuesto de normalidad si el  p-value es mayor a 0.01
shapiro.test(SOX6B$T[SOX6B$SOX6CG=="CC"]) 
shapiro.test(SOX6B$T[SOX6B$SOX6CG=="CG"]) 
shapiro.test(SOX6B$T[SOX6B$SOX6CG=="GG"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6B$T[SOX6B$SOX6CG=="CC"],pch=19,col="red")
plot(SOX6B$T[SOX6B$SOX6CG=="CG"],pch=19,col="black")
plot(SOX6B$T[SOX6B$SOX6CG=="GG"],pch=19,col="blue")


Box.test(SOX6B$T[SOX6B$SOX6CG=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$T[SOX6B$SOX6CG=="CG"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$T[SOX6B$SOX6CG=="GG"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6B<-aov(T~SOX6CG,data=SOX6B)   
summary(aovSOX6B)

tapply(SOX6B$M, factor(SOX6B$SOX6CG),mean,na.rm=TRUE)
hist(SOX6B$M)
boxplot(M~SOX6CG,data=SOX6B)
qqnorm(SOX6B $M, pch=20,col="red",cex=1.5, main = "M")
barplot(tapply(SOX6B$M,SOX6B$SOX6CG,mean),ylim=c(0,10),col=c("black","red","blue"), space=1,border="darkblue",main="M",las=1)
boxplot(M~ SOX6CG,data= SOX6B,main="Gráfica boxplot",col=c("black","red","blue"),border=c("darkblue","darkblue"),range=0)

shapiro.test(SOX6B$M)
#CGeptamos El supuesto de normalidad si el  p-value es mayor a 0.01
shapiro.test(SOX6B$M[SOX6B$SOX6CG=="CC"]) 
shapiro.test(SOX6B$M[SOX6B$SOX6CG=="CG"]) 
shapiro.test(SOX6B$M[SOX6B$SOX6CG=="GG"]) 
#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6B$M[SOX6B$SOX6CG=="CC"],pch=19,col="red")
plot(SOX6B$M[SOX6B$SOX6CG=="CG"],pch=19,col="black")
plot(SOX6B$M[SOX6B$SOX6CG=="CG"],pch=19,col="blue")

Box.test(SOX6B$M[SOX6B$SOX6CG=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$M[SOX6B$SOX6CG=="CG"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$M[SOX6B$SOX6CG=="GG"],lag=1,type=c("Ljung-Box"))
#ANOVA
aovSOX6B<-aov(M~SOX6CG,data=SOX6B)   
summary(aovSOX6B)

tapply(SOX6B$H,factor(SOX6B$SOX6CG),mean,na.rm=TRUE)
hist(SOX6B$H)
boxplot(H~SOX6CG,data=SOX6B)
qqnorm(SOX6B $H, pch=20,col="red",cex=1.5, main = "H")
barplot(tapply(SOX6B$H,SOX6B$SOX6CG,mean),ylim=c(0,10),col=c("black","red","blue"), space=1,border="darkblue",main="H",las=1)
boxplot(H~ SOX6CG,data= SOX6B,main="Gráfica boxplot",col=c("black","red","blue"),border=c("darkblue","darkblue"),range=0)

shapiro.test(SOX6B$H)
#CGeptamos El supuesto de normalidad si el  p-value es mayor a 0.01
shapiro.test(SOX6B$H[SOX6B$SOX6CG=="CC"]) 
shapiro.test(SOX6B$H[SOX6B$SOX6CG=="CG"]) 
shapiro.test(SOX6B$H[SOX6B$SOX6CG=="GG"]) 
#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(SOX6B$H[SOX6B$SOX6CG=="CC"],pch=19,col="red")
plot(SOX6B$M[SOX6B$SOX6CG=="CG"],pch=19,col="black")
plot(SOX6B$M[SOX6B$SOX6CG=="CG"],pch=19,col="blue")

Box.test(SOX6B$H[SOX6B$SOX6CG=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$H[SOX6B$SOX6CG=="CG"],lag=1,type=c("Ljung-Box"))
Box.test(SOX6B$H[SOX6B$SOX6CG=="GG"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovSOX6B<-aov(H~SOX6CG,data=SOX6B)   
summary(aovSOX6B)
