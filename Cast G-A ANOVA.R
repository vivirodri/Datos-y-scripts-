CASTGAB <-read.csv2("CASTGAB.csv")
library(FactoMineR)
summary(CASTGAB)
head(CASTGAB)
names(CASTGAB) 
shapiro.test(CASTGAB$pH45min)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTGAB$pH45min[CASTGAB$CASTG.A=="GG"]) 
shapiro.test(CASTGAB$pH45min[CASTGAB$CASTG.A=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTGAB$pH45min[CASTGAB$CASTG.A=="GG"],pch=19,col="red")
plot(CASTGAB$pH45min[CASTGAB$CASTG.A=="GA"],pch=19,col="black")

Box.test(CASTGAB$pH45min[CASTGAB$CASTG.A=="GG"],lag=1,type=c("Ljung-Box"))
Box.test(CASTGAB$pH45min[CASTGAB$CASTG.A==" GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTGAB<-aov(pH45min~CASTG.A,data=CASTGAB)   
summary(aovCASTGAB)



  
  PH 24HS Y CAST

tapply(CASTGAB$pH24h,factor(CASTGAB$CASTG.A),mean,na.rm=TRUE)
hist(CASTGAB$pH24h)


boxplot(pH24h~CASTG.A,data=CASTGAB)

qqnorm(CASTGAB $pH24h, pch=20,col="red",cex=1.5, main = "pH24h")

barplot(tapply(CASTGAB$pH24h,CASTGAB$CASTG.A,mean),ylim=c(0,7),col=c("black","red"), space=1,border="darkblue",main="pH24h",las=1)
boxplot(pH24h~ CASTG.A,data= CASTGAB,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTGAB$pH24h)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTGAB$pH24h[CASTGAB$CASTG.A=="GG"]) 
shapiro.test(CASTGAB$pH24h[CASTGAB$CASTG.A=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTGAB$pH24h[CASTGAB$CASTG.A=="GG"],pch=19,col="red")
plot(CASTGAB$pH24h[CASTGAB$CASTG.A=="GA"],pch=19,col="black")

Box.test(CASTGAB$pH24h[CASTGAB$CASTG.A=="GG"],lag=1,type=c("Ljung-Box"))
Box.test(CASTGAB$pH24h[CASTGAB$CASTG.A=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTGAB<-aov(pH24h~CASTG.A,data=CASTGAB)   
summary(aovCASTGAB)


tapply(CASTGAB$L.,factor(CASTGAB$CASTG.A),mean,na.rm=TRUE)
hist(CASTGAB$L.)
boxplot(L.~CASTG.A,data=CASTGAB)
qqnorm(CASTGAB $L., pch=20,col="red",cex=1.5, main = "pH24h")
barplot(tapply(CASTGAB$L.,CASTGAB$CASTG.A,mean),ylim=c(0,60),col=c("black","red"), space=1,border="darkblue",main="L*",las=1)
boxplot(L.~CASTG.A,data=CASTGAB,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTGAB$L.)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTGAB$L.[CASTGAB$CASTG.A=="GG"]) 
shapiro.test(CASTGAB$L.[CASTGAB$CASTG.A=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTGAB$L.[CASTGAB$CASTG.A=="GG"],pch=19,col="red")
plot(CASTGAB$L.[CASTGAB$CASTG.A=="GA"],pch=19,col="black")

Box.test(CASTGAB$L.[CASTGAB$CASTG.A=="GG"],lag=1,type=c("Ljung-Box"))
Box.test(CASTGAB$L.[CASTGAB$CASTG.A=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTGAB<-aov(L.~CASTG.A,data=CASTGAB)   
summary(aovCASTGAB)


CASTGA y a.
tapply(CASTGAB$a.,factor(CASTGAB$CASTG.A),mean,na.rm=TRUE)
hist(CASTGAB$a.)


boxplot(a.~CASTG.A,data=CASTGAB)

qqnorm(CASTGAB $a., pch=20,col="red",cex=1.5, main = "a*")

barplot(tapply(CASTGAB$a.,CASTGAB$CASTG.A,mean),ylim=c(0,60),col=c("black","red"), space=1,border="darkblue",main="a*",las=1)
boxplot(a.~ CASTG.A,data= CASTGAB,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTGAB$a.)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTGAB$a.[CASTGAB$CASTG.A=="GG"]) 
shapiro.test(CASTGAB$a.[CASTGAB$CASTG.A=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTGAB$a.[CASTGAB$CASTG.A=="GG"],pch=19,col="red")
plot(CASTGAB$a.[CASTGAB$CASTG.A=="GA"],pch=19,col="black")

Box.test(CASTGAB$a.[CASTGAB$CASTG.A=="GG"],lag=1,type=c("Ljung-Box"))
Box.test(CASTGAB$a.[CASTGAB$CASTG.A=="GA"],lag=1,type=c("Ljung-Box"))


  CAST Y b.
tapply(CASTGAB$b.,factor(CASTGAB$CASTG.A),mean,na.rm=TRUE)
hist(CASTGAB$b.)


boxplot(b.~CASTG.A,data=CASTGAB)

qqnorm(CASTGAB $b., pch=20,col="red",cex=1.5, main = "b*")

barplot(tapply(CASTGAB$b.,CASTGAB$CASTG.A,mean),ylim=c(0,60),col=c("black","red"), space=1,border="darkblue",main="b*",las=1)
boxplot(b.~ CASTG.A,data= CASTGAB,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTGAB$b.)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTGAB$b.[CASTGAB$CASTG.A=="GG"]) 
shapiro.test(CASTGAB$b.[CASTGAB$CASTG.A=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTGAB$b.[CASTGAB$CASTG.A=="GG"],pch=19,col="red")
plot(CASTGAB$b.[CASTGAB$CASTG.A=="GA"],pch=19,col="black")

Box.test(CASTGAB$b.[CASTGAB$CASTG.A=="GG"],lag=1,type=c("Ljung-Box"))
Box.test(CASTGAB$b.[CASTGAB$CASTG.A=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTGAB<-aov(b.~CASTG.A,data=CASTGAB)   


  CASTGAB y MG24h
tapply(CASTGAB$MG24h,factor(CASTGAB$CASTG.A),mean,na.rm=TRUE)
hist(CASTGAB$MG24h)


boxplot(MG24h~CASTG.A,data=CASTGAB)

qqnorm(CASTGAB $MG24h, pch=20,col="red",cex=1.5, main = "MG24h")

barplot(tapply(CASTGAB$MG24h,CASTGAB$CASTG.A,mean),ylim=c(0,10),col=c("black","red"), space=1,border="darkblue",main="MG24h",las=1)
boxplot(MG24h~ CASTG.A,data= CASTGAB,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTGAB$MG24h)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTGAB$MG24h[CASTGAB$CASTG.A=="GG"]) 
shapiro.test(CASTGAB$MG24h[CASTGAB$CASTG.A=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTGAB$MG24h[CASTGAB$CASTG.A=="GG"],pch=19,col="red")
plot(CASTGAB$MG24h[CASTGAB$CASTG.A=="GA"],pch=19,col="black")

Box.test(CASTGAB$MG24h[CASTGAB$CASTG.A=="GG"],lag=1,type=c("Ljung-Box"))
Box.test(CASTGAB$MG24h[CASTGAB$CASTG.A=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTGAB<-aov(MG24h~CASTG.A,data=CASTGAB)   
summary(aovCASTGAB)


mean(carnern$MG24h,na.rm=TRUE) 
tapply(carnern$MG24h,factor(carnern$RN),mean,na.rm=TRUE) 
plot(MG24h~RN,data=carnern)
boxplot(MG24h~RN,data=carnern)
barplot(tapply(carnern$MG24h,carnern$RN,mean),ylim=c(0,7),col=c("black","red","green","blue","purple"), space=1,border="darkblue",main="MG 24h",las=1)
hist(carnern$MG24h)
qqnorm(carnern$MG24h,pch=20,col="red",cex=1.5, main = "MG 24h")        
boxplot(MG24h~RN,data=carnern,main="Gráfica boxplot",col=c("black","red","green","blue","purple"),border=c("darkblue","darkblue","darkblue","darkblue","darkblue"),range=0)

RN Y MG 48h
tapply(CASTGAB$MG48h,factor(CASTGAB$CASTG.A),mean,na.rm=TRUE)
hist(CASTGAB$MG48h)


boxplot(MG48h~CASTG.A,data=CASTGAB)

qqnorm(CASTGAB $MG48h, pch=20,col="red",cex=1.5, main = "MG24h")

barplot(tapply(CASTGAB$MG48h,CASTGAB$CASTG.A,mean),ylim=c(0,10),col=c("black","red"), space=1,border="darkblue",main="MG48h",las=1)
boxplot(MG48h~ CASTG.A,data= CASTGAB,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTGAB$MG48h)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTGAB$MG48h[CASTGAB$CASTG.A=="GG"]) 
shapiro.test(CASTGAB$MG48h[CASTGAB$CASTG.A=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTGAB$MG48h[CASTGAB$CASTG.A=="GG"],pch=19,col="red")
plot(CASTGAB$MG48h[CASTGAB$CASTG.A=="GA"],pch=19,col="black")

Box.test(CASTGAB$MG48h[CASTGAB$CASTG.A=="GG"],lag=1,type=c("Ljung-Box"))
Box.test(CASTGAB$MG48h[CASTGAB$CASTG.A=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTGAB<-aov(MG48h~CASTG.A,data=CASTGAB)   
summary(aovCASTGAB)





RN y MD

tapply(CASTGAB$MD,factor(CASTGAB$CASTG.A),mean,na.rm=TRUE)
hist(CASTGAB$MD)


boxplot(MD~CASTG.A,data=CASTGAB)

qqnorm(CASTGAB $MD, pch=20,col="red",cex=1.5, main = "MD")

barplot(tapply(CASTGAB$MD,CASTGAB$CASTG.A,mean),ylim=c(0,10),col=c("black","red"), space=1,border="darkblue",main="MD",las=1)
boxplot(MD~ CASTG.A,data= CASTGAB,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTGAB$MD)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTGAB$MD[CASTGAB$CASTG.A=="GG"]) 
shapiro.test(CASTGAB$MD[CASTGAB$CASTG.A=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTGAB$MD[CASTGAB$CASTG.A=="GG"],pch=19,col="red")
plot(CASTGAB$MD[CASTGAB$CASTG.A=="GA"],pch=19,col="black")

Box.test(CASTGAB$MD[CASTGAB$CASTG.A=="GG"],lag=1,type=c("Ljung-Box"))
Box.test(CASTGAB$MD[CASTGAB$CASTG.A=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTGAB<-aov(MD~CASTG.A,data=CASTGAB)   
summary(aovCASTGAB)



CAST y MERMAS POR COCCIÓN MC
tapply(CASTGAB$MC,factor(CASTGAB$CASTG.A),mean,na.rm=TRUE)
hist(CASTGAB$MC)


boxplot(MC~CASTG.A,data=CASTGAB)

qqnorm(CASTGAB $MC, pch=20,col="red",cex=1.5, main = "MC")

barplot(tapply(CASTGAB$MC,CASTGAB$CASTG.A,mean),ylim=c(0,40),col=c("black","red"), space=1,border="darkblue",main="MC",las=1)
boxplot(MC~ CASTG.A,data= CASTGAB,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTGAB$MC)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTGAB$MC[CASTGAB$CASTG.A=="GG"]) 
shapiro.test(CASTGAB$MC[CASTGAB$CASTG.A=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTGAB$MC[CASTGAB$CASTG.A=="GG"],pch=19,col="red")
plot(CASTGAB$MC[CASTGAB$CASTG.A=="GA"],pch=19,col="black")

Box.test(CASTGAB$MC[CASTGAB$CASTG.A=="GG"],lag=1,type=c("Ljung-Box"))
Box.test(CASTGAB$MC[CASTGAB$CASTG.A=="GA"],lag=1,type=c("Ljung-Box"))

   #ANOVA
aovCASTGAB<-aov(MC~CASTG.A,data=CASTGAB)   
summary(aovCASTGAB)


CAST Y Terneza T

tapply(CASTGAB$T,factor(CASTGAB$CASTG.A),mean,na.rm=TRUE)
hist(CASTGAB$T)


boxplot(T~CASTG.A,data=CASTGAB)

qqnorm(CASTGAB $T, pch=20,col="red",cex=1.5, main = "T")

barplot(tapply(CASTGAB$T,CASTGAB$CASTG.A,mean),ylim=c(0,10),col=c("black","red"), space=1,border="darkblue",main="T",las=1)
boxplot(T~ CASTG.A,data= CASTGAB,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTGAB$T)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTGAB$T[CASTGAB$CASTG.A=="GG"]) 
shapiro.test(CASTGAB$T[CASTGAB$CASTG.A=="GA"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTGAB$T[CASTGAB$CASTG.A=="GG"],pch=19,col="red")
plot(CASTGAB$T[CASTGAB$CASTG.A=="GA"],pch=19,col="black")

Box.test(CASTGAB$T[CASTGAB$CASTG.A=="GG"],lag=1,type=c("Ljung-Box"))
Box.test(CASTGAB$T[CASTGAB$CASTG.A=="GA"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTGAB<-aov(T~CASTG.A,data=CASTGAB)   
summary(aovCASTGAB)


