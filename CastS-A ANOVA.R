CASTAC <-read.csv2("CASTAC.csv")
library(FactoMineR)
summary(CASTAC)
head(CASTAC)
names(CASTAC) 
tapply(CASTAC$pH45min,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
hist(CASTAC$pH45min)


boxplot(pH45min~CASTA.C,data=CASTAC)

qqnorm(CASTAC $pH45min, pch=20,col="red",cex=1.5, main = "pH 45min")

barplot(tapply(CASTAC$pH45min,CASTAC$CASTA.C,mean),ylim=c(0,7),col=c("black","red"), space=1,border="darkblue",main="pH 45min",las=1)
boxplot(pH45min~ CASTA.C,data= CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTAC$pH45min)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTAC$pH45min[CASTAC$CASTA.C=="CC"]) 
shapiro.test(CASTAC$pH45min[CASTAC$CASTA.C=="AC"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTAC$pH45min[CASTAC$CASTA.C=="CC"],pch=19,col="red")
plot(CASTAC$pH45min[CASTAC$CASTA.C=="AC"],pch=19,col="black")

Box.test(CASTAC$pH45min[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(CASTAC$pH45min[CASTAC$CASTA.C==" AC"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTAC<-aov(pH45min~CASTA.C,data=CASTAC)   
summary(aovCASTAC)

tapply(CASTAC$pH45min,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
> hist(CASTAC$pH45min)
> 
  > 
  > boxplot(pH45min~CASTA.C,data=CASTAC)
> 
  > qqnorm(CASTAC $pH45min, pch=20,col="red",cex=1.5, main = "pH 45min")
> 
  > barplot(tapply(CASTAC$pH45min,CASTAC$CASTA.C,mean),ylim=c(0,7),col=c("black","red"), space=1,border="darkblue",main="pH 45min",las=1)
> boxplot(pH45min~ CASTA.C,data= CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)
> 
  > 
  > 
  > shapiro.test(CASTAC$pH45min)
si el  p-value es mayor a 0.01
  > 
  > shapiro.test(CASTAC$pH45min[CASTAC$CASTA.C=="CC"]) 

Shapiro-Wilk normality test

data:  CASTAC$pH45min[CASTAC$CASTA.C == "CC"]

> shapiro.test(CASTAC$pH45min[CASTAC$CASTA.C=="AC"]) 

Shapiro-Wilk normality test

data:  CASTAC$pH45min[CASTAC$CASTA.C == "AC"]

> 
  > #Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
  > #Independencia
  > plot(CASTAC$pH45min[CASTAC$CASTA.C=="CC"],pch=19,col="red")
> plot(CASTAC$pH45min[CASTAC$CASTA.C=="AC"],pch=19,col="black")
> 
  > Box.test(CASTAC$pH45min[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))

Box-Ljung test

data:  CASTAC$pH45min[CASTAC$CASTA.C == "CC"]



> Box.test(CASTAC$pH45min[CASTAC$CASTA.C=="AC"],lag=1,type=c("Ljung-Box"))

Box-Ljung test

data:  CASTAC$pH45min[CASTAC$CASTA.C == "AC"]



> #ANOVA
  > aovCASTAC<-aov(pH45min~CASTA.C,data=CASTAC)   
> summary(aovCASTAC)



PH 24HS Y CAST

tapply(CASTAC$pH24h,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
hist(CASTAC$pH24h)


boxplot(pH24h~CASTA.C,data=CASTAC)

qqnorm(CASTAC $pH24h, pch=20,col="red",cex=1.5, main = "pH24h")

barplot(tapply(CASTAC$pH24h,CASTAC$CASTA.C,mean),ylim=c(0,7),col=c("black","red"), space=1,border="darkblue",main="pH24h",las=1)
boxplot(pH24h~ CASTA.C,data= CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTAC$pH24h)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTAC$pH24h[CASTAC$CASTA.C=="CC"]) 
shapiro.test(CASTAC$pH24h[CASTAC$CASTA.C=="AC"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTAC$pH24h[CASTAC$CASTA.C=="CC"],pch=19,col="red")
plot(CASTAC$pH24h[CASTAC$CASTA.C=="AC"],pch=19,col="black")

Box.test(CASTAC$pH24h[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(CASTAC$pH24h[CASTAC$CASTA.C=="AC"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTAC<-aov(pH24h~CASTA.C,data=CASTAC)   
summary(aovCASTAC) 













tapply(CASTAC$L.,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
hist(CASTAC$L.)
boxplot(L.~CASTA.C,data=CASTAC)
qqnorm(CASTAC $L., pch=20,col="red",cex=1.5, main = "pH24h")
barplot(tapply(CASTAC$L.,CASTAC$CASTA.C,mean),ylim=c(0,60),col=c("black","red"), space=1,border="darkblue",main="L*",las=1)
boxplot(L.~CASTA.C,data=CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)

shapiro.test(CASTAC$L.)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTAC$L.[CASTAC$CASTA.C=="CC"]) 
shapiro.test(CASTAC$L.[CASTAC$CASTA.C=="AC"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTAC$L.[CASTAC$CASTA.C=="CC"],pch=19,col="red")
plot(CASTAC$L.[CASTAC$CASTA.C=="AC"],pch=19,col="black")

Box.test(CASTAC$L.[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(CASTAC$L.[CASTAC$CASTA.C=="AC"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTAC<-aov(L.~CASTA.C,data=CASTAC)   
summary(aovCASTAC)




CASTAC y a.
tapply(CASTAC$a.,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
hist(CASTAC$a.)


boxplot(a.~CASTA.C,data=CASTAC)

qqnorm(CASTAC $a., pch=20,col="red",cex=1.5, main = "a*")

barplot(tapply(CASTAC$a.,CASTAC$CASTA.C,mean),ylim=c(0,60),col=c("black","red"), space=1,border="darkblue",main="a*",las=1)
boxplot(a.~ CASTA.C,data= CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTAC$a.)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTAC$a.[CASTAC$CASTA.C=="CC"]) 
shapiro.test(CASTAC$a.[CASTAC$CASTA.C=="AC"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTAC$a.[CASTAC$CASTA.C=="CC"],pch=19,col="red")
plot(CASTAC$a.[CASTAC$CASTA.C=="AC"],pch=19,col="black")

Box.test(CASTAC$a.[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(CASTAC$a.[CASTAC$CASTA.C=="AC"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTAC<-aov(a.~CASTA.C,data=CASTAC)   
summary(aovCASTAC)

  CAST Y b.
tapply(CASTAC$b.,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
hist(CASTAC$b.)


boxplot(b.~CASTA.C,data=CASTAC)

qqnorm(CASTAC $b., pch=20,col="red",cex=1.5, main = "b*")

barplot(tapply(CASTAC$b.,CASTAC$CASTA.C,mean),ylim=c(0,60),col=c("black","red"), space=1,border="darkblue",main="b*",las=1)
boxplot(b.~ CASTA.C,data= CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTAC$b.)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTAC$b.[CASTAC$CASTA.C=="CC"]) 
shapiro.test(CASTAC$b.[CASTAC$CASTA.C=="AC"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTAC$b.[CASTAC$CASTA.C=="CC"],pch=19,col="red")
plot(CASTAC$b.[CASTAC$CASTA.C=="AC"],pch=19,col="black")

Box.test(CASTAC$b.[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(CASTAC$b.[CASTAC$CASTA.C=="AC"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTAC<-aov(b.~CASTA.C,data=CASTAC)   


CASTAC y MG24h
tapply(CASTAC$MG24h,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
hist(CASTAC$MG24h)


boxplot(MG24h~CASTA.C,data=CASTAC)

qqnorm(CASTAC $MG24h, pch=20,col="red",cex=1.5, main = "MG24h")

barplot(tapply(CASTAC$MG24h,CASTAC$CASTA.C,mean),ylim=c(0,10),col=c("black","red"), space=1,border="darkblue",main="MG24h",las=1)
boxplot(MG24h~ CASTA.C,data= CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTAC$MG24h)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTAC$MG24h[CASTAC$CASTA.C=="CC"]) 
shapiro.test(CASTAC$MG24h[CASTAC$CASTA.C=="AC"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTAC$MG24h[CASTAC$CASTA.C=="CC"],pch=19,col="red")
plot(CASTAC$MG24h[CASTAC$CASTA.C=="AC"],pch=19,col="black")

Box.test(CASTAC$MG24h[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(CASTAC$MG24h[CASTAC$CASTA.C=="AC"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTAC<-aov(MG24h~CASTA.C,data=CASTAC)   
summary(aovCASTAC)

> 
  > #ANOVA
  > aovCASTAC<-aov(MG24h~CASTA.C,data=CASTAC)   
> summary(aovCASTAC)
Df Sum Sq Mean Sq F value Pr(>F)
CASTA.C      1   6.34   6.338   2.291  0.138
Residuals   39 107.89   2.766










RN Y MG 48h
tapply(CASTAC$MG48h,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
hist(CASTAC$MG48h)
boxplot(MG48h~CASTA.C,data=CASTAC)

qqnorm(CASTAC $MG48h, pch=20,col="red",cex=1.5, main = "MG24h")

barplot(tapply(CASTAC$MG48h,CASTAC$CASTA.C,mean),ylim=c(0,10),col=c("black","red"), space=1,border="darkblue",main="MG48h",las=1)
boxplot(MG48h~ CASTA.C,data= CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTAC$MG48h)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTAC$MG48h[CASTAC$CASTA.C=="CC"]) 
shapiro.test(CASTAC$MG48h[CASTAC$CASTA.C=="AC"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTAC$MG48h[CASTAC$CASTA.C=="CC"],pch=19,col="red")
plot(CASTAC$MG48h[CASTAC$CASTA.C=="AC"],pch=19,col="black")

Box.test(CASTAC$MG48h[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(CASTAC$MG48h[CASTAC$CASTA.C=="AC"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTAC<-aov(MG48h~CASTA.C,data=CASTAC)   
summary(aovCASTAC)


  
  
  
  
  
  

CAST AC y MD

tapply(CASTAC$MD,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
hist(CASTAC$MD)
boxplot(MD~CASTA.C,data=CASTAC)
qqnorm(CASTAC $MD, pch=20,col="red",cex=1.5, main = "MD")
barplot(tapply(CASTAC$MD,CASTAC$CASTA.C,mean),ylim=c(0,10),col=c("black","red"), space=1,border="darkblue",main="MD",las=1)
boxplot(MD~ CASTA.C,data= CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)
shapiro.test(CASTAC$MD)
#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01
shapiro.test(CASTAC$MD[CASTAC$CASTA.C=="CC"]) 
shapiro.test(CASTAC$MD[CASTAC$CASTA.C=="AC"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTAC$MD[CASTAC$CASTA.C=="CC"],pch=19,col="red")
plot(CASTAC$MD[CASTAC$CASTA.C=="AC"],pch=19,col="black")

Box.test(CASTAC$MD[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(CASTAC$MD[CASTAC$CASTA.C=="AC"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTAC<-aov(MD~CASTA.C,data=CASTAC)   
summary(aovCASTAC)

  
  CAST y MERMAS POR COCCIÓN MC
tapply(CASTAC$MC,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
hist(CASTAC$MC)
boxplot(MC~CASTA.C,data=CASTAC)
qqnorm(CASTAC $MC, pch=20,col="red",cex=1.5, main = "MC")
barplot(tapply(CASTAC$MC,CASTAC$CASTA.C,mean),ylim=c(0,40),col=c("black","red"), space=1,border="darkblue",main="MC",las=1)
boxplot(MC~ CASTA.C,data= CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)
shapiro.test(CASTAC$MC)
#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01
shapiro.test(CASTAC$MC[CASTAC$CASTA.C=="CC"]) 
shapiro.test(CASTAC$MC[CASTAC$CASTA.C=="AC"]) 
#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTAC$MC[CASTAC$CASTA.C=="CC"],pch=19,col="red")
plot(CASTAC$MC[CASTAC$CASTA.C=="AC"],pch=19,col="black")

Box.test(CASTAC$MC[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(CASTAC$MC[CASTAC$CASTA.C=="AC"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTAC<-aov(MC~CASTA.C,data=CASTAC)   
summary(aovCASTAC)



              

> 
  
  
  
  
  
  
  CAST Y Terneza T

tapply(CASTAC$T,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
hist(CASTAC$T)


boxplot(T~CASTA.C,data=CASTAC)
qqnorm(CASTAC $T, pch=20,col="red",cex=1.5, main = "T")
barplot(tapply(CASTAC$T,CASTAC$CASTA.C,mean),ylim=c(0,10),col=c("black","red"), space=1,border="darkblue",main="T",las=1)
boxplot(T~ CASTA.C,data= CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)
shapiro.test(CASTAC$T)
#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01
shapiro.test(CASTAC$T[CASTAC$CASTA.C=="CC"]) 
shapiro.test(CASTAC$T[CASTAC$CASTA.C=="AC"]) 
#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTAC$T[CASTAC$CASTA.C=="CC"],pch=19,col="red")
plot(CASTAC$T[CASTAC$CASTA.C=="AC"],pch=19,col="black")

Box.test(CASTAC$T[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(CASTAC$T[CASTAC$CASTA.C=="AC"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTAC<-aov(T~CASTA.C,data=CASTAC)   
summary(aovCASTAC)



CAST Y MARMOLADO M

tapply(CASTAC$M,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
hist(CASTAC$M)

boxplot(M~CASTA.C,data=CASTAC)
barplot(tapply(CASTAC$M,CASTAC$CASTA.C,mean),ylim=c(0,10),col=c("black","red"), space=1,border="darkblue",main="M",las=1)
boxplot(M~ CASTA.C,data= CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)
shapiro.test(CASTAC$M)

Shapiro-Wilk normality test

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01
  > 
shapiro.test(CASTAC$M[CASTAC$CASTA.C=="CC"]) 



shapiro.test(CASTAC$M[CASTAC$CASTA.C=="AC"]) 


> 
  > #Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
  > #Independencia
  plot(CASTAC$M[CASTAC$CASTA.C=="CC"],pch=19,col="red")
plot(CASTAC$M[CASTAC$CASTA.C=="AC"],pch=19,col="black")
Box.test(CASTAC$M[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))

Box-Ljung test


Box.test(CASTAC$M[CASTAC$CASTA.C=="AC"],lag=1,type=c("Ljung-Box"))


> 
  > #ANOVA
aovCASTAC<-aov(M~CASTA.C,data=CASTAC)   
> summary(aovCASTAC)
              
-

tapply(CASTAC$H,factor(CASTAC$CASTA.C),mean,na.rm=TRUE)
hist(CASTAC$H)


boxplot(H~CASTA.C,data=CASTAC)

qqnorm(CASTAC $H, pch=20,col="red",cex=1.5, main = "H")

barplot(tapply(CASTAC$H,CASTAC$CASTA.C,mean),ylim=c(0,10),col=c("black","red"), space=1,border="darkblue",main="H",las=1)
boxplot(H~ CASTA.C,data= CASTAC,main="Gráfica boxplot",col=c("black","red"),border=c("darkblue","darkblue"),range=0)


shapiro.test(CASTAC$M)

#Aceptamos El supuesto de normalidad si el  p-value es mayor a 0.01

shapiro.test(CASTAC$H[CASTAC$CASTA.C=="CC"]) 
shapiro.test(CASTAC$H[CASTAC$CASTA.C=="AC"]) 

#Hubiéramos rechazado el supuesto de normalidad si p-value hubiera sido al menos para uno de los grupos menor a 0.05.
#Independencia
plot(CASTAC$H[CASTAC$CASTA.C=="CC"],pch=19,col="red")
plot(CASTAC$M[CASTAC$CASTA.C=="AC"],pch=19,col="black")

Box.test(CASTAC$H[CASTAC$CASTA.C=="CC"],lag=1,type=c("Ljung-Box"))
Box.test(CASTAC$H[CASTAC$CASTA.C=="AC"],lag=1,type=c("Ljung-Box"))

#ANOVA
aovCASTAC<-aov(H~CASTA.C,data=CASTAC)   
summary(aovCASTAC)
