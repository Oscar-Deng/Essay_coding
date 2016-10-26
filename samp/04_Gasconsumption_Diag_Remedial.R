library(car)
library(lmtest)
#library(nortest)
setwd("C:\\Users\\user\\Desktop\\")
GasData=read.table("gasconsumption.txt",header=T)
NGasData=GasData[,-2]
NGasData$ET <- as.factor(NGasData$ET)
is.factor(NGasData$ET)

set.seed(135)
n=dim(NGasData)[1]
Sindex=sample(n,round(n*0.8))
Train=NGasData[Sindex,]
Test=NGasData[-Sindex,]  

#NGasData1 <- NGasData[sample(nrow(NGasData)),]
Train=NGasData[1:35,]
Test=NGasData[36:38,]
attach(Train)
pairs(Train)
cor(cbind(MPG,WT,DIS,NC,HP,ACC))
#Due to the limited sample size, we don't use ACC anad HP in this example.
M1=lm(MPG~.,data=Train)
summary(M1)

#Diagnostics I#
e=residuals(M1)
es=rstandard(M1)
yhat=fitted.values(M1)

plot(es,type = "l",col='2')
dwtest(M1)#Durbin-Watson test
acf(es,ci=0.99)

plot(yhat,es,col='2')
#plot(yhat,es,col='2')
abline(h=0) #e-yhat plot function form &變異數一致性問題。
residualPlots(M1,type="rstandard",quadratic=F)

##resettest(M1,power=seq(0.5,2,by=0.5),type = "regressor") # Ramsey's reset goodness of fit test
#raintest(M1)#Rainbow goodness of fit test
#bptest(M1) #Breusch and Pagan test

qqPlot(M1)
#lillie.test(e)#KS test for normality
shapiro.test(e)#Shapiro-Wilk Normality Test

### Remedial I ###
library(MASS)
boxcox(M1)  #取log，因為取倒數會呈反向變動。
lnMPG=log(MPG)
M2=lm(lnMPG~.,data=Train)
summary(M2)

#Diagnostics II#
e=residuals(M2)
es=rstudent(M2)
yhat=fitted.values(M2)

plot(es,type = "l",col='2')
dwtest(M2)#Durbin-Watson test

plot(yhat,es,col='2')
#plot(yhat,es,col='2')
abline(h=0) #e-yhat plot
residualPlots(M2,type="rstandard",quadratic=F)

##resettest(M2,power=seq(0.5,2,by=0.5),type = "regressor") # Ramsey's reset goodness of fit test
#raintest(M2)#Rainbow goodness of fit test
#bptest(M2) #Breusch and Pagan test

qqPlot(M2)
#lillie.test(e)#KS test for normality
shapiro.test(e)#Shapiro-Wilk Normality Test

M3=lm(lnMPG~WT+poly(DIS,3)+NC+ET+ACC+poly(HP,3),data=Train)
summary(M3)

step(M3)

M4 <- lm(lnMPG ~ WT + poly(DIS, 2) + NC + ET + poly(HP, 3),data=Train)
summary(M4)  #M4 =>經過變數選擇的模型，DIS的poly 3 -> 2
vif(M4)  #poly高沒關係(poly共線性沒有影響)

M5 <- lm(lnMPG ~poly(DIS, 2) + NC + ET + poly(HP, 3),data=Train) #-WT
summary(M5)
vif(M5)
#-----------------------------------------------------------------------------
M6 <- lm(lnMPG ~ WT + poly(DIS, 2) + ET + poly(HP, 3),data=Train) #-NC
summary(M6)  
vif(M6)

influenceIndexPlot(M6,id.n=3)
influencePlot(M6,id.n=3)
p1 <- predict(M6,newdata=Test) #prediction and corresponding CI for mean response
#predict(M2,New,interval="prediction") #prediction and corresponding CI for new obs
anova(M6)
MSPR1 <- mean((p1-log(Test$MPG))^2)
MSPR2 <- mean((exp(p1)-Test$MPG)^2)
#-----------------------------------------------------------------------------

#Diagnostics II#
e=residuals(M3)
es=rstudent(M3)
yhat=fitted.values(M3)

residualPlots(M3,type="rstandard",quadratic=F)
boxcox(M3)
#....No polynomail is needed
#Accept M2

###################
#confint(M2,level=0.99)
#0.6+.0=Test[,c(3,4,5)]
#predict(M2,New,interval="confidence") #prediction and corresponding CI for mean response

#attach(GasData)
#stem(MPG)
#stem(WT)
#stem(DIS)
#stem(NC)
#barplot(table(ET))
#summary(ET)
#sum(ET==0)/dim(GasData)[1]
#pairs(cbind(MPG,WT,DIS,NC,HP,ACC))
#scatterplotMatrix(GasData)
#scatterplot(MPG~WT,, id.method="identify")
#cor(cbind(MPG,WT,DIS,NC,HP,ACC))
#M1=lm(MPG~WT+DIS+NC+HP+ACC+ET)
#summary(M1)
#anova(M1)#

#confint(M1,level=0.95)
#confint(M1,level=0.99)
#M2=lm(MPG~WT+DIS+NC)
#detach(GasData)
#WT=c(2.5,3.5,4.5)
#DIS=c(250,350,450)
#NC=c(4,5,6)
#New=data.frame(cbind(WT,DIS,NC))
#predict(M2,New,interval="confidence") #prediction and corresponding CI for mean response
#predict(M2,New,interval="prediction") #prediction and corresponding CI for new obs
