GC <- read.table('gasconsumption.txt',header=T)
GC$ET <- as.factor(GC$ET)
stem(GC$MPG)
plot(GC$ET)
plot(GC)
#解釋散步圖矩陣：
#列的方向Y座標，行方向X座標
#MPG~GPM 可以數學換算(呈線性) => 單位換算(模型沒有意義)
#MPG~GPM 如100cm=1m
#所以需要移除MPG~GPM模型，程式碼如下
plot(GC[,-2],data=GC)
plot(GC[,c(6,1,3,4,5,7,8)],data=GC)
GC$ET
#解釋MPG~ET 的自變數x為何是[1,2]
#Levels: 0 1
#0會對應到1，1會對應到2
#看左右(0,1)資料中心點，有差異是有影響MPG
#If左右(0,1)中心點在同一水平上，資料是沒有影響的

#WT~DIS下
#If 自變數太相關會有參數估計的問題，形成共線性(Multicollinearity)問題
cor(GC[,-c(2,8)])
#除了ACC其他變數都跟MPG有強烈負相關
cov(GC[,-c(2,8)])
#MPG~MPG => 42.8673 => 等於MPG的變異數
var(GC$MPG)

#--------------------------------------------------------------------
#install.packages('car')
library(car)
GasData <- GC
str(GasData)
GasData$ET <- as.factor(GasData$ET)
attach(GasData)
stem(MPG)
stem(WT)
stem(DIS)
stem(NC)
stem(HP)
stem(ACC)
table(ET);plot(ET);barplot(table(ET))

plot(GasData,pch='.',col=2)
plot(GasData[,-2])
plot(GasData[,c(3,1,4:8)])
pairs(cbind(MPG,WT,DIS,NC))
pairs(~.,data=GasData)
pairs(~MPG+WT+DIS+NC,data=GasData)

scatterplotMatrix(GasData,smoother=F,diagonal='histogram')
scatterplot(MPG~WT,id.method='identify')
scatterplotMatrix(GasData,diagonal='histogram',span=8,spread=F)
scatterplotMatrix(GasData,diagonal='density',span=8,spread=F)
scatterplotMatrix(GasData,diagonal='histogram',smoother=F)

cor(GasData[,1:7])
?scatterplot #散步圖

#複回歸模型
M1 <- lm(MPG~WT+DIS+NC)
summary(M1)
#MPG^ = 59.6 -14.4*WT + 0.07*DIS -1.09*NC
#在其他自變數不變下，每增加1單位WT，MPG的平均值會下降14.4單位
#R^2 = SSR/SSTO = 0.8783，模型變異占總變異的87.83%
#複迴歸中 => H0: B1=B2=B3=0
#F*=MSR/MSE = 1.262e-15，Reject H0。
#結論：我所假設的模型參數中至少有一顯著不為零
anova(M1)
confint(M1,level=0.95)
#有95%信心在其他變數不變下，WT每增加1單位，MPG平均值會增加[-18.15,-10.7]單位
confint(M1,level=0.95)
detach(GasData)
WT=c(2.5,3.5,4.5)
DIS=c(250,350,450)
NC=c(4,5,6)
New=data.frame(cbind(WT,DIS,NC))
predict(M1,New,interval='confidence',level=0.95)
#有95%信心第二台車MPG平均值介於[22.7,33.4]之間
predict(M1,New,interval='prediction')

data(Prestige)
attach(Prestige)
#找出遺失值，算出扣掉遺失值比率如下：
crate <- sum(complete.cases(Prestige))/nrow(Prestige)
Prestige.good <- na.omit(Prestige)
detach(Prestige)
attach(Prestige.good)
contrasts(type)
Fmod <- lm(prestige~income+education+type)
summary(Fmod)
#type(Factor)，只要是Factor就會自動做dummy variable
Rmod <- lm(prestige~income+education)
anova(Fmod,Rmod,test='F')  #檢定type顯著不為0
detach(Prestige.good)

#-------------------------------------------------------

IOWAtest <- read.table('iowatest.txt',header=T,sep=',')
str(IOWAtest)
IOWAtest=IOWAtest[,-1]
stem(IOWAtest$Poverty)
stem(IOWAtest$Test)
barplot(table(IOWAtest$City))

contrasts(IOWAtest$City)
IOWAtest$City <- relevel(IOWAtest$City,ref='Waterloo') ##指定參考類別
contrasts(IOWAtest$City)

set.seed(123)
ind <- sample(nrow(IOWAtest),100)
TrainD <- IOWAtest[ind,]
TestD <- IOWAtest[-ind,]

TrainD=IOWAtest[1:120,]
TestD=IOWAtest[121:133,]
attach(TrainD)
pairs(TrainD)
cor(TrainD[,1:2])
mod1 <- lm(Test~.,data=TrainD)
summary(mod1)
mod2 <- lm(Test~.-City,data=TrainD)
summary(mod2)

anova(mod1,mod2)

e=residuals(mod1)
es=rstandard(mod1)
ed=rstudent(mod1)
yhat=fitted.values(mod1)

plot(yhat,e,col=2)
abline(h=0)


residualPlots(mod1,type='rstandard',quadratic=F)
qqPlot(mod1)
plot(e,type='l',col=2)
acf(es,ci=0.99)  ##藍色虛線(CI)，超過(突出來)就是有顯著性。圖形沒顯著(Good)
detach(TrainD)
pv1=predict(mod1,TestD,interval='confidence')

Labor=read.table("C:\\Users\\willy\\Desktop\\Reg\\2015-12-03_residuals\\Grocery.txt")
colnames(Labor)=c('TLabor','Nship','ILabor','Holiday')
Labor$Holiday <- as.factor(Labor$Holiday)
attach(Labor)
stem(TLabor)
stem(Nship)
stem(ILabor)

plot(Labor)
pairs(Labor)

library(car)
scatterplotMatrix(Labor,smoother=F,diagonal='histogram')
scatterplot(TLabor~ILabor,id.method='identify')
cor(Labor[,1:3])

mod1=lm(TLabor~.,data=Labor)
summary(mod1)
#6.236e+02 =>在其他自變數不變情況下，Holidy=1的類別total labor期望值比Holidy=0的類別total labor期望值多了6.236e+02(623.6)單位
anova(mod1)
confint(mod1,level=0.95)
#有95%信心在其他自變數不變的情況下，Holidy=1的類別total labor期望值比Holidy=0的類別total labor期望值多4.976064e+02(497.6)到7.495025e+02(749.5)單位

Nship=c(310000,320000,330000)
ILabor=c(6,7,8)
Holiday=c(0,0,1)
New=data.frame(cbind(Nship,ILabor,Holiday))
New$Holiday <- as.factor(New$Holiday)
predict(mod1,newdata=New,interval='confidence',level=0.95)
predict(mod1,newdata=New,interval='prediction',level=0.99)

#------------------------------------------------------------

IOWAtest <- read.table('iowatest.txt',header=T,sep=',')
IOWAtest=IOWAtest[,-1]

stem(IOWAtest$Poverty)
stem(IOWAtest$Test)
barplot(table(IOWAtest$City))

contrasts(IOWAtest$City)
IOWAtest$City=relevel(IOWAtest$City,ref='Waterloo')
contrasts(IOWAtest$City)

set.seed(123)
ind1 <- sample(nrow(IOWAtest),110)
TrainD=IOWAtest[ind1,]
TestD=IOWAtest[-ind1,]
attach(TrainD)
pairs(Test~.,data=TrainD)  #. 除了Test以外所有變數
cor(TrainD[,1:2])
cor(cbind(Poverty,Test))
#資料沒有特別離群值，自變數跟應變數關係明顯

mod1=lm(Test~.,data=TrainD)
summary(mod1)

mod2=lm(Test~.-City,data=TrainD)
summary(mod2)

anova(mod1,mod2)

e=residuals(mod1)
es=rstandard(mod1)  #標準化殘差
es1 <- rstudent(mod1) #去除後殘差
yhat=fitted.values(mod1)

modt <- lm(Test~poly(Poverty,2)+City,data=TrainD)
summary(modt)

library(MASS)
boxcox(mod1)
residualPlots(modt,type='rstandard',quadratic=F)
qqPlot(modt)
es=rstandard(modt)
acf(es,ci=0.99)

pv1=predict(mod1,newdata=TestD,interval='confidence')
pvt=predict(modt,newdata=TestD,interval='confidence')
MSPR1 <- mean((TestD$Test-pv1)^2)
MSPRt <- mean((TestD$Test-pvt)^2)
#MSPR越小，model預測能力越好

ST=read.table("stopping.txt",header=T)
attach(ST)
stem(X);stem(Y)
ind <- sample(nrow(ST),50)
TrainD=ST[ind,]
TestD=ST[-ind,]
pairs(Y~X,data=ST)
cor(TrainD)

mod1 <- lm(Y~X,data=ST)
summary(mod1)

e=residuals(mod1)
es=rstandard(mod1)
es1=rstudent(mod1)
yhat=fitted.values(mod1)

plot(yhat,es,col=2);abline(h=0)
residualPlots(mod1,type='rstandard',quadratic=F)
qqPlot(mod1)
es=rstandard(mod1)
acf(es,ci=0.99)
boxcox(mod1)

modt <- lm(Y^0.5~poly(X,2),data=TrainD)
summary(modt)
residualPlots(modt,type='rstandard',quadratic=F)
qqPlot(mod1)
es=rstandard(modt)
acf(es,ci=0.99)

#----------------------------------------------------
rawx = runif(200,min=0,max=10);hist(rawx)
X = exp(rawx)
ydev = rnorm(200,0,1)
Y = rawx + ydev

m1 <- lm(Y~X)
summary(m1)
plot (X,Y,col="red",pch=16)
title(main="A plot of Y on X")
abline(reg=m1,col='blue')
plot(m1,which=1)

m2 <- lm(log(Y+15)~X)
plot(X,Y+15,col="red",pch=16)
#=============================
X = runif(200,min=-10,max=10)
X = sort(X)
ydev = rnorm(200,0,4)
Y = 62.3 + 0.4 * X - 0.2 * X^2 + 0.03 * X^3 + ydev

m3a <- lm(Y~poly(X,3,raw=T)) #raw=T 用X原本的值去形成X^1、X^2、X^3
summary(m3a)
m3a1 <- lm(Y~poly(X,3)) #正規化X多項函數
summary(m3a1)
m4a <- lm(Y~poly(X,4))
summary(m4a)  #4次項不顯著

#-----------------------------------------------
StopD <- read.table('C:\\Users\\willy\\Desktop\\Reg\\2015-12-31\\MSPR\\stopping.txt',header=T)
colnames(StopD) <- c('Speed','Dist')
plot(StopD,col=2,pch=3,cex=2.5)

N <- nrow(StopD)
set.seed(123)
ind1 <- sample(N,size=floor(nrow(StopD)*0.7))
trd=StopD[ind1,]
ttd=StopD[-ind1,]
plot(trd,col=2,pch=4,cex=2.5)

stem(trd$Speed)
stem(trd$Dist)
summary(trd)

m1 <- lm(Dist~Speed,data=trd)
summary(m1)

e=rstandard(m1)
yhat=fitted.values(m1)
plot(yhat,e,col=2);abline(h=0)  ##e-yhat plot  <<=函數、變異數一致性問題
acf(e,ci=0.99)

library(car)
qqPlot(e)

library(MASS)
boxcox(m1)

m2 <- lm(sqrt(Dist)~Speed,data=trd)
e=rstandard(m2)
yhat=fitted.values(m2)
plot(yhat,e,col=2);abline(h=0)
acf(e,ci=0.99)
qqPlot(e)
boxcox(m2)
summary(m2)
anova(m2) #MSE=0.525

m2p=predict(m2,newdata=ttd,se.fit=F)
MSPR1 <- mean((m2p-sqrt(ttd$Dist))^2)
#測試集Y開根號求MSPR1 => sqrt(ttd$Dist)
#測試樣本誤差的平均值0.504; MSE=0.525。
#MSPR1越接近MSE越好=>不會有過度配適的問題

MSPR=mean((m2p^2-ttd$Dist)^2)
#原單位誤差平方的平均值，MSPR是距離的平方
#所以MSPR要開根號還原預測的平均值 => sqrt(MSPR)
sqrt(MSPR)  #預測的平均值=9.958161

plot(ttd$Speed,m2p^2)
points(ttd$Speed,ttd$Dist,col=2)

dnew <- data.frame(Speed=c(5,10,15,20,25,30,35,40))
p1 <- predict(m2,dnew,interval='confidence')^2  #平均值信賴區間
p2 <- predict(m2,dnew,interval='prediction')^2  #單一值信賴區間
plot(Dist~Speed,data=StopD,col=2,
     main='Stopping distance at different speeds',
     xlab='Speed(miles/hour)',ylab='Dist(feet)')
points(dnew[,1],p1[,1],lwd=3,type='l')
for(k in 1:nrow(dnew)){
  segments(dnew$Speed[k],p1[k,2],dnew$Speed[k],p1[k,3],col=3,lwd=4)
  segments(dnew$Speed[k],p2[k,2],dnew$Speed[k],p2[k,3],col=4)
}

#----------------------------------------------------
#Multicollinearity-------------------
Demodata1=read.table('C:\\Users\\willy\\Desktop\\Reg\\2016-01-07\\01_Multicollinearity\\CH07TA06.DAT',header=T)
attach(Demodata1)
cor(X1,X2)  #=0 沒共線性問題
tmod1=lm(Y~X1+X2)
summary(tmod1)
tmod2=lm(Y~X1)
summary(tmod2) #在其他自變數不變情況下，每增加一單位X1，Y的平均期望會增加5.375單位
tmod3=lm(Y~X2)
summary(tmod3)
detach(Demodata1)

Demodata=read.table('C:\\Users\\willy\\Desktop\\Reg\\2016-01-07\\01_Multicollinearity\\CH07TA01.DAT',header=T)
attach(Demodata)
Demodata[1:10,]
cor(cbind(triceps, thigh, midarm))
pairs(cbind(ybodyfat,triceps, thigh, midarm))  # X&Y、X&X 關係
#共線性殘害了
#triceps & thigh到底丟掉誰?事務法，兩個都掉試試看
#用vif診斷，發現mod1的vif，triceps=708.8429>>10(太大)
mod1=lm(ybodyfat~triceps+thigh+midarm)
summary(mod1)
mod2=lm(ybodyfat~thigh+midarm)  #-triceps; R-squared:  0.7757
summary(mod2)
mod3=lm(ybodyfat~triceps+midarm)  #-thigh; R-squared:  0.7862
summary(mod3)
#結論丟掉thigh，R-squared較高
mod4=lm(ybodyfat~triceps+thigh)
summary(mod4)
mod5=lm(ybodyfat~triceps)
summary(mod5)

library(car)
vif(mod1)  #get the VIF(Variance Inflaction Vector,變異膨脹因子) for X corresponding reg coeff 
vif(mod3)  #vif <10
#給老闆mod3的模型

#Outlier----------------------
library(car)
data(Duncan)
plot(Duncan)
mod.duncan <- lm(prestige~income+education,data=Duncan)
summary(mod.duncan)
residualPlots(mod.duncan,quadratic=F)
qqPlot(mod.duncan)  #符合常態性假設
vif(mod.duncan)
#car裡面才有influenceIndexPlot函數
influenceIndexPlot(mod.duncan,id.n=3)  #id.n=3 數值最大(最嚴重)的"3個"數值標示出來
influencePlot(mod.duncan,id.n=3) #Hat=0.1~0.15(嚴格參考線);  、 0.2(寬鬆參考線)
p=length(mod.duncan$coef)
n=nrow(Duncan)
#limit1 = (2*p)/n
#limit2 = (3*p)/n
dfbetaPlots(mod.duncan,id.n=3) # dfbetaPlots在car裡面有特別被標準化
#minister會讓ncome係數變小，engineer會變大
#dfbetaPlots更詳細的看回歸參數是否有影響。
outlierTest(mod.duncan)  #(是否是Y的離群值)看Bonferonni p = 0.14297(結論不是)

plot(cooks.distance(mod.duncan))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(mod.duncan), row.names(Duncan))
#[1]  6 16  => rowname is 6 and 16

Duncan[rownames(Duncan)=='minister',]

newd <- Duncan[!rownames(Duncan)%in%c('minister'),] # ! => 不是
m1 <- lm(prestige~income+education,data=newd)
influenceIndexPlot(m1,id.n=3)
n=nrow(newd)
plot(cooks.distance(m1))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(m1), row.names(newd))
dfbetaPlots(m1,id.n=3)

compareCoefs(mod.duncan,m1,se=F)  #income       0.599  0.732

newd1 <- newd[!rownames(newd)%in%c('conductor'),]
m2 <- lm(prestige~income+education,data=newd1)
influenceIndexPlot(m2,id.n=3)
n=nrow(newd1)
plot(cooks.distance(m2))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(m2), row.names(newd1))
dfbetaPlots(m2,id.n=3)

compareCoefs(m1,m2,se=F) #income       0.732  0.867

newd2 <- newd1[!rownames(newd1)%in%c('reporter'),]
m3 <- lm(prestige~income+education,data=newd2)
influenceIndexPlot(m3,id.n=3)
n=nrow(newd2)
plot(cooks.distance(m3))
abline(h=4/(n-p), lty=2)
identify(1:n,cooks.distance(m3), row.names(newd2))
dfbetaPlots(m3,id.n=3)

compareCoefs(m2,m3,se=F) #income       0.867  0.877

#m3發現不需要刪掉reporter
#本模型(報告給老闆的是m2)不能使用在minister、conducter這兩類的人身上。
#把變數幹掉他，就要告訴他模型不能用在幹掉變數的狀態。

#VariableSelection------------------------
library(car)
data(Highway1)
str(Highway1)  #看資料結構
attach(Highway1)
hmod1 <- lm(log(rate)~log(len)+log(ADT)+log(trks)+log(sigs1)
            +slim+shld+lane+acpt+itg+poly(lwid,3)+hwy,data=Highway1)
summary(hmod1)# R-squared:  0.8517
residualPlots(hmod1,quadratic=F)
qqPlot(hmod1)
mods1aic <- step(hmod1)
#<none>  1.2076 -103.523 <=不刪除任何變數AIC=-103.52
#一直刪除變數直到出現<none>在第一列，有AIC最小。

#t-test for further adjustment
m1 <- lm(log(rate) ~ log(len) + log(sigs1) + slim + poly(lwid, 3) + hwy)
summary(m1) # R-squared:  0.8343 (刪掉很多變數R-squared才降(0.8517-0.8343)，划算)

m2 <- lm(log(rate) ~ log(len) + log(sigs1) + slim + poly(lwid, 3))  # - hwy
summary(m2) #R-squared:  0.731 (刪掉hwy減少(0.8343-0.731)，賠慘，結論不刪hwy)

m1a <- lm(log(rate) ~ log(len) + log(sigs1) + slim + poly(lwid, 3) + hwy + acpt) # + acpt
summary(m1a) #R-squared:  0.8402(增加一個不顯著變數只有增加不到1%的R-squared(0.8402-0.8343))

m3 <- lm(log(rate) ~ log(len)  + log(sigs1) + slim + hwy)
summary(m3)

m4 <- lm(log(rate) ~ log(len)  + log(sigs1) + slim )
summary(m4)

#m3 is a good choice for stopping

#Model Validation----------------------
GasData=read.table("C:\\Users\\willy\\Desktop\\Reg\\2016-01-07\\04_Model Validation\\gasconsumption.txt",header=T)
NGasData=GasData[,-2]
NGasData$ET <- as.factor(NGasData$ET)
is.factor(NGasData$ET)

set.seed(135)
n=dim(NGasData)[1]
Sindex=sample(n,round(n*0.8))
Train=NGasData[Sindex,]
Test=NGasData[-Sindex,]  

Train=NGasData[1:35,]
Test=NGasData[36:38,]
attach(Train)
pairs(Train)
cor(cbind(MPG,WT,DIS,NC,HP,ACC))
#Due to the limited sample size, we don't use ACC anad HP in this example.
M1=lm(MPG~.,data=Train)
summary(M1)

e=residuals(M1)
es=rstandard(M1)
yhat=fitted.values(M1)

plot(es,type = "l",col='2')
acf(es,ci=0.99)

plot(yhat,es,col='2');abline(h=0)  #e-yhat plot function form &變異數一致性問題。
residualPlots(M1,type="rstandard",quadratic=F)
qqPlot(M1)

library(MASS)
boxcox(M1)  #取log，因為取倒數會呈反向變動。
lnMPG=log(MPG)
M2=lm(lnMPG~.,data=Train)
summary(M2)

e=residuals(M2)
es=rstudent(M2)
yhat=fitted.values(M2)

plot(es,type = "l",col='2')

plot(yhat,es,col='2');abline(h=0) #e-yhat plot
residualPlots(M2,type="rstandard",quadratic=F)
qqPlot(M2)

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

