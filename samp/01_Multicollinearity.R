#This is a demo for the Applied Linear Model in Section 7.6
#Note that help for the attach package can be obtained by library(help="car") as using package car as an example.

#Loading possible used packages
install.packages('car')
library(car)
library(lmtest)


#Reading the data as a data frame in R
#When X variables are not correlated

Demodata1=read.table('C:\\Users\\user\\Desktop\\CH07TA06.DAT',header=T)
attach(Demodata1)
cor(X1,X2)  #=0 沒共線性問題
tmod1=lm(Y~X1+X2)
summary(tmod1)
tmod2=lm(Y~X1)
summary(tmod2) #在其他自變數不變情況下，每增加一單位X1，Y的平均期望ˊ會增加5.375單位
tmod3=lm(Y~X2)
summary(tmod3)

#When X variables are correlated
detach(Demodata1)
Demodata=read.table('C:\\Users\\user\\Desktop\\CH07TA01.DAT',header=T)
#fix(Demodata2) #the fix function can be used to change the name of the column, but here in this example, I change the column name from the original file.
#The name of the variables are changed V1->alpha and V2->plut
attach(Demodata) 
# 'attach' allow us to access its columns by name from the data frame 'demodata2'
Demodata[1:10,]

#Explore the correlation between predictor variables

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


vif(mod1)  #get the VIF(Variance Inflaction Vector,變異膨脹因子) for X corresponding reg coeff 
vif(mod3)  #vif <10
#給老闆mod3的模型
library(MASS)
#ridgemod=lm.ridge(ybodyfat~.,data=Demodata, lambda=seq(0,0.1,0.0001))
#ridgemod=lm.ridge(ybodyfat~.,data=Demodata)
scaledata=data.frame(scale(Demodata[,c('triceps','thigh','midarm','ybodyfat')]))
attach(scaledata)
plot(lm.ridge(ybodyfat~., data=scaledata, lambda=seq(0,0.5,0.01)))
select(lm.ridge(ybodyfat~., data=scaledata, lambda=seq(0,0.2,0.01)))
plot(lm.ridge(ybodyfat~., data=scaledata, lambda=seq(0,0.05,0.01)))
ridgemod1=lm.ridge(ybodyfat~.-1,data=scaledata,lambda=0.0085)
ridgemod1
ridgemod2=lm.ridge(ybodyfat~.-1,data=scaledata,lambda=0.31)
ridgemod2
ridgemod3=lm.ridge(ybodyfat~.-1,data=scaledata,lambda=0.02)
ridgemod3


