#### PART I Artifial data demo ###########

# This time we'll generate the necessary fake data
# within R itself, rather than working in Python and 
# then reading the data into R.  This will demonstrate
# some handy R commands that you may not have known about.


# Let's start with a problem where the predictor variable X
# does not seem to have a linear relationship with Y and 
# needs to be transformed 

rawx = runif(200,min=0,max=10);hist(rawx)
X = exp(rawx)
ydev = rnorm(200,0,1)
Y = rawx + ydev

plot (X,Y,col="red",pch=16)
title(main="A plot of Y on X")
#Yi = beta0 + beta1*e^Xi =epsoni

# Now try transforming the X variable using the natural 
# log function

logX = log(X)

plot (logX,Y,col="red",pch=16)
title(main="A plot of Y on log(X)")

# Note the correlations between Y and each version of X
cor(X,Y)
cor(logX,Y)


m1 <- lm(Y~X)
summary(m1)
plot (X,Y,col="red",pch=16)
title(main="A plot of Y on X")
abline(reg=m1,col='blue')
plot(m1,which=1)

m2 <- lm(log(Y+15)~X)
plot(X,Y+15,col="red",pch=16)


# Another problem where the predictor variable X
# does not seem to have a linear relationship with Y and 
# needs to be transformed (x^2 this time)

rawx = runif(200,min=0,max=80)
rawx = sort(rawx)
X = sqrt(rawx)
ydev = rnorm(200,0,2)
Y = rawx + ydev
?sort

plot (X,Y,col="red",pch=16)
title(main="A plot of Y on X")

expX = exp(X)
X2 = X * X

plot (expX,Y,col="red",pch=16)
title(main="A plot of Y on exp(X)")

plot (X2,Y,col="red",pch=16)
title(main="A plot of Y on X-squared")


# Fit linear models to the raw X and the two transformations
unchanged = lm (Y ~ X)
expTrans = lm ( Y ~ expX )
squareTrans = lm ( Y ~ X2 )

# Use AIC to choose between the three models
AIC (unchanged,expTrans,squareTrans)

# Plot the predictions from the three models all on one plot

plot (X,Y,col="red",pch=16)
lines(X,predict(unchanged),col="blue",lw=2)
lines(X,predict(expTrans),col="green",lw=2)
lines(X,predict(squareTrans),col="black",lw=2)
title(main="Various models for Y")


# Now a problem where there's a polynomial relationship between 
# X and Y.

X = runif(200,min=-10,max=10)
X = sort(X)
ydev = rnorm(200,0,4)
Y = 62.3 + 0.4 * X - 0.2 * X^2 + 0.03 * X^3 + ydev
?sort

plot (X,Y,col="red",pch=16)
title(main="A plot of Y on X")

# We fit the null, the linear, and some polynomial models of
# increasing degree.  In fact the right answer is the 3rd order
# polynomial model (look how we created the data above) and
# the AIC method identifies this model.
m0 = lm ( Y ~ 1 )
m1 = lm ( Y ~ X )
m2 = lm ( Y ~ X + I(X^2) )  #I() 變成一般轉換+-*/
m3 = lm ( Y ~ X + I(X^2) + I(X^3) )
m4 = lm ( Y ~ X + I(X^2) + I(X^3) + I(X^4) )
#mm <- lm(Y~X1+X2)
?formula
#In addition to + and :, a number of other operators are useful in model formulae. The * operator denotes factor crossing: a*b interpreted as a+b+a:b

AIC(m0,m1,m2,m3,m4)


# Produce a plot of the different polynomial models 
# fitted to the data

plot (X,Y,col="red",pch=16)
lines(X,predict(m0),col="grey",lw=2)
lines(X,predict(m1),col="green",lw=2)
lines(X,predict(m2),col="yellow",lw=2)
lines(X,predict(m3),col="blue",lw=2)
lines(X,predict(m4),col="black",lw=2)
title(main="Various polynomial models for Y")


m3a <- lm(Y~poly(X,3,raw=T)) #raw=T 用X原本的值去形成X^1、X^2、X^3
summary(m3a)
m3a1 <- lm(Y~poly(X,3)) #正規化X多項函數
summary(m3a1)
m4a <- lm(Y~poly(X,4))
summary(m4a)  #4次項不顯著


plot (X,Y,col="red",pch=16)
lines(X,predict(m3a),col="blue",lw=2)
lines(X,predict(m3a1),col="green",lw=2)

# Now investigate a situation where there is a significant
# interaction term

## First no interaction case ##
X = runif(200,min=0,max=20)
X = sort(X)

GroupChoice = runif(200)
Group = ifelse(GroupChoice < 0.5, "A", "B")
Y = ifelse(GroupChoice < 0.5, 36 + X*4.2, 100 + X*4.2)  #X*4.2 + X*4.2
colour = ifelse(GroupChoice < 0.5, "red", "blue")
ydev = rnorm(200,0,12)
Y = Y + ydev

plot (X,Y,col="red",pch=16)
title(main="A plot of Y on X")

plot (X,Y,col=colour,pch=16)
title(main="A plot of Y on X")

Group = as.factor(Group)
fullModel = lm ( Y ~ X*Group)  #X*Group 產生交互作用模型
summary(fullModel)  #X:GroupB 交互作用項
drop1(fullModel)

fullModel1 = lm ( Y ~ X+Group)  #X+Group 各一項(沒交互作用)
summary(fullModel1)
drop1(fullModel1)

plot (X,Y,col=colour,pch=16)
points(X,predict(fullModel),col="green",pch=15)
title(main="A plot of Y on X, without fitted interaction model")




## Second interaction presents ##
X = runif(200,min=0,max=20)
X = sort(X)

GroupChoice = runif(200)
Group = ifelse(GroupChoice < 0.5, "A", "B")
Y = ifelse(GroupChoice < 0.5, 36 + X*4.2, 100 - X*2)  #X*4.2, X*2
colour = ifelse(GroupChoice < 0.5, "red", "blue")
ydev = rnorm(200,0,12)
Y = Y + ydev


# Plot the relationship between X and Y

plot (X,Y,col="red",pch=16)
title(main="A plot of Y on X")


# Plot the relationship between X and Y, highlighting 
# the effects of Group

plot (X,Y,col=colour,pch=16)
title(main="A plot of Y on X")




# Now fit a regression model of Y on X and Group
# Use the drop1 command to see if the interaction term can go
Group = as.factor(Group)
fullModel = lm ( Y ~ X*Group)
fullModel = lm ( Y ~ X+Group+X:Group)  #X*Group = X+Group+X:Group
summary(fullModel)
drop1(fullModel)


# Plot the relationship between X and Y, highlighting 
# the effects of Group, and showing the fitted model

plot (X,Y,col=colour,pch=16)
points(X,predict(fullModel),col="green",pch=15)
title(main="A plot of Y on X, with fitted interaction model")

#### PART I Real data demo ###########
install.packages('visreg')
library(visreg)
data(airquality)
pairs(Ozone~.,data=airquality)
m1 <- lm(Ozone~.,data=airquality)
install.packages('car')
library(car)
residualPlots(m1,quadractic=F)
library(MASS)
boxcox(m1)

m2 <- lm(log(Ozone)~.,data=airquality)
residualPlots(m2,quadractic=F)

m2a <- lm(log(Ozone)~Solar.R+
          poly(Wind,2)+ 
          Temp+Month+Day,data=airquality)
summary(m2)
summary(m2a)
residualPlots(m2a,quadratic=F)

m2b <- lm(log(Ozone)~Solar.R+poly(Wind,2)+
          Temp +Solar.R:Temp+
          Month+Day,data=airquality)
summary(m2b)  #Solar.R:Temp = 0.86991   


# Transfrom of x and y
fit1 <- lm(Ozone ~ Solar.R + Wind + I(Wind^2) + Temp, data = airquality)
fit2 <- lm(log(Ozone) ~ Solar.R + Wind + Temp, data = airquality)
fit3 <- lm(log(Ozone) ~ Solar.R + Wind + I(Wind^2) + Temp, data = airquality)
visreg(fit1, "Wind")
visreg(fit2, "Wind")# visreg(fit2, "Wind", trans = exp, ylab = "Ozone")
visreg(fit3, "Wind")# visreg(fit3, "Wind", trans = exp, ylab = "Ozone")



# Interaction effect #

#Cross-sectional plots
airquality$Heat <- cut(airquality$Temp, 3, labels=c("Cool", "Mild", "Hot"))

fit <- lm(Ozone ~ Solar.R + Wind * Heat, data = airquality)
visreg(fit, "Wind", by = "Heat")
visreg(fit, "Wind", by="Heat", overlay=TRUE, partial=FALSE)

#Surface plots
fit1 <- lm(Ozone ~ Solar.R + Wind + Temp, data = airquality)
visreg2d(fit1, "Wind", "Temp", plot.type = "image")
visreg2d(fit1, "Wind", "Temp", plot.type = "persp")

fit2 <- lm(Ozone ~ Solar.R + poly(Wind,2) + Temp, data = airquality)
visreg2d(fit2, "Wind", "Temp", plot.type = "image")
visreg2d(fit2, "Wind", "Temp", plot.type = "persp")

fit3 <- lm(Ozone ~ Solar.R + Wind*Temp, data = airquality)
visreg2d(fit3, "Wind", "Temp", plot.type = "image")
visreg2d(fit3, "Wind", "Temp", plot.type = "persp")

