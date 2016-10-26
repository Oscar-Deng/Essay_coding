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

mods1bic <- step(hmod1,k=log(nrow(Highway1)))
modb1 <- step(hmod1, direction = "backward")
modf1 <- step(lm(log(rate)~1),~log(len)+log(ADT)+log(trks)+log(sigs1)+slim+shld+lane+acpt+itg+lwid+hwy, direction = "forward")
#trace=F for no middle models be printed

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



library(leaps)
subx=regsubsets(log(rate)~log(len)+log(ADT)+log(trks)+log(sigs1)+slim+shld+lane+acpt+itg+lwid+hwy, nbest=5, data=Highway1)
subsets(subx,statistic="bic")
subsets(subx,statistic="bic",min.size=2, max.size=6)

subsets(subx,statistic="adjr2")
subsets(subx,statistic="adjr2",min.size=4, max.size=8)

subsets(subx,statistic="rsq")
subsets(subx,statistic="rsq",min.size=4, max.size=8)



#subsets(subx,statistic="cp")
#abline(a=0,b=1)
#subsets(subx,statistic="cp",min.size=4, max.size=7)
#abline(a=0,b=1)


