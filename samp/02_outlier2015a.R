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













