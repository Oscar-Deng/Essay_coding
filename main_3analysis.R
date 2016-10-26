# ----
TEJ0 <- readDB(DB_fil = "TEJ1996.xlsx", attr_fil = "DB2.xlsx", attr_sht = "TEJ_attr", xls_sht = "TEJ")
TEJ8 <- TEJ0 %>%  DBfilter() %>%
  replaceNAby0(col=c('OERD','OEPRO','Land','LandR','CTP_IFRS_CFI','CTP_IFRS_CFO','CTP_IFRS_CFF','CTP_GAAP','INTAN','RELATIN')) %>% 
  dep_var() %>% STR() %>% 
  select(-c(market,TEJ_name2,TEJ_code2,TEJ_name1,TEJ_code1,CTP_GAAP,CTP_IFRS_CFI,CTP_IFRS_CFO,CTP_IFRS_CFF,STR_RD.lag,STR_EMP.lag,STR_MB.lag,STR_MARKET.lag,STR_PPE.lag)) %>%
  fnHHI() %>% fnGDP(file="DB2.xlsx",col_sht="GDP_colnames",DB_sht="GDP")
# ----
write.csv(na_count(TEJ0),file="原始資料集缺漏值.csv",row.names=TRUE)
yr <- seq(2006,2015);yr0 <- seq(1996,2005)
write.csv(na_count(TEJ0 %>% filter(year(date) %in% yr)),file="原始資料2006.2015闕漏值.csv",row.names = TRUE)
write.csv(na_count(TEJ0 %>% filter(year(date) %in% yr0)),file="原始資料1996.2005闕漏值.csv",row.names = TRUE)
# ----
write.csv(TEJ8,"TEJ8.csv",row.names = T)
#write.csv(TEJ8.1,"TEJ8.1.csv",row.names = T)
writeLines("Wrote 'TEJ8.csv' in folder, and data is set to analyze,please confirm!\n100 % is completed...")

# ----

# TEJ8 = from 2001-2015
TEJ8 <- read.csv("TEJ8.csv",row.names = 1)
DB.orig <- dplyr::select(TEJ8, year,TSE_code,ETR,CETR,STR,HHI,HHI_Dum,ROA,SIZE,LEV,
                         INTANG,QUICK,EQINC,OUTINSTI,RELATIN,RELATOUT,RELAT,FAM_Dum,GDP,
                         RD,EMP,MB,MARKET,PPE)

DB.orig$RELAT2 <- DB.orig$RELATIN + DB.orig$RELATOUT


DB.orig$FAM_Dum <- as.factor(DB.orig$FAM_Dum)
DB$HHI_Dum <- DB$HHI_Dum + 1
#DB.orig$HHI_Dum <- as.factor(DB.orig$HHI_Dum)
str(DB.orig)
#scatterplotMatrix(DB.orig,diagonal='histogram',smoother=F)
# DB.wind = TEJ8.1
DB.orig <- replace(x = DB.orig,is.na(DB.orig),values = 0)

DB.orig.2006 <- DB.orig %>% filter(year %in% seq(2006,2015))
DB.wind <- winsamp(x="DB.orig",col=c('ROA','SIZE','LEV','INTANG','QUICK','EQINC',
                                     'RELATIN','RELATOUT'),prob = 0.01)
DB.wind$ETR[DB.wind$ETR <= 0] <- 0; DB.wind$CETR[DB.wind$CETR <=0] <- 0
DB.wind$ETR[DB.wind$ETR >= 1] <- 1; DB.wind$CETR[DB.wind$CETR >=1] <- 1
DB.wind.2006 <- DB.wind %>% filter(year %in% seq(2006,2015))
DB.wind.2006 <- winsamp(x="DB.wind.2006",col=c('ROA','SIZE','LEV','INTANG','QUICK','EQINC','RELATIN','RELATOUT'),prob = 0.01)

# 分析STR變數優良性
DB <- DB.wind.2006[,c(3,4,5,7,8,9,10,11,13,14,19,25)]
DB$HHI_Dum <- as.factor(DB$HHI_Dum)
library(MASS)
lm <- lm(ETR ~ STR+HHI_Dum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT2+FAM_Dum+GDP,DB.wind.2006)
summary(lm)
lm2 <- stepAIC(lm(ETR ~ STR+HHI_Dum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT2+FAM_Dum+GDP,DB.wind.2006),direction = "both",k=log(nrow(DB)),steps = 1000)
summary(lm2)
DB$ETR = DB$ETR+1
lm3 <- lm(ETR ~ STR+HHI_Dum+ROA+SIZE+LEV+EQINC+OUTINSTI+RELAT2+GDP, DB)
summary(lm3)
plot(lm3,which=1)


bc <- boxcox(lm3)
bc$x[which.max(bc$y)]
# -2 ... so make ETR^-2

lm3.1 <- lm((ETR^-2) ~ STR+HHI_Dum+ROA+SIZE+LEV+EQINC+OUTINSTI+RELAT2+GDP, DB)
summary(lm3.1)
plot(lm3.1,which=1)
library(lmtest)
dwtest(lm3.1)
durbinWatsonTest(lm3.1)
#acf(lm3.1)


vif(lm3.1)

qqPlot(lm3.1,labels="lm3.1",id.method="identify"
       ,simulate=TRUE,main="Q-Q Plot")



lm3.2 <- lm((ETR^-2) ~ log(STR)+HHI_Dum+log(ROA+1)+log(SIZE)+log(LEV)+log(EQINC+1)+log(OUTINSTI+1)+log(RELAT2+1)+GDP, DB)
summary(lm3.2)
plot(lm3.2,which=1)
#lm3.2.0 <- stepAIC(lm((ETR^-2) ~ poly(log(STR),3)+HHI_Dum+poly(log(ROA+1),3)+poly(log(SIZE),3)+poly(log(LEV),3)+poly(log(EQINC+1),3)+poly(log(OUTINSTI+1),3)+poly(log(RELAT2+1),3)+poly(GDP,3), DB),direction = "backward",steps = 10000,k = log(nrow(DB)))
lm3.2.0 <- lm((ETR^-2) ~ log(STR)+HHI_Dum+log(SIZE)+log(LEV)+log(ROA+1)+log(EQINC+1)+log(OUTINSTI+1)+log(RELAT2+1)+GDP, DB)
lm3.2.1 <- lm((ETR^-2) ~ log(STR)^3+HHI_Dum+log(STR)^3:HHI_Dum+log(SIZE)^2+log(LEV)^2+log(ROA+1)^3*log(EQINC+1)*log(OUTINSTI+1)+log(RELAT2+1)+GDP, DB)
lm3.2.1.AIC <-  stepAIC(lm3.2.1,steps = 1000,k=log(nrow(DB)),direction = "both")
lm3.2.2 <- lm((ETR^-2) ~ HHI_Dum+log(STR)^2:HHI_Dum+log(SIZE)^2+log(LEV)^2+log(ROA+1)^2+log(EQINC+1)+log(OUTINSTI+1)+log(RELAT2+1)+GDP, DB)
summary(lm3.2.1.AIC)
summary(lm3.2.2)
#lm3.2.3 <- lm((ETR^-2) ~ log(STR)^2:HHI_Dum+log(ROA+1)^2*log(EQINC+1)+log(OUTINSTI+1)+log(RELAT2+1)+GDP, DB)
#summary(lm3.2.3)

lmA <- lm((ETR^-2) ~ HHI_Dum+log(STR)^2:HHI_Dum+log(SIZE)^2+log(LEV)^2+
            log(ROA+1)^2+log(EQINC+1)+log(OUTINSTI+1)+log(RELAT2+1)+GDP, DB)
lmB <- lm((ETR^-2) ~ HHI_Dum+log(STR)^2:HHI_Dum+log(SIZE)^2+log(LEV)^2+
            log(ROA+1)^2+log(EQINC+1)+log(OUTINSTI+1)+log(RELAT2+1), DB)
anova(lmA,lmB)

xvar <- as.matrix(DB[,c(3,4,5,6,7,9,10,11,12)])
yvar <- DB$ETR
regsubsets(xvar,yvar,method="forward",nbest=1)

plot(lm3.2.2,which=1)
plot(lmB,which=1)
summary(lm(ETR^-2 ~ STR^2*HHI_Dum*SIZE^2 ,DB))
bartlett.test(lm3.2.2,DB)
#gdp-1
#relat-1
#outi-1
#eqinc-12
#lev-123
#size-123
#roa-123



newDB.wind.2006 <- as.data.frame(apply(dplyr::select(DB.wind.2006, ETR,CETR,STR,HHI,HHI_Dum,ROA,SIZE,LEV,
                         INTANG,QUICK,EQINC,OUTINSTI,RELATIN,RELATOUT,RELAT,FAM_Dum,GDP)
                   ,MARGIN = 2, FUN = doNormalization))

newDB.wind.lmE <- lm(ETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP
                  ,newDB.wind.2006)

# scatter plot: ETR to each dependent variables
# ----
png("各變數與ETR之散佈圖.png",width = 1200,height=1000)
par(mfrow=c(3,4))
plot(DB$ETR,DB$STR)
plot(DB$ETR,DB$HHI_Dum)
plot(DB$ETR,DB$ROA)
plot(DB$ETR,DB$SIZE)
plot(DB$ETR,DB$LEV)
plot(DB$ETR,DB$INTANG)
plot(DB$ETR,DB$QUICK)
plot(DB$ETR,DB$EQINC)
plot(DB$ETR,DB$OUTINSTI)
plot(DB$ETR,DB$RELAT)
plot(DB$ETR,DB$FAM_Dum)
plot(DB$ETR,DB$GDP)
dev.off()
# ----



DB.wind.lmC <- lm(CETR ~ STR+HHI_Dum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP
              ,DB.wind)

DB.wind.lmE.SH <- lm(ETR ~ STR+HHI_Dum+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,DB.wind)
DB.wind.lmC.SH <- lm(CETR ~ STR+HHI_Dum+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,DB.wind)

#TEJ8.1$STR_HHIsum <- TEJ8.1$STR * TEJ8.1$HHIsum # this is not a good idea!
DB.wind.2006.lmE <- lm(ETR ~ STR+HHI_Dum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP
                ,DB.wind.2006)
DB.wind.2006.lmC <- lm(CETR ~ STR+HHI_Dum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP
                 ,DB.wind.2006)
DB.wind.2006.lmE.SH <- lm(ETR ~ STR+HHI_Dum+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP
                   ,DB.wind.2006)
DB.wind.2006.lmC.SH <- lm(CETR ~ STR+HHI_Dum+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP
                    ,DB.wind.2006)

TEJ8.2 <- TEJ8.1 %>% mutate(STR.PR = STR_RD.perank+STR_MB.perank+STR_EMP.perank+STR_PPE.perank+STR_MARKET.perank) %>% mutate(STR.PR_HHI = STR.PR*HHI_Dum)
lm.ETR2 <- lm(ETR ~ STR.PR+HHI_Dum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.2)
lm.CETR2 <- lm(CETR ~ STR.PR+HHI_Dum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.2)
lm.ETR.SH2 <- lm(ETR ~ STR.PR+HHI_Dum+STR.PR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.2)
lm.CETR.SH2 <- lm(CETR ~ STR.PR+HHI_Dum+STR.PR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.2)

# ----
DB.orig.Ecor <- corstars(DB.orig %>% select(ETR,STR,HHI_Dum,ROA,SIZE,LEV,
                                         INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP)
                       ,method = "pearson",removeTriangle = "lower",star = 3,result = "none")
DB.orig.Ccor <- corstars(DB.orig %>% select(CETR,STR,HHI_Dum,ROA,SIZE,LEV,
                                          INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP)
                        ,method = "pearson",removeTriangle = "lower",star = 3,result = "none")
DB.wind.Ecor <- corstars(DB.wind %>% select(ETR,STR,HHIsum,ROA,SIZE,LEV,
                                             INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP)
                           ,method = "pearson",removeTriangle = "lower",star = 3,result = "none")
DB.wind.Ccor <- corstars(DB.wind %>% select(CETR,STR,HHIsum,ROA,SIZE,LEV,
                                              INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP)
                            ,method = "pearson",removeTriangle = "lower",star = 3,result = "none")

# ----
# 檢驗LM等分散性:
require(car)
ncvTest(newDB.wind.lmE)
#Non-constant Variance Score Test 
#Variance formula: ~ fitted.values 
#Chisquare = 104.776    Df = 1     p = 1.36754e-24 
# 卡方統計量、自由度、顯著性...
# p < 0.000, 拒絕虛無假設(B0=B1=B2...=0)
# 是否符合變異數等分散性?

# 檢驗殘差獨立性
durbinWatsonTest(newDB.wind.lmE)
#lag Autocorrelation D-W Statistic p-value
#1     0.001851732      1.996287   0.808
#Alternative hypothesis: rho != 0
# 殘差自我相關值、D-W統計量、顯著性
# 不顯著、殘差自我相關係數趨近0

# 常態性
# shapiro.test(residuals(DB.wind.lmE)) # 超過5000筆，不能用...用下面的圖
# residualPlot(DB.wind.lmE) = following line
plot(DB.wind.lmE,which=1)

# 校正模型
DB.wind.lmE.2 <- lm(ETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,newDB.orig)

plot(DB.wind.lmE.2,which=1)

DB.wind.lmE.SH.2 <- lm(ETR ~ STR+HHI_Dum+STR:HHI_Dum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP
                     ,DB.wind)

# check vif 
#install.packages("car")
require(car)
vif(lm.ETR,digits=3)
vif(lm.CETR,digits=3)
vif(lm.ETR.SH,digits=3)
vif(lm.CETR.SH,digits=3)





# ----
TEJ8.3 <- merge(
  merge(
    x=TEJ8.1 %>% group_by(TSE_code,year) %>% summarise(NetSales.Deno = sum(as.numeric(NetSales),na.rm=TRUE)),
    y=TEJ8.1 %>% group_by(TSE_code,year) %>% top_n(n=4,wt = NetSales) %>% summarise(NetSales.nume = sum(as.numeric(NetSales),na.rm=TRUE)),
    by=c("TSE_code","year")) %>%
    mutate(CR4 = as.numeric(NetSales.nume / NetSales.Deno))
  ,y=TEJ8.1
  ,by=c("TSE_code","year")
) %>% mutate(STR_CR4 = STR * CR4)

lm.ETR3 <- lm(ETR ~ STR+CR4+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.3)
lm.CETR3 <- lm(CETR ~ STR+CR4+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.3)
lm.ETR.SH3 <- lm(ETR ~ STR+CR4+STR_CR4+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.3)
lm.CETR.SH3 <- lm(CETR ~ STR+CR4+STR_CR4+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.3)

# ----




write(stargazer(
  TEJ8.1 %>% select(ETR,CETR,STR,HHI_Dum,STR_HHI,ROA,SIZE,LEV,
                    INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP,
                    #,RD,EMP,MB,MARKET,PPE
                    STR_RD,STR_EMP,STR_MB,STR_MARKET,STR_PPE
  )
  ,type = "html"
  ,summary.stat = c("n","min","median","max","sd","mean")
  ,align = TRUE
  ,notes.append = TRUE,label = "D1"
),"敘述統計表.html")
writeLines("Finish '敘述統計表.html' and print in folder.")

write.csv(ETR.cortab,"ETR.Pearson相關分析.csv")
write.csv(CETR.cortab,"CETR.Pearson相關分析.csv")
writeLines("Finish 'Pearson相關分析.csv' and print in folder.")
plottbA5 <- function(x=TEJ8.1){
  x$TSE <- paste(x$TSE_code,x$TSE_name,sep="")
  HHI_DB <- base::subset(x, select=c(TSE,year,HHI)) %>% distinct
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI >= 0.3,'高寡佔I 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.3 & HHI_DB$HHI >= 0.18,'高寡佔II 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.18 & HHI_DB$HHI >= 0.14,'低寡占I 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.14 & HHI_DB$HHI >= 0.1,'低寡占II 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.1 & HHI_DB$HHI >= 0.05,'競爭I 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.05,'競爭II 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI == "NaN","")
  HHI_tbl <- dcast(HHI_DB,TSE ~ year) %>% as.data.frame
  HHI_tbl <- as.data.frame(HHI_tbl[,-1],row.names=HHI_tbl[,1])
  return(HHI_tbl)
}
HHI_tbl <- plottbA5()
write.csv(HHI_tbl,"產業年度架構表.csv")
writeLines("Finish '產業年度架構表.csv' and print in folder.")

writeLines("Finish Constructing Linear Model, part1.")
# Emp 1
write(stargazer(lm.ETR,lm.CETR,type="html",
          dep.var.labels = c("$TAXAVO_{it}=ETR_{it}$","$TAXAVO_{it}=CashETR_{it}$"),
          digits=3,label = "E1",align = TRUE),"實證一不含STR_HHI.html")

write(stargazer(lm.ETR.SH,lm.CETR.SH,type="html",
          dep.var.labels = c("$TAXAVO_{it}=ETR_{it}$","$TAXAVO_{it}=CashETR_{it}$"),
          digits=3,label = "E2",align = TRUE),"實證一含STR_HHI.html")

writeLines("Output 'Empirical 1(.html)' result in folder, please confirm.")
# ----
write(stargazer(lm.ETR.n1,lm.CETR.n1,type="html",
                dep.var.labels = c("$TAXAVO_{it}=ETR_{it}$","$TAXAVO_{it}=CashETR_{it}$"),
                digits=3,label = "E1",align = TRUE),"實證二(原始HHI)不含STR_HHIsum.html")

write(stargazer(lm.ETR.SH.n1,lm.CETR.SH.n1,type="html",
                dep.var.labels = c("$TAXAVO_{it}=ETR_{it}$","$TAXAVO_{it}=CashETR_{it}$"),
                digits=3,label = "E2",align = TRUE),"實證二(原始HHI)含STR_HHIsum.html")

write.csv(ETR.origHHIsum,"ETR.Pearson.原始HHI.csv")
write.csv(CETR.origHHIsum,"CETR.Pearson.原始HHI.csv")



# ----
writeLines("Finish Constructing Linear Model, part2.")

write(stargazer(lm.ETR2,lm.CETR2,
          dep.var.labels = c("$TAXAVO_{it}=ETR_{it}$","$TAXAVO_{it}=CashETR_{it}$"),
          digits=3,label = "F1",align = TRUE),"敏感分析一不含STR_HHI.html")
write(stargazer(lm.ETR.SH2,lm.CETR.SH2,
          dep.var.labels = c("$TAXAVO_{it}=ETR_{it}$","$TAXAVO_{it}=CashETR_{it}$"),
          digits=3,label = "F2",align = TRUE),"敏感分析一含STR_HHI.html")

writeLines("Output 'Empirical 2(.html)' result in folder, please confirm.")
# ----

writeLines("Finish Constructing Linear Model, part3.")

write(stargazer(lm.ETR3,lm.CETR3,
          dep.var.labels = c("$TAXAVO_{it}=ETR_{it}$","$TAXAVO_{it}=CashETR_{it}$"),
          digits=3,label = "F4",align = TRUE),"敏感分析二不含STR_HHI.html")

write(stargazer(lm.ETR.SH3,lm.CETR.SH3,
          dep.var.labels = c("$TAXAVO_{it}=ETR_{it}$","$TAXAVO_{it}=CashETR_{it}$"),
          digits=3,label = "F5",align = TRUE),"敏感分析二含STR_HHI.html")

writeLines("Output 'Empirical 3(.html)' result in folder, please confirm.")

# ----

tab.ETR <- summary(lm.ETR)$coefficients
tab.CETR <- summary(lm.CETR)$coefficients
tab.ETR2 <- summary(lm.ETR2)$coefficients
tab.CETR2 <- summary(lm.CETR2)$coefficients
tab.ETR3 <- summary(lm.ETR3)$coefficients
tab.CETR3 <- summary(lm.CETR3)$coefficients

tab.ETR.SH <- summary(lm.ETR.SH)$coefficients
tab.CETR.SH <- summary(lm.CETR.SH)$coefficients
tab.ETR.SH2 <- summary(lm.ETR.SH2)$coefficients
tab.CETR.SH2 <- summary(lm.CETR.SH2)$coefficients
tab.ETR.SH3 <- summary(lm.ETR.SH3)$coefficients
tab.CETR.SH3 <- summary(lm.CETR.SH3)$coefficients


colnames(tab.ETR) <- paste("ETR",colnames(tab.ETR),sep="_")
colnames(tab.CETR) <- paste("CETR",colnames(tab.CETR),sep="_")
colnames(tab.ETR.SH) <- paste("ETR.SH",colnames(tab.ETR.SH),sep="_")
colnames(tab.CETR.SH) <- paste("CETR.SH",colnames(tab.CETR.SH),sep="_")

colnames(tab.ETR2) <- paste("ETR.PR",colnames(tab.ETR2),sep="_")
colnames(tab.CETR2) <- paste("CTR.PR",colnames(tab.CETR2),sep="_")
colnames(tab.ETR.SH2) <- paste("ETR.PR.SH",colnames(tab.ETR.SH2),sep="_")
colnames(tab.CETR.SH2) <- paste("CETR.PR.SH",colnames(tab.CETR.SH2),sep="_")

colnames(tab.ETR3) <- paste("ETR.CR4",colnames(tab.ETR3),sep="_")
colnames(tab.CETR3) <- paste("CETR.CR4",colnames(tab.CETR3),sep="_")
colnames(tab.ETR.SH3) <- paste("ETR.CR4.SH",colnames(tab.ETR.SH3),sep="_")
colnames(tab.CETR.SH3) <- paste("CETR.CR4.SH",colnames(tab.CETR.SH3),sep="_")

#write(stargazer(cbind(tab.ETR,tab.CETR),type="html"),"tab.lm.html")
write.csv(cbind(tab.ETR,tab.CETR),"lm1.csv")

setwd("..")



