
if("TEJ8.1" %in% ls()){print("You've already got TEJ8.1, great!")}else{
  if("TEJ8.1.csv" %in% dir()){
    TEJ8.1 <- read.csv("TEJ8.1.csv",row.names = 1)}else{
      stop("Sorry, you don't have TEJ8.1, please do 'setSweave.R' first.")
    }
  }
require(stargazer)
require(xtable)
require(dplyr)
require(data.table)
require(ggplot2)
require(stats)
source("corstars.R")  
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
ETR.cortab <- corstars(TEJ8.1 %>% select(ETR,STR,HHI_Dum,ROA,SIZE,LEV,
                                         INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP)
                       ,method = "pearson",removeTriangle = "lower",star = 3,result = "none")
CETR.cortab <- corstars(TEJ8.1 %>% select(CETR,STR,HHI_Dum,ROA,SIZE,LEV,
                                          INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP)
                        ,method = "pearson",removeTriangle = "lower",star = 3,result = "none")
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
lm.ETR <- lm(ETR ~ STR+HHI_Dum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP
             ,TEJ8.1)
lm.CETR <- lm(CETR ~ STR+HHI_Dum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP
              ,TEJ8.1)
lm.ETR.SH <- lm(ETR ~ STR+HHI_Dum+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.1)
lm.CETR.SH <- lm(CETR ~ STR+HHI_Dum+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.1)

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
TEJ8.1$STR_HHIsum <- TEJ8.1$STR * TEJ8.1$HHIsum
lm.ETR.n1 <- lm(ETR ~ STR+HHIsum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP
             ,TEJ8.1)
lm.CETR.n1 <- lm(CETR ~ STR+HHIsum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP
              ,TEJ8.1)
lm.ETR.SH.n1 <- lm(ETR ~ STR+HHIsum+STR_HHIsum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.1)
lm.CETR.SH.n1 <- lm(CETR ~ STR+HHIsum+STR_HHIsum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.1)
write(stargazer(lm.ETR.n1,lm.CETR.n1,type="html",
                dep.var.labels = c("$TAXAVO_{it}=ETR_{it}$","$TAXAVO_{it}=CashETR_{it}$"),
                digits=3,label = "E1",align = TRUE),"實證二(原始HHI)不含STR_HHIsum.html")

write(stargazer(lm.ETR.SH.n1,lm.CETR.SH.n1,type="html",
                dep.var.labels = c("$TAXAVO_{it}=ETR_{it}$","$TAXAVO_{it}=CashETR_{it}$"),
                digits=3,label = "E2",align = TRUE),"實證二(原始HHI)含STR_HHIsum.html")
ETR.origHHIsum <- corstars(TEJ8.1 %>% select(ETR,STR,HHIsum,ROA,SIZE,LEV,
                                        INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP)
                      ,method = "pearson",removeTriangle = "lower",star = 3,result = "none")
CETR.origHHIsum <- corstars(TEJ8.1 %>% select(CETR,STR,HHIsum,ROA,SIZE,LEV,
                                          INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP)
                        ,method = "pearson",removeTriangle = "lower",star = 3,result = "none")
write.csv(ETR.origHHI,"ETR.Pearson.原始HHI.csv")
write.csv(CETR.origHHI,"CETR.Pearson.原始HHI.csv")



# ----
TEJ8.2 <- TEJ8.1 %>% mutate(STR.PR = STR_RD.perank+STR_MB.perank+STR_EMP.perank+STR_PPE.perank+STR_MARKET.perank) %>% mutate(STR.PR_HHI = STR.PR*HHI_Dum)
lm.ETR2 <- lm(ETR ~ STR.PR+HHI_Dum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.2)
lm.CETR2 <- lm(CETR ~ STR.PR+HHI_Dum+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.2)
lm.ETR.SH2 <- lm(ETR ~ STR.PR+HHI_Dum+STR.PR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.2)
lm.CETR.SH2 <- lm(CETR ~ STR.PR+HHI_Dum+STR.PR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ8.2)
writeLines("Finish Constructing Linear Model, part2.")

write(stargazer(lm.ETR2,lm.CETR2,
          dep.var.labels = c("$TAXAVO_{it}=ETR_{it}$","$TAXAVO_{it}=CashETR_{it}$"),
          digits=3,label = "F1",align = TRUE),"敏感分析一不含STR_HHI.html")
write(stargazer(lm.ETR.SH2,lm.CETR.SH2,
          dep.var.labels = c("$TAXAVO_{it}=ETR_{it}$","$TAXAVO_{it}=CashETR_{it}$"),
          digits=3,label = "F2",align = TRUE),"敏感分析一含STR_HHI.html")

writeLines("Output 'Empirical 2(.html)' result in folder, please confirm.")
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





