# -*- encoding = utf-8 -*-
# Head
# ----------
# install packages if first use.
install.packages("readxl")
install.packages("data.table")
install.packages("dplyr")

# require packages
rm(list=ls())

# set directory to path
setwd("D:\\Documents\\Dropbox\\MyEssay\\Rcoding\\")


require(readxl)
require(data.table)
require(dplyr)

# Read in Basic from DB_essay <in xlsm (with macro)>, setting list name "Basic"
# 364variables in rawTEJ at first, and observations have NAs.
# read database attributes from DBcol.csv, which included colnames and column attr.
# colattr <- read.csv("DBcol.csv", header=TRUE, sep=",", quote="\"", dec=".",fill=TRUE)
colattribute <- read_excel(path ="DB.xlsx", sheet = "colname_list", col_names = TRUE)
rawTEJ <- read_excel(path = "DB.xlsx", sheet = "DBnew-recol", col_names = TRUE, col_types = as.character(colattribute$type))

# rename columns
setnames(rawTEJ, old=as.character(colattribute$old), new=as.character(colattribute$new))

## ----

### command line for vars ###

# write code into indVar.R -- dependent variable
# ----------
# ???��??I?ұo?|???ة?2013?~?_?A??IFRS???��??T?Ӭ??ءA?G???ۦ??[?`?A?μW?R?ݭn?????��C
# ?g?a?????W?ȥ??P?C

sink("depVar.R") 
# P.s names with "_" is temp data, without "_" is variable. EX: ETR2015 -> var, ETR_2015 -> temp
# BTE - sum past five year data
capture.output(for(t in 2015:2001){cat("rawTEJ$BTE_",t, sep="", fill=TRUE)}) -> list_BTE
for(i in 1:11){cat(cat("rawTEJ$BTEsum_", 2016-i, " <- ", sep="",fill=FALSE), cat(list_BTE[i:(i+4)], sep="+",fill=FALSE), sep="",fill=TRUE)}


# PTEBX - sum past five year data
capture.output(for(t in 2015:2001){cat("rawTEJ$PTEBX_",t, sep="", fill=TRUE)}) -> list_PTEBX
for(i in 1:11){cat(cat("rawTEJ$PTEBXsum_", 2016-i, " <- ", sep="",fill=FALSE), cat(list_PTEBX[i:(i+4)], sep="+",fill=FALSE), sep="",fill=TRUE)}


# CashTaxPaid (CTP) - sum past five year data
capture.output(for(t in 2015:2001){cat("rawTEJ$CashTaxPaid_",t, sep="", fill=TRUE)}) -> list_CTP
for(i in 1:11){cat(cat("rawTEJ$CTPsum_", 2016-i, " <- ", sep="",fill=FALSE), cat(list_CTP[i:(i+4)], sep="+",fill=FALSE), sep="",fill=TRUE)}

## make to calculate variables
capture.output(for(t in 2015:2005){cat("rawTEJ$ETR",t, sep="", fill=TRUE)}) -> l_ETR
capture.output(for(t in 2015:2005){cat("rawTEJ$CETR",t, sep="", fill=TRUE)}) -> l_CETR
capture.output(for(t in 2015:2005){cat("rawTEJ$BTEsum_",t, sep="", fill=TRUE)}) -> l_BTE
capture.output(for(t in 2015:2005){cat("rawTEJ$PTEBXsum_",t, sep="", fill=TRUE)}) -> l_PTEBX
capture.output(for(t in 2015:2005){cat("rawTEJ$CTPsum_",t, sep="", fill=TRUE)}) -> l_CTP

# dependent variable
for(x in 1:11){cat(l_ETR[x], "<-", l_BTE[x],"/", l_PTEBX[x], fill=TRUE)}
for(x in 1:11){cat(l_CETR[x], "<-", l_CTP[x],"/", l_PTEBX[x], fill=TRUE)}

# close sink
sink()
rm(list=ls(pattern="list_"))
rm(list=ls(pattern="l_"))
rm(t,x,i)
## ----
source("depVar.R")

# Write into explanatory variable - STRATEGY (file name = 'expVar_01STR.R')
# ----------
sink("expVar_01STR.R")

## Explanatory variables
# RD: OERD_/NetSales_ -> RD
capture.output(for(t in 2015:2001){cat("rawTEJ$OERD_",t, sep="", fill=TRUE)}) -> l_OERD
capture.output(for(t in 2015:2001){cat("rawTEJ$NetSales_",t, sep="", fill=TRUE)}) -> l_NetSales
capture.output(for(t in 2015:2001){cat("rawTEJ$RD_",t, sep="", fill=TRUE)}) -> l_RD1
for(y in 1:15){cat(l_RD1[y], "<-", l_OERD[y],"/", l_NetSales[y], fill=TRUE)}
capture.output(for(y in 1:14){cat(l_RD1[c( seq(from=y+1,to=y+5,by=1) )],sep=",",fill=TRUE)}) -> l_RD2
capture.output(sapply(strsplit(l_RD2, split=',NA', fixed=TRUE), function(x) (x[1]))) -> l_RD3
capture.output(for(t in 2015:2001){cat("rawTEJRDsum",t, sep="", fill=TRUE)}) -> l_RDsum
for(y in 1:10){cat(l_RDsum[y], "<-c(", l_RD3[y], ")", sep="", fill=TRUE)}
capture.output(for(t in 2015:2001){cat("rawTEJ$RD",t, sep="", fill=TRUE)}) -> l_RD
for(y in 1:14){cat(l_RD[y], "<-mean(", l_RDsum[y], ",na.rm=TRUE)",sep="",fill=TRUE)}

# EMP: employee_/NetSales_ -> EMP
capture.output(for(t in 2015:2001){cat("rawTEJ$employee_",t, sep="", fill=TRUE)}) -> l_employee
capture.output(for(t in 2015:2001){cat("rawTEJ$NetSales_",t, sep="", fill=TRUE)}) -> l_NetSales
capture.output(for(t in 2015:2001){cat("rawTEJ$EMP_",t, sep="", fill=TRUE)}) -> l_EMP1
for(y in 1:15){cat(l_EMP1[y], "<-", l_employee[y],"/", l_NetSales[y], fill=TRUE)}
capture.output(for(y in 1:15){cat(l_EMP1[c( seq(from=y+1,to=y+5,by=1) )],sep=",",fill=TRUE)}) -> l_EMP2
capture.output(for(t in 2015:2001){cat("rawTEJEMPsum",t, sep="", fill=TRUE)}) -> l_EMPsum
for(y in 1:14){cat(l_EMPsum[y], "<-c(", l_EMP2[y], ")", sep="", fill=TRUE)}
capture.output(for(t in 2015:2001){cat("rawTEJ$EMP",t, sep="", fill=TRUE)}) -> l_EMP
for(y in 1:14){cat(l_EMP[y], "<-mean(", l_EMPsum[y], ",na.rm=TRUE)",sep="",fill=TRUE)}

# MB: PB_ -> MB
capture.output(for(t in 2015:2001){cat("rawTEJ$PB_",t, sep="", fill=TRUE)}) -> l_PB1
capture.output(for(y in 1:15){cat(l_PB1[c( seq(from=y+1,to=y+5,by=1) )],sep=",",fill=TRUE)}) -> l_PB2
capture.output(for(t in 2015:2001){cat("rawTEJPBsum",t, sep="", fill=TRUE)}) -> l_PBsum
for(y in 1:14){cat(l_PBsum[y], "<-c(", l_PB2[y], ")", sep="", fill=TRUE)}
capture.output(for(t in 2015:2001){cat("rawTEJ$PB",t, sep="", fill=TRUE)}) -> l_PB
for(y in 1:14){cat(l_PB[y], "<-mean(", l_PBsum[y], ",na.rm=TRUE)",sep="",fill=TRUE)}

# MARKET: OEPRO_/NetSales_ -> MARKET
capture.output(for(t in 2015:2001){cat("rawTEJ$OEPRO_",t, sep="", fill=TRUE)}) -> l_OEPRO
capture.output(for(t in 2015:2001){cat("rawTEJ$NetSales_",t, sep="", fill=TRUE)}) -> l_NetSales
capture.output(for(t in 2015:2001){cat("rawTEJ$MARKET_",t, sep="", fill=TRUE)}) -> l_MARKET1
for(y in 1:15){cat(l_MARKET1[y], "<-", l_OEPRO[y],"/", l_NetSales[y], fill=TRUE)}
capture.output(for(y in 1:15){cat(l_MARKET1[c( seq(from=y+1,to=y+5,by=1) )],sep=",",fill=TRUE)}) -> l_MARKET2
capture.output(for(t in 2015:2001){cat("rawTEJMARKETsum",t, sep="", fill=TRUE)}) -> l_MARKETsum
for(y in 1:14){cat(l_MARKETsum[y], "<-c(", l_MARKET2[y], ")", sep="", fill=TRUE)}
capture.output(for(t in 2015:2001){cat("rawTEJ$MARKET",t, sep="", fill=TRUE)}) -> l_MARKET
for(y in 1:14){cat(l_MARKET[y], "<-mean(", l_MARKETsum[y], ",na.rm=TRUE)",sep="",fill=TRUE)}

# PPE: (FA_ - Land_ - LandR_)/TA_ -> PPE
capture.output(for(t in 2015:2001){cat("rawTEJ$FA_",t, sep="", fill=TRUE)}) -> l_FA
capture.output(for(t in 2015:2001){cat("rawTEJ$Land_",t, sep="", fill=TRUE)}) -> l_LAND
capture.output(for(t in 2015:2001){cat("rawTEJ$LandR_",t, sep="", fill=TRUE)}) -> l_LANDR
capture.output(for(t in 2015:2001){cat("rawTEJ$TA_",t, sep="", fill=TRUE)}) -> l_TA
capture.output(for(t in 2015:2001){cat("rawTEJ$PPE_",t, sep="", fill=TRUE)}) -> l_PPE1 # variable
for(y in 1:15){cat(l_PPE1[y], "<-", "(", l_FA[y],"-",l_LAND[y],"-",l_LANDR[y],")/", l_TA[y], fill=TRUE)}
capture.output(for(y in 1:15){cat(l_PPE1[c( seq(from=y+1,to=y+5,by=1) )],sep=",",fill=TRUE)}) -> l_PPE2
capture.output(for(t in 2015:2001){cat("rawTEJPPEsum",t, sep="", fill=TRUE)}) -> l_PPEsum
for(y in 1:14){cat(l_PPEsum[y], "<-c(", l_PPE2[y], ")", sep="", fill=TRUE)}
capture.output(for(t in 2015:2001){cat("rawTEJ$PPE",t, sep="", fill=TRUE)}) -> l_PPE
for(y in 1:14){cat(l_PPE[y], "<-mean(", l_PPEsum[y], ",na.rm=TRUE)",sep="",fill=TRUE)}

## close sink
sink()
rm(list=ls(pattern="l_"))
rm(t,y)
source("expVar_01STR.R")
rm(list=ls(pattern="sum"))
## ----

# Write into explanatory variable - HHI (file name = 'expVar_02HHI.R')
# ----------
indCode <- levels(as.factor(rawTEJ$TSE_indCode))
sink("expVar_02HHI.R")
for(i in 1:length(indCode)){cat("TSE_",i," <- subset(rawTEJ, TSE_indCode == indCode[",i,"],select = c(Company, TSE_indCode, NetSales_2015:NetSales_2001))", fill=TRUE, sep="")}

for(t in 2015:2001){for(i in 1:length(indCode)){cat("TSE_",i,"Q",t,"<- sum(TSE_",i,"$NetSales_",t,")",fill=TRUE,sep="")}}
for(t in 2015:2001){for(i in 1:length(indCode)){cat("TSE_",i,"$A",t,"<- (TSE_",i,"$NetSales_",t,"/TSE_",i,"Q",t,")",fill=TRUE,sep="")}}


capture.output(for(i in 1:length(indCode)){for(t in 2015:2001){cat("(TSE_",i,"A",t,")^2", sep="",fill=TRUE)}}) -> l_A
# TSE_1
capture.output(for(k in 1:31){for(i in 1:10){ cat(seq(from=(15*(k-1)+i+1),to=(15*(k-1)+i+5)), sep=",",fill=TRUE) }}
)-> seqq; print(seqq)
for(i in 1:length(indCode)){for(t in 2015:2006){cat(seqq[(-(t-2016))*i],sep = "",fill = TRUE)}}
for(i in 1:length(indCode)){for(t in 2015:2006){cat("TSE_",i,"HHI",t," <- ","sum(", cat(l_A[]) ,")", sep="",fill=TRUE)}}

## close sink
sink()
## ----

## Control Variables
# Write into control variable -- ROA, SIZE, LEV, INTANG, QUICK, 
# EQINC, OUTINSTI, RELATION < RELATIN , RELATOUT >,
# FAMILY < FAMILY, FAMILYNOTE> [p.s. NAs are presented "0"],
# GDP  (file name = 'conVar.R')
# ----------
sink("conVar.R")
# ROA : NetSales / TotalAssets
for(t in 2015:2001){cat("rawTEJ$ROA",t,"<-(rawTEJ$NetSales_",t,"/rawTEJ$TA_",t,")",sep="",fill=TRUE)}

# SIZE : ln(TA)
for(t in 2015:2001){cat("rawTEJ$SIZE",t,"<-log((rawTEJ$NetSales_",t,"/rawTEJ$TA_",t,"),base=exp(1))",sep="",fill=TRUE)}

# LEV : TL / TA
for(t in 2015:2001){cat("rawTEJ$LEV",t,"<-(rawTEJ$TL_",t,"/rawTEJ$TA_",t,")",sep="",fill=TRUE)}

# INTANG : intangible assets / TA
for(t in 2015:2001){cat("rawTEJ$INTANG",t,"<-(rawTEJ$INTANG_",t,"/rawTEJ$TA_",t,")",sep="",fill=TRUE)}

# QUICK : QUICK_year = QUICK

# EQINC : (InvIn + InvLos) / TA
# should make InvIn positive and make InvLos negative, so thus just add minus in denominator.
for(t in 2015:2001){cat("rawTEJ$EQINC",t,"<-(rawTEJ$InvIn_",t,"+rawTEJ$InvLos_",t,")/-rawTEJ$TA_",t,sep="",fill=TRUE)}

# OUTINSTI : OUTINSTI_year = OUTINSTI


# RELATION : Relation in and out, two variables
# RELATIN = RELATIN_year, RELATOUT = RELATOUT_year

# FAMILY : if company is FAMILY, then 1, else 0
for(t in 2015:2001){cat("rawTEJ$FAMILY",t,"<-ifelse(rawTEJ$FAMILY_",t," =='F','1','0')",sep="",fill=TRUE)}

# GDP : ln(realGDP)
GDP_colname <- read_excel("DB2.xlsx", sheet="GDP_colnames")
rGDP <- read_excel("DB2.xlsx", sheet="GDP", col_types = 'text')

## close sink
sink()
rm(t)
## ----


# Write into control variable - ROA (file name = 'conVar_ROA.R')
# ----------
## ----



# run codes above
# ----------
source("depVar.R")
source("expVar_01STR.R")
source("expVar_02HHI.R")
## ----

# remove lists
# ----------
rm(list=ls(pattern="sum"))
rm(list=ls(pattern="l_"))
rm(t,y)
## ----
