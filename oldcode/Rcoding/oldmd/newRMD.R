#' ---
#' title: "處理論文統計分析過程說明"
#' author: "Oscar-Deng"
#' date: "2016年7月31日"
#' output: 
#'   html_document: 
#'   keep_md: yes
#' theme: cosmo
#' toc: yes
#' toc_depth: 5
#' ---

#' ## 前言
#' 編輯人：鄧孝航
#' 聯絡信箱：[402391174@mail.fju.edu.tw](402391174@mail.fju.edu.tw)
#' 內容如有不當煩請告知，謝謝！
#' 為了推廣「可重複研究**(Reproducible Research)**」的概念並方便將來再次研究分析，故建立此說明檔解釋相關的R語言函數及數據處理過程。
#' 有關於可重複研究的概念，可參考維基百科[**(Reproducible Research)**](https://en.wikipedia.org/wiki/Reproducibility#Reproducible_research)。
#' 
#' 本分析使用R語言作為統計分析之工具，並搭配R、Rstudio、Excel、TEJ資料庫。
#' > 
#' 參考論文： **企業競爭策略與產業競爭程度對避稅行為之影響**
#' 作者：**史宗玄**
#' 指導教授：**黃美祝 博士**
#' <br>
#' 本文僅供學術研究之用！
#' <br>
#' 
#' ## Ready to run R
#' 
#' 欲建立運行環境，請先至[R的網站](https://cran.r-project.org/mirrors.html)下載新版的R安裝。
#' 
#' >
#' 1. 使用<kbd>Ctrl+F</kbd>搜尋**Taiwan**，並任選一鏡像下載點，或直接[點此下載](http://cran.csie.ntu.edu.tw/)。
#' 2. 請選擇適合自己電腦運行介面的版本，R提供Linux, Mac及Windows三種版本。
#' 3. R支援多國語言，從哪個鏡像下載不影響安裝。
#' 4. 建議版本需**3.3.0**以後。
#' 
#' 再至[Rstudio官網](https://www.rstudio.com/)下載主程式安裝，或[點此](https://www.rstudio.com/products/rstudio/download/)至下載頁面。
#' 
#' Rstudio載點快速連結：(**0.99.902**版，於2016/7/28更新)
#' 
#' > 
#' 1. [Windows Vista/7/8/10](https://download1.rstudio.org/RStudio-0.99.902.exe "RStudio-0.99.902.exe")
#' 2. [Mac OS X 10.6+ (64-bit)](https://download1.rstudio.org/RStudio-0.99.902.dmg "RStudio-0.99.902.dmg")
#' 3. [Ubuntu 12.04+/Debian 8+ (64-bit)](https://download1.rstudio.org/rstudio-0.99.902-amd64.deb "rstudio-0.99.902-amd64.deb")
#' 
#' 安裝完成後，請確認Rstudio或RGui之語言及區域(language & locale)設定正確：

#+ locale, eval=FALSE
# 查看環境設定
sessionInfo()
# 查看語言/地區設定
Sys.getlocale(category = "LC_ALL")
# 若上述回傳非顯示相同值，請輸入下方設定
Sys.setlocale("LC_ALL",locale='cht')

#' 其他疑難排解，請見[手冊](https://github.com/dspim/R/wiki/R-&-RStudio-Troubleshooting-Guide "R & RStudio Troubleshooting Guide")及[下方](#qa "Q&A")說明
#' 
#' 
#' ##**Empirical Analysis**
#' 
#' ### **Coding Process**
#' 
#' > 
#' 1. TEJ資料庫抓取資料建立分析資料庫。**(Getting Data)**
#' 2. 整理資料至可使用程度(排除不需要的欄位)。**(Preparing Data)**
#' 3. 產生虛擬變數及可供分析建模的變數。**(Produce Variables)**
#' 4. 以線性多變量回歸模型分析資料，並製作相關分析表。**(Analyze)**
#' 5. 產生報表。**(Produce reports and graphs)**
#' 6. 解釋分析結果。**(Explain)**
#' 
#' 
#' ### **Getting Data**
#' > 
#'  1. 開啟Excel，使用TEJ的Excel增益集。 [(如何開啟TEJ增益集?)](#如何開啟tej增益集)
#'  2. 讀入記錄檔.dat，可以得到本分析資料庫的原始設定。
#'  3. 運行RStudio
#' 
#' ### **Preparation for RStudio**
#+ setwd, 
#' 設定工作資料夾(EX: D:\Documents\Dropbox\MyEssay\Rcoding)
setwd("D:\\Documents\\Dropbox\\MyEssay\\Rcoding\\")
#' 清除環境清單
rm(list=ls())
#' # 跑全部檔案分析
#' `source('run2.R',encoding='utf-8')`
#' # 設定語言、區域
sessionInfo() # get R version
Sys.getlocale(category = "LC_ALL") # get locale
Sys.setlocale("LC_ALL",locale='cht') # set R locale to chinese traditional
#' # other trouble shooting please read:
#' [Rstudio guide](https://github.com/dspim/R/wiki/R-&-RStudio-Troubleshooting-Guide "R-&-RStudio-Troubleshooting-Guide")

#' 讀入函數設定
# Functions
# install all packages and load.
packtogo <- c("readxl","xlsx","plyr","dplyr","knitr", "data.table", #"dtplyr"
              "grid","gridExtra","ggplot2","zoo","R.oo","R.utils","psych",
              "robustHD","rbenchmark","foreign","rgl","stargazer","rmarkdown")

Install.pack <- function(list = packtogo){
  list.of.packages <- list
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)){install.packages(new.packages)}else{update.packages(list.of.packages)}
}
Load.pack <- function(list=as.list(packtogo)){lapply(list, require, character.only = TRUE)}

readDB <- function(fil = "DB2.xlsx", attr_sht = "TEJ_attr", xls_sht = "TEJ"){
  DBattr <- read_excel(fil, sheet=attr_sht, col_names = TRUE)
  # read in excel database: DB2.xlsx, excel sheet: TEJ, with column names.
  DBori <- read_excel(fil, sheet=xls_sht, col_names = TRUE, col_types = DBattr$attr)
  # rename columns
  setnames(DBori,old=as.character(DBattr$old), new=as.character(DBattr$new))
  return(DBori)
}
DBfilter <- function(x = TEJ,filt='filtered'){
  DB <- as.data.table(x)
  DB$year <- year(DB$date)
  DB0 <- DB[,.SD[.N > 0],by=list(TSE_code,year)]
  DB1 <- DB0[!(DB0$TSE_code %in% c('M2800','M9900','M2331','W91'))] # M2800金融業 # M9900其他 # M2331其他電子 # W91存託憑證
  DB2 <- DB1[,.SD[.N >= 5],by=list(TSE_code,year)] # removed M1800<2001-2005>,M2200<2001>
  DB3 <- DB2[!(DB2$FAMILY %in% NA) & # most family with NA got lots of NAs in other columns
               !(DB2$PB %in% NA) & # important var, must not be NA
               !(DB2$TA %in% NA) & # denominator or main var as PPE, ROA, SIZE, LEV, INTANG, must not bo NA.
               !(DB2$NetSales %in% c(0,NA)) & # remove netsales = 0 ... Denominator of (RD,EMP,MARKET),HHI's main var,
               !(DB2$employee %in% NA)]
  DB4 <- rbind(DB0,DB3)
  DB4 <- DB4[order(DB4$TSE_code,DB4$year),]
  DB5 <- DB4[!(duplicated(DB4) | duplicated(DB4, fromLast = TRUE)),]
  base::ifelse(filt=='filtered', return(DB2), base::ifelse(filt=='dropped', return(DB5), print("please assign filter type")))
} # 篩選後的:filt=filtered, #篩選刪掉的filt=dropped
NAto0 <- function(x = 'TEJ01',col=c(NA)){
  x1 <- captureOutput(
    for(y in col){cat(x,'$',y,'[is.na(',x,'$',y,')] <- 0',sep="",fill = TRUE)})
  x2 <- captureOutput(cat('return(',paste(x),')',sep=""))
  xx <- c(x1,x2)
  eval(base::parse(text=xx))} # replace NA with 0.
control_var <- function(x=TEJ1){
  y <- transform(x,
                 ROA = as.numeric(PTEBX) / as.numeric(TA), # ROA : NetSales / TotalAssets
                 SIZE = as.numeric(log(x = as.numeric(TA), base = exp(1))), # SIZE : ln(TA)
                 LEV = as.numeric(TL) / as.numeric(TA), # LEV : TL / TA
                 INTANG = as.numeric(INTAN) / as.numeric(TA), # INTANG : intangible assets / TA
                 QUICK = ifelse(is.na(QUICK),0,as.numeric(QUICK)), # QUICK : = QUICK
                 EQINC = as.numeric(-(InvIn + InvLoss)) / as.numeric(TA), # EQINC : (InvIn + InvLos) / -TA
                 OUTINSTI = ifelse(is.na(OUTINSTI),0,as.numeric(OUTINSTI)), # OUTINSTI : = OUTINSTI
                 RELATIN = ifelse(is.na(RELATIN),0,as.numeric(RELATIN)),
                 RELATOUT = ifelse(is.na(RELATOUT),0,as.numeric(RELATOUT)),
                 FAM_Dum = ifelse(FAMILY == 'F', 1, 0)
  )
  DB <- as.data.table(y[order(y$company,y$year),]) # sort by company<ascending> and year<ascending>
  return(DB)}
exp_var_STR <- function(x=TEJ1){
  y <- transform(x,
                 CTP_IFRS = as.numeric(-(CTP_IFRS_CFI + CTP_IFRS_CFO + CTP_IFRS_CFF)),
                 STR_RD = as.numeric(OERD) / as.numeric(NetSales),
                 STR_EMP = as.numeric(employee) / as.numeric(NetSales),
                 STR_MB = as.numeric(PB),
                 STR_MARKET = as.numeric(OEPRO) / as.numeric(NetSales),
                 STR_PPE = as.numeric( FA - Land - LandR ) / as.numeric(TA)
  )
  z <- transform(y, CTP = ifelse(year >= 2012,CTP_IFRS,CTP_GAAP)) # combine IFRS as 2012~ , GAAP as ~2011
  DB <- as.data.table(z[order(z$company,z$year),]) # sort by company<ascending> and year<ascending>
  return(DB)}
dep_var <- function(x=TEJ2,k=5){
  DB01 <- x[,.SD[.N >= k],by=company]
  DB02 <- x[,.SD[.N < k],by=company]
  DB1 <- DB01[,`:=`(BTE5yrsum = rollapplyr(BTE, width = 5, FUN = sum, fill = NA),
                    CTP5yrsum = rollapplyr(CTP, width = 5, FUN = sum, fill = NA),
                    PTEBX5yrsum = rollapplyr(PTEBX, width = 5, FUN = sum, fill = NA)),
              by=company]
  DB2 <- DB02[,`:=`(BTE5yrsum = NA,CTP5yrsum = NA,PTEBX5yrsum = NA),by=company]
  DB3 <- rbind(DB1,DB2)
  DB <- transform(DB3,
                  ETR = as.numeric(BTE5yrsum) / as.numeric(PTEBX5yrsum),
                  CETR = as.numeric(CTP5yrsum) / as.numeric(PTEBX5yrsum))
  return(as.data.table(DB[order(DB$company,DB$year),]))} # add up 5 years moving sum
STR <- function(x=TEJ4) {
  x <- x[order(x$company,x$year),]
  rollmn <- function(x) rollapplyr(x, width, function(x) mean(x, na.rm = TRUE), fill=NA)
  mkdt <- capture.output(for(i in 1:15){
    cat('DB',i,"<- x[,.SD[.N==",i,"],by=company]",sep="",fill=TRUE)
    if(i>5){cat("width <- list(numeric(0),-1,-(1:2),-(1:3),-(1:4)",rep(',-(1:5)',i-5),')',sep="",fill=TRUE)}
    if(i==5){cat("width <- list(numeric(0),-1,-(1:2),-(1:3),-(1:4)",')',sep="",fill=TRUE)}
    if(i==4){cat("width <- list(numeric(0),-1,-(1:2),-(1:3)",')',sep="",fill=TRUE)}
    if(i==3){cat("width <- list(numeric(0),-1,-(1:2)",')',sep="",fill=TRUE)}
    if(i==2){cat("width <- list(numeric(0),-1",')',sep="",fill=TRUE)}
    if(i==1){cat("width <- numeric(0)",sep="",fill=TRUE)}
    cat('DB',i,'<-transform(DB',i, 
        #cat(
        ",STR_RD_mean = ave(STR_RD, company, FUN=rollmn),STR_EMP_mean = ave(STR_EMP, company, FUN=rollmn),STR_MB_mean = ave(STR_MB, company, FUN=rollmn),STR_MARKET_mean = ave(STR_MARKET, company, FUN=rollmn),STR_PPE_mean = ave(STR_PPE, company, FUN=rollmn))",sep="",fill=TRUE)
  })
  eval(base::parse(text=mkdt))
  DT <- rbind(DB1,DB2,DB3,DB4,DB5,DB6,DB7,DB8,DB9,DB10,DB11,DB12,DB13,DB14,DB15)
  NAto0 <- function(x = 'DT',col=c('STR_RD_mean','STR_EMP_mean','STR_MB_mean','STR_MARKET_mean','STR_PPE_mean')){
    x1 <- captureOutput(
      for(y in col){cat(x,'$',y,'[is.nan(',x,'$',y,')] <- 0',sep="",fill = TRUE)})
    x2 <- captureOutput(cat('return(',paste(x),')',sep=""))
    xx <- c(x1,x2)
    eval(base::parse(text=xx))} # replace NA with 0.
  DT1 <- NAto0()
  DBA <- as.data.table(DT1[order(DT1$company,DT1$year),])
  return(DBA)
}

STRrank <- function(x=TEJ5){
  prank<-function(x) {ifelse(is.na(x),NA,rank(x,ties.method = 'min')/sum(!is.na(x)))} # STRATEGY ranktile.
  rankscore <- function(x) ifelse(x>=0 & x<=0.2,1,ifelse(x>0.2 & x<=0.4,2,ifelse(x>0.4 & x<=0.6,3,ifelse(x>0.6 & x<=0.8,4,ifelse(x>0.8 & x<=1,5,NA)))))
  DB <- transform(x[,by=c(TSE_code,year)],
                  STR_RD_mean_rank = prank(STR_RD_mean),
                  STR_EMP_mean_rank = prank(STR_EMP_mean),
                  STR_MB_mean_rank = prank(STR_MB_mean),
                  STR_MARKET_mean_rank = prank(STR_MARKET_mean),
                  STR_PPE_mean_rank = prank(STR_PPE_mean))
  DB2 <- transform(DB,
                   RD = rankscore(STR_RD_mean_rank),
                   EMP = rankscore(STR_EMP_mean_rank),
                   MB = rankscore(STR_MB_mean_rank),
                   MARKET = rankscore(STR_MARKET_mean_rank),
                   PPE = rankscore(STR_PPE_mean_rank))
  DB2$STR <- as.numeric(DB2$RD) + as.numeric(DB2$EMP) + as.numeric(DB2$MB) + as.numeric(DB2$MARKET) + as.numeric(DB2$PPE)
  return(DB2)} # rank score function
fnGDP <- function(x=TEJ91,file="DB2.xlsx",col_sht="GDP_colnames",DB_sht="GDP"){
  GDP_colname <- read_excel(file, sheet=col_sht)
  rGDP <- read_excel(file, sheet=DB_sht)
  setnames(rGDP, old=as.character(GDP_colname$old), new=as.character(GDP_colname$new))
  rGDP$year <- year(rGDP$Date)
  rGDP$GDP <- log(rGDP$Value,base=exp(1))
  GDP <- subset(rGDP,select=c(year,GDP))
  return(merge(x,GDP,by="year"))}
fnHHI <- function(x=TEJ6) {
  func <- function(z=y2) {
    rollmn <- function(x) rollapplyr(x, width, function(x) mean(x, na.rm = TRUE), fill=NA)
    mkdt <- capture.output(for(i in 1:15){
      cat('DB',i,"<- z[,.SD[.N==",i,"],by=TSE_code]",sep="",fill=TRUE)
      if(i>5){cat("width <- list(numeric(0),-1,-(1:2),-(1:3),-(1:4)",rep(',-(1:5)',i-5),')',sep="",fill=TRUE)}
      if(i==5){cat("width <- list(numeric(0),-1,-(1:2),-(1:3),-(1:4)",')',sep="",fill=TRUE)}
      if(i==4){cat("width <- list(numeric(0),-1,-(1:2),-(1:3)",')',sep="",fill=TRUE)}
      if(i==3){cat("width <- list(numeric(0),-1,-(1:2)",')',sep="",fill=TRUE)}
      if(i==2){cat("width <- list(numeric(0),-1",')',sep="",fill=TRUE)}
      if(i==1){cat("width <- numeric(0)",sep="",fill=TRUE)}
      cat('DB',i,'<-transform(DB',i, ",HHI = ave(HHIsum, TSE_code, FUN=rollmn))",sep="",fill=TRUE)
    })
    eval(base::parse(text=mkdt))
    DT <- rbind(DB1,DB2,DB3,DB4,DB5,DB6,DB7,DB8,DB9,DB10,DB11,DB12,DB13,DB14,DB15)
    return(DT)
  }
  x1 <- x[,NSsum := sum(NetSales,na.rm = TRUE),by=list(TSE_code,year)]
  x2 <- x1[,NSalpha2 := (as.numeric(NetSales) / as.numeric(NSsum))^2 ]
  x3 <- x2[,HHIsum := sum(NSalpha2,na.rm = TRUE),by=list(TSE_code,year)]
  y1 <- subset(x3,select=c(TSE_code,year,HHIsum))
  y2 <- y1[!duplicated(y1)][order(TSE_code, year),]
  y3 <- func(y2)
  y4 <- subset(y3,select=c(TSE_code,year,HHI))
  z1 <- merge(x3,y4,by=c("TSE_code","year"))
  z1$HHI <- ifelse(is.nan(z1$HHI),as.numeric(NA),as.numeric(z1$HHI))
  z1$HHI_mark <- ifelse(z1$HHI < 0.1,1,0)
  DB <- transform(z1, STR_HHI = as.numeric(STR * HHI_mark))
  #DBA <- DB[order(TSE_code,year,company)]
  return(DB)
}
catchDB <- function(x){
  y <- base::subset(x=x,select=c(company,market,TSE_code,TSE_name,year,
                                 ETR,CETR,STR,HHI,STR_HHI,
                                 ROA,SIZE,LEV,INTANG,QUICK,EQINC,OUTINSTI,RELATIN,RELATOUT,FAM_Dum
  ))
  return(y)}
winsorized.sample <- function (x, prob = 0) { # remove NA
  n <- length(x)
  n0 <- length(x[!is.na(x)])
  low <- floor(n0 * prob) + 1
  high <- n0 + 1 - low
  idx <- seq(1,n)
  DT<-data.frame(idx,x)
  DT2<-DT[order(DT$x,DT$idx,na.last=TRUE),]
  DT2$x[1:low]<-DT2$x[low]
  DT2$x[high:n0]<-DT2$x[high]
  DT3<-DT2[order(DT2$idx,DT2$x),]
  x2<-DT3$x
  return(x2)}
winsamp1 <- function(x = 'TEJ81', col=c('ETR','CETR','ROA','SIZE','LEV','INTANG','QUICK','EQINC','OUTINSTI','RELATIN','RELATOUT')
                     , prob=0.01, na.rm=TRUE){
  x1 <- captureOutput(cat('DB1<-',x,sep="",fill=TRUE))
  x2 <- captureOutput(for(y in col){cat('DB1$',y,' <- winsor(',x,'$',y,',trim = ',prob,',na.rm = ',na.rm,')',sep="",fill = TRUE)})
  eval(base::parse(text=x1))
  eval(base::parse(text=x2))
  return(DB1)}
winsamp2 <- function(x = 'TEJ82', col=c('ETR','CETR','ROA','SIZE','LEV','INTANG','QUICK','EQINC','OUTINSTI','RELATIN','RELATOUT')
                     , prob=0.01){
  x1 <- captureOutput(cat('DB1<-',x,sep="",fill=TRUE))
  x2 <- captureOutput(for(y in col){cat('DB1$',y,' <- winsorized.sample(',x,'$',y,',prob = ',prob,')',sep="",fill = TRUE)})
  eval(base::parse(text=x1))
  eval(base::parse(text=x2))
  return(DB1)}
# Function Ends.
# ----

#' 安裝所有未安裝之套件
Install.pack()

#' 讀入所有需要之套件
Load.pack()

#' 擷取工作表路徑
wd <- getwd()

#' 讀入TEJ資料表，設定擷取活頁名：TEJ，資料表屬性：TEJ_attr
TEJ <- readDB(fil = "DB2.xlsx", attr_sht = "TEJ_attr", xls_sht = "TEJ")

#' 應用函數：篩選資料表，使用函數DBfilter，指派已篩選的資料表為TEJ01，被篩去的為TEJ02
TEJ01 <- DBfilter(x = TEJ,filt = 'filtered')
TEJ02 <- DBfilter(x = TEJ,filt = 'dropped')

#' 將特定欄位之變數缺漏值設為0
TEJ1 <- NAto0(x ='TEJ01',col=c('OERD','OEPRO','Land','LandR','CTP_IFRS_CFI','CTP_IFRS_CFO','CTP_IFRS_CFF','CTP_GAAP'))

#' 加入控制變數：
TEJ2 <- control_var(x=TEJ1)

#' 準備解釋變數欄位：
TEJ3 <- exp_var_STR(x=TEJ2)

#' 加入應變數：
TEJ4 <- dep_var(TEJ3,k=5)

#' 加入解釋變數1：「企業競爭策略變數」
TEJ5 <- STR(TEJ4)
#' 將「企業競爭策略變數」以五分位法評分
TEJ6 <- STRrank(TEJ5)
TEJ7 <- fnHHI(TEJ6)
C:
#' 
TEJ81 <- TEJ7
TEJ81 <- winsamp1(x='TEJ81',col=c('ETR','CETR','ROA','SIZE','LEV','INTANG','QUICK','EQINC','OUTINSTI','RELATIN','RELATOUT')
                  ,prob=0.01,na.rm=TRUE)
TEJ82 <- TEJ7
TEJ82 <- winsamp2(x='TEJ82',col = c('ETR','CETR','ROA','SIZE','LEV','INTANG'
                                    ,'QUICK','EQINC','OUTINSTI','RELATIN','RELATOUT')
                  ,prob = 0.01)
TEJ91 <- catchDB(x=TEJ81)
TEJ92 <- catchDB(x=TEJ82)
TEJ101 <- fnGDP(x=TEJ91,file="DB2.xlsx",col_sht="GDP_colnames",DB_sht="GDP")
TEJ102 <- fnGDP(x=TEJ92,file="DB2.xlsx",col_sht="GDP_colnames",DB_sht="GDP")

MNCattr <- read_excel("MNC.xlsx", sheet='MNC_attr', col_names = TRUE,col_types =as.vector(rep("text",3)))
MNC <- read_excel("MNC.xlsx", sheet='MNC', col_names = TRUE)
setnames(MNC,old=as.character(MNCattr$old), new=as.character(MNCattr$new))
MNC$year <- year(MNC$date)

#' 
#' 
#' 
#' 
#' 
#' 





# ----
source('output.R',encoding='utf-8')
outputcsv()
#outputSPSS()
#outputSAS()
#outputSTATA

# ----
source('tables.R',encoding='utf-8')

# ----
print("Finished running 'run2.R' !")

#View(TEJ101)
#View(TEJ102)


# ----
# replace TSE_code to TEJ_code1 ### code beneath havn't finished!!!!!!!!!
# GIANT & MERIDA are deleted< sort by TSE_code>, do we have to use TEJ_code1 or TEJ_code2 to classify?
# ----


