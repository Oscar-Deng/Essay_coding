#' ---
#' title: "處理論文統計分析過程說明"
#' author: "Oscar-Deng"
#' date: "2016年7月31日"
#' output: 
#'   html_document:
#'     keep_md: yes
#'     theme: cosmo
#'     toc: yes
#'     toc_depth: 5
#' ---

#' ##**前言**
#' 編輯人：鄧孝航
#' 聯絡信箱：[402391174@mail.fju.edu.tw](mailto:402391174@mail.fju.edu.tw,402391174@mail.fju.edu.tw?subject=Questions&body=你好，我想請教關於...%0A請盡速回復，謝謝)
#' 內容如有不當煩請告知，謝謝！
#' 為了推廣「可重複研究**(Reproducible Research)**」的概念並方便將來再次研究分析，故建立此說明檔解釋相關的R語言函數及數據處理過程。
#' 有關於可重複研究的概念，可參考維基百科[**(Reproducible Research)**](https://en.wikipedia.org/wiki/Reproducibility#Reproducible_research)。
#' 
#' 本分析使用R語言作為統計分析之工具，並搭配R、Rstudio、Excel、TEJ資料庫。
#'
#' > 
#' 參考論文： **企業競爭策略與產業競爭程度對避稅行為之影響**
#' <br>
#' 作者：**史宗玄**
#' <br>
#' 指導教授：**黃美祝 博士**
#' <br>
#' 
#' 本文僅供學術研究之用！
#' <br>
#' 

#' ##**Empirical Analysis**
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
#' ### **使用R執行統計分析**
#' #### **設定及運行函數庫**
#' ##### 環境設定


#' 執行R統計編程
#' `source('run2.R',encoding='UTF-8')`


#' ##### 設定讀入資料庫函數
#+ function_readDB
# readDB函數：讀入TEJ之excel檔，並用excel中之屬性表設定欄位
readDB <- cmpfun(function(fil = "DB2.xlsx", attr_sht = "TEJ_attr", xls_sht = "TEJ"){
  DBattr <- read_excel(fil, sheet=attr_sht, col_names = TRUE)
  # read in excel database: DB2.xlsx, excel sheet: TEJ, with column names.
  DBori <- read_excel(fil, sheet=xls_sht, col_names = TRUE, col_types = DBattr$attr)
  # rename columns
  setnames(DBori,old=as.character(DBattr$old), new=as.character(DBattr$new))
  return(DBori)
})

#' 應用函數，讀入TEJ資料表，設定擷取活頁名：TEJ，資料表屬性：TEJ_attr
#+ load_readDB
TEJ <- readDB(fil = "DB2.xlsx", attr_sht = "TEJ_attr", xls_sht = "TEJ")
# 輸出TEJ資料前10筆，及TEJ基本統計敘述
head(TEJ,10)
summary(TEJ)
View(TEJ)

#' ##### 設定篩選函數，
#+ function_DBfilter
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
  base::ifelse(filt=='filtered', return(DB3), base::ifelse(filt=='dropped', return(DB5), print("please assign filter type")))
} # 篩選後的:filt=filtered, #篩選刪掉的filt=dropped

#'
#+ load_DBfilter
# 應用函數：篩選資料表，使用函數DBfilter，指派已篩選的資料表為TEJ01，被篩去的為TEJ02
TEJ01 <- DBfilter(x = TEJ,filt = 'filtered')
TEJ02 <- DBfilter(x = TEJ,filt = 'dropped')
TEJ01_2010 <- TEJ01[(TEJ01$year %in% seq(2001,2010))]
TEJ02_2010 <- TEJ02[(TEJ02$year %in% seq(2001,2010))]

# 輸出資料前10筆及基本統計敘述
head(TEJ01,10)
summary(TEJ01)
# 輸出資料前10筆及基本統計敘述
head(TEJ02,10)
summary(TEJ02)

#' #####
#' 將特定欄位之變數缺漏值設為0
#+ function_NAto0
NAto0 <- cmpfun(function(x = 'TEJ01',col=c(NA)){
  x1 <- captureOutput(
    for(y in col){cat(x,'$',y,'[is.na(',x,'$',y,')] <- 0',sep="",fill = TRUE)})
  x2 <- captureOutput(cat('return(',paste(x),')',sep=""))
  xx <- c(x1,x2)
  eval(base::parse(text=xx))}) # replace NA with 0.

#' 
#+ load_NAto0
TEJ1 <- NAto0(x ='TEJ01',col=c('OERD','OEPRO','Land','LandR','CTP_IFRS_CFI','CTP_IFRS_CFO','CTP_IFRS_CFF','CTP_GAAP'))
TEJ1_2010 <- NAto0(x ='TEJ01_2010',col=c('OERD','OEPRO','Land','LandR','CTP_IFRS_CFI','CTP_IFRS_CFO','CTP_IFRS_CFF','CTP_GAAP'))
# 輸出資料前10筆及基本統計敘述
head(TEJ1,10)
summary(TEJ1)

#' #####
#' 
#+ function_control_var
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

#' 加入控制變數：
#+ load_control_var
TEJ2 <- control_var(x=TEJ1)
TEJ2_2010 <- control_var(x=TEJ1_2010)
# 輸出資料前10筆及基本統計敘述
head(TEJ2,10)
summary(TEJ2)

#' #####
#+ function_exp_var_STR
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

#' 準備解釋變數欄位：
#+ load_exp_var_STR
TEJ3 <- exp_var_STR(x=TEJ2)
TEJ3_2010 <- exp_var_STR(x=TEJ2_2010)

#+ add.TEJ31, eval=TRUE 
# add.RELATION ratio
fn_relation <- function(x=TEJ3){
  y <- transform(x,RELAT = RELATIN/RELATOUT)}
TEJ3 <- fn_relation(TEJ3)


# 輸出資料前10筆及基本統計敘述
head(TEJ3,10)
summary(TEJ3)

#' #####
#+ function_dep_var
dep_var <- function(x=TEJ3,k=5){
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

#' 加入應變數：
#+ load_dep_var
TEJ4 <- dep_var(TEJ3,k=5)
TEJ4_2010 <- dep_var(TEJ3_2010,k=5)
# 輸出資料前10筆及基本統計敘述
head(TEJ4,10)
summary(TEJ4)

#' #####設定STR函數
#+ function_STR
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

#' 加入解釋變數1：「企業競爭策略變數」
#+ load_STR
# this step is slow!
STR <- cmpfun(STR)
TEJ5 <- STR(TEJ4)
TEJ5_2010 <- STR(TEJ4_2010)
# 輸出資料前10筆及基本統計敘述
head(TEJ5,10)
summary(TEJ5)

#' #####設定STR排名函數
#+ function_STRrank
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

#' 將「企業競爭策略變數」以五分位法評分
#+ load_STRrank
TEJ6 <- STRrank(TEJ5)
TEJ6_2010 <- STRrank(TEJ5_2010)

# 輸出資料前10筆及基本統計敘述
head(TEJ6,10)
summary(TEJ6)

#' #####
#+ function_fnHHI
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

#'
#+ load_fnHHI
TEJ7 <- fnHHI(TEJ6)
TEJ7_2010 <- fnHHI(TEJ6_2010)
# 輸出資料前10筆及基本統計敘述
head(TEJ7,10)
summary(TEJ7)

#' #####
#+ function_wind
winsorized.sample <- function (x, prob = 0) { # remove NA
  n <- length(x)
  n0 <- length(x[!is.na(x)])
  low <- floor(n0 * prob) + 1
  high <- n0 + 1 - low
  idx <- seq(1,n)
  DT <-data.frame(idx,x)
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
winsamp1 <- cmpfun(winsamp1)
winsamp2 <- cmpfun(winsamp2)

#'
#+ load_winsamp1
TEJ81 <- TEJ7
TEJ81 <- winsamp1(x='TEJ81',col=c('ETR','CETR','ROA','SIZE','LEV','INTANG','QUICK','EQINC','OUTINSTI','RELATIN','RELATOUT')
                  ,prob=0.01,na.rm=TRUE)
TEJ82 <- TEJ7
TEJ82 <- winsamp2(x='TEJ82',col = c('ETR','CETR','ROA','SIZE','LEV','INTANG'
                                    ,'QUICK','EQINC','OUTINSTI','RELATIN','RELATOUT')
                  ,prob = 0.01)
# 輸出資料前10筆及基本統計敘述
head(TEJ81,10)
summary(TEJ81)
# 輸出資料前10筆及基本統計敘述
head(TEJ82,10)
summary(TEJ82)

#' #####
#+ function_catchDB
catchDB <- function(x){
  y <- base::subset(x=x,select=c(company,market,TSE_code,TSE_name,year,
                                 ETR,CETR,STR,HHI,STR_HHI,
                                 ROA,SIZE,LEV,INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum
  ))
  return(y)}

#'
#+ load_catchDB
TEJ91 <- catchDB(x=TEJ81)
TEJ92 <- catchDB(x=TEJ82)
# 輸出資料前10筆及基本統計敘述
head(TEJ91,10)
summary(TEJ91)
# 輸出資料前10筆及基本統計敘述
head(TEJ92,10)
summary(TEJ92)

#' #####
#+ function_fnGDP
fnGDP <- function(x=TEJ91,file="DB2.xlsx",col_sht="GDP_colnames",DB_sht="GDP"){
  GDP_colname <- read_excel(file, sheet=col_sht)
  rGDP <- read_excel(file, sheet=DB_sht)
  setnames(rGDP, old=as.character(GDP_colname$old), new=as.character(GDP_colname$new))
  rGDP$year <- year(rGDP$Date)
  rGDP$GDP <- log(rGDP$Value,base=exp(1))
  GDP <- subset(rGDP,select=c(year,GDP))
  return(merge(x,GDP,by="year"))}

#'
#+ load_fnGDP
TEJ101 <- fnGDP(x=TEJ91,file="DB2.xlsx",col_sht="GDP_colnames",DB_sht="GDP")
TEJ102 <- fnGDP(x=TEJ92,file="DB2.xlsx",col_sht="GDP_colnames",DB_sht="GDP")
# 輸出資料前30筆及基本統計敘述
head(TEJ101,30)
summary(TEJ101)
# 輸出資料前30筆及基本統計敘述
head(TEJ102,30)
summary(TEJ102)

#' #####讀入MNC資料庫(TEJ)
#' 來源檔：TEJ
readMNC <- function(x="MNC.xlsx",DB='MNC',attr='MNC_attr'){
  MNCattr <- read_excel(x, sheet=attr, col_names = TRUE,col_types =as.vector(rep("text",3)))
  MNC <- read_excel(x, sheet=DB, col_names = TRUE)
  setnames(MNC,old=as.character(MNCattr$old), new=as.character(MNCattr$new))
  MNC$year <- year(MNC$date)
  return(MNC)
}

#' 運行MNC函數
#+ load_readMNC
MNC <- readMNC()
summary(MNC)
#' 列出國外營運公司之所在地統計
table(MNC$nation)

#' #####計算各公司MNC變數
#' 
fnMNC <- function(x=TEJ101,y=MNC,feedback=c('x','plot','table')){
  x <- as.data.table(x)
  y <- as.data.table(y)
  y_TW <- plyr::count(y[y$nation %in% '台灣'],vars=c("company","year"))
  setnames(y_TW,'freq','MNC_TW')
  y_FOR <- plyr::count(y[!(y$nation %in% '台灣')],vars=c("company","year"))
  setnames(y_FOR,'freq','MNC_FOREIGN')
  y_mix <- merge(y_TW,y_FOR,by=c('company','year'),all = TRUE)
  x <- merge(x,y_mix,by=c('company','year'))
  # plot <- 
  
  ifelse(feedback=="x",return(x),ifelse(feedback=="plot",return(plot)))
}

#' 
#+
TEJ111 <- fnMNC(x=TEJ101,y=MNC,feedback='x')
TEJ112 <- fnMNC(x=TEJ102,y=MNC,feedback='x')
#+ linear models, eval=TRUE, echo=TRUE
# fix RELATIN/RELATOUT = NaN, Inf, NA (for modeling can't include thee)
TEJ_lm <- replace(TEJ101,TEJ101$RELAT[!is.finite(TEJ101$RELAT)],0)
# without STR*HHI
ETR_lmodel <- lm(ETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm)
CETR_lmodel <- lm(CETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm)

ETR_lm_MNC <- lm(ETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP+MNC,TEJ_lm)
CETR_lm_MNC <- lm(CETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP+MNC,TEJ_lm)

# with STR*HHI
ETR_lmodel2 <- lm(ETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm)
CETR_lmodel2 <- lm(CETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm)

ETR_lm_MNC2 <- lm(ETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP+MNC,TEJ_lm)
CETR_lm_MNC2 <- lm(CETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP+MNC,TEJ_lm)


#' ###報表輸出
#' ####一、樣本篩選量表
#' #####表一、
#' function_plottbA1
plottbA1 <- function(Q){
  x <- nrow(TEJ)
  x1 <- nrow(TEJ[TEJ$TSE_code=='M2800',])
  x2 <- nrow(TEJ[TEJ$TSE_code=='M9900',])
  x3 <- nrow(TEJ[TEJ$TSE_code=='M2331',])
  x4 <- nrow(TEJ[TEJ$TSE_code=='W91',])
  DB05 <- TEJ[!(TEJ$TSE_code %in% c('M2800','M9900','M2331','W91')),]
  x5 <- nrow(DB05)
  DB06 <- as.data.table(DB05)[,.SD[.N<5],by=list(TSE_code,year(date))]
  x6 <- nrow(DB06)
  DB07 <- as.data.table(DB05)[,.SD[.N >= 5],by=list(TSE_code,year(date))]
  x7 <- nrow(DB07)
  DB08 <- DB07[(DB07$FAMILY %in% NA) |(DB07$PB %in% NA) |(DB07$TA %in% NA) |(DB07$NetSales %in% c(0,NA)) |(DB07$employee %in% NA)]
  x8 <- nrow(DB08)
  DB09 <- DB07[!(DB07$FAMILY %in% NA) & !(DB07$PB %in% NA) & !(DB07$TA %in% NA) & !(DB07$NetSales %in% c(0,NA)) & !(DB07$employee %in% NA)]
  x9 <- nrow(DB09)
  tbA1 <- data.frame(
    '說明'= c('2001~2015 原始樣本總數','刪除金融保險業(TSE產業代碼 M2800)','刪除其他產業(TSE產業代碼 M9900)',
            '刪除其他電子業(TSE產業代碼 M2331)','刪除存託憑證(TSE產業代碼 W91)','刪除當年度產業內公司家數不足5筆之樣本',
            '刪除有缺漏值且足以影響分析之樣本','全樣本合計'),
    '樣本數'=as.integer(c(x, x1, x2, x3, x4, x6, x8, x9)),
    '小計'=as.integer(c(x,'','','',ifelse(x1+x2+x3+x4 == x-x5,x-x5,'error'),'',x6+x8,ifelse(x5-x6-x8 == x9,x9,'error')))
  )
  
  theme1 <- ttheme_default(
    core = list(
      fg_params = list(
        fontface=c(rep("plain", 7),"bold.italic")
        #fontsize,hjust=0,x=0.1
      ),
      bg_params = list(
        fill=c(rep(c("grey95", "grey90"),length.out=7),
               "#6BAED6"),
        alpha = rep(c(1,0.5), each=5)
      )
    ),
    colhead = list(
      fg_params = list(fontsize=9,fontface="bold"))
  )
  
  m <- format(tbA1, digits = 1, scientific=F,big.mark = ",")
  g1 <- tableGrob(m, theme = theme1, rows=NULL)
  # png(filename="table1.png",width=125,height = 70,units="mm",res = 500)
  grid.draw(g1)
  dev.off()
  # write.xlsx(tbA1,file="tables.xlsx",sheetName = "table1",col.names = TRUE,row.names = FALSE,showNA = FALSE,append = FALSE)
  return(tbA1)
}

#' 運行plottbA1
#+ load_plottbA1, fig.width=5, fig.height=5, dpi=500
tbA1 <- plottbA1()

#' #####表X、
#' plottbA2
#+ function_plottbA2
plottbA2 <- function(){
  TEJ01$TSE <- paste(TEJ01$TSE_code,TEJ01$TSE_name,sep=" ")
  tbA2 <- as.data.frame.matrix(table(TEJ01$TSE,TEJ01$year))
  tbA2 <- cbind(tbA2, '小計'=rowSums(tbA2,na.rm = TRUE))
  tbA2 <- rbind(tbA2, '合計'=colSums(tbA2,na.rm = TRUE))
  png(filename="table2.png",width=300,height = 200,units="mm",res = 500)
  grid.table(tbA2)
  dev.off()
  # write.xlsx(tbA2,file="tables.xlsx",sheetName = "table2",col.names = TRUE,row.names = TRUE,showNA = FALSE,append = TRUE)
  return(tbA2)
}
#' 運行plottbA2
#+ load_plottbA2, fig.width=10, fig.height=10, dpi=500
tbA2 <- plottbA2()


#' ####二、敘述統計表
#' #####表X、
#' plottbA3
#+ function_plottbA3
plottbA3 <- function(){
  #fnmin <- function(x){apply(TEJ101[,6:21,with=FALSE],2,mean(x,na.rm=TRUE))}
  #write(stargazer::stargazer(TEJ101,type = "html"),file="table3.html",append = TRUE)
  write(stargazer::stargazer(TEJ101,type = "html"),file="table3.html",append = FALSE)
  }
#' 運行plottbA3
#+ load_plottbA3
tbA3 <- plottbA3()

#' #####表X、
#' plottbA4
#+ function_plottbA4
plottbA4 <- function(){
  TEJ101n <- TEJ101
  TEJ101n$RELAT <- (TEJ101$RELATIN/TEJ101$RELATOUT)
  tbA4 <- base::subset(TEJ101n,select=c(ETR,CETR,STR,HHI,STR_HHI,ROA,SIZE,LEV,INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum))
  tbA4$RELAT[which(!is.finite(tbA4$RELAT))] <- 1000
  #tbA4$RELAT[which(is.na(tbA4$RELAT))] <- NA
  #cor(tbA4,,method = 'pearson')
  cortb <- round(cor(tbA4,use='na.or.complete',method='pearson'),3)
  lower <- cortb
  lower[lower.tri(cortb,diag = TRUE)] <- ""
  lower <- as.data.frame(lower)
  write(stargazer(lower,type = 'html',summary=FALSE),file="table4.html",append = FALSE)
  }

#' 運行plottbA4
#+ load_plottbA4
tbA4 <- plottbA4()

#' #####表X、
#' plottbA5
#+ function_plottbA5
plottbA5 <- function(){
  HHI_DB <- base::subset(TEJ101, select=c(TSE,year,HHI)) %>% distinct
# 高寡佔I 型≧0.3＞高寡佔II 型≧0.18＞低寡占I 型≧0.14＞低寡占II 型≧0.1＞競爭I 型≧0.05＞競爭II 型
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI >= 0.3,'高寡佔I 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.3 & HHI_DB$HHI >= 0.18,'高寡佔II 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.18 & HHI_DB$HHI >= 0.14,'低寡占I 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.14 & HHI_DB$HHI >= 0.1,'低寡占II 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.1 & HHI_DB$HHI >= 0.05,'競爭I 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.05,'競爭II 型')
  HHI_tbl <- dcast(HHI_DB,TSE ~ year) %>% as.data.frame
  HHI_tbl <- as.data.frame(HHI_tbl[,-1],row.names=HHI_tbl[,1])
  write(stargazer(HHI_tbl,summary = FALSE,type='html',notes = 
      c("註a.分類方式參考美國司法部之市場結構分類標準，依HHI 值判斷其競爭程度，HHI 值愈小代表該產業集中度愈低，產業競爭程度愈激烈。",
      "註b.分類區間：高寡佔I 型≧0.3＞高寡佔II 型≧0.18＞低寡占I 型≧0.14＞低寡占II 型≧0.1＞競爭I 型≧0.05＞競爭II 型。")),
    file='tableA5.html',append=FALSE)
  return(HHI_tbl)
  }

#'運行plottbA5
#+ load_plottbA5
tbA5 <- plottbA5()

#' ####三、相關係數分析
#' #####plot
plottbB1 <- function(){
    stargazer::stargazer(ETR_lmodel,CETR_lmodel,
                         
                         type='html',
                         style='default',
                         align=TRUE,
                         column.labels = c("TAXAVO_{it}=ETR_{it}","TAXAVO_{it}=CashETR_{it}"),
                         digits=3,
                         dep.var.labels.include=T,
                         dep.var.caption="formula",
                         #summary=TRUE,
                         ci=TRUE,
                         ci.level=0.99,
                         single.row=T,
                         notes.append=T,
                         title="實證結果─不包含STRATEGY×HHI",
                         notes.align='l',
                       #  notes.lable="註：",
                         notes = "變數定義同表4-1",
                         out='tbB1.html')
  stargazer::stargazer(ETR_lmodel2,CETR_lmodel2,
                       type='html',
                       style='default',
                       title="實證結果─包含STRATEGY×HHI",
                       align=TRUE,
                       column.labels = c("TAXAVO=ETR","TAXAVO=CashETR"),
                       digits=3,
                       summary=TRUE,
                       ci=TRUE,
                       ci.level=0.99,
                       single.row=TRUE,
                       out='tbB2.html')
  
  write(
    xtable(ETR_lmodel),
  #  xtable(CETR_lmodel,align='r'),
    file='tbB1_1.html',
    append = FALSE
  )
}
tbB1 <- plottbB1()
#' #####plot
plottbB2 <- function(){}
tbB2 <- plottbB2()
#' #####plot
plottbB3 <- function(){}
tbB3 <- plottbB3()
#' #####plot
plottbB4 <- function(){}
tbB4 <- plottbB4()

#' ####四、實證分析表
#' #####plot
#' #####plot
#' #####plot
#' #####plot

#' ####五、敏感性分析
#' #####plot

#' #####1.百分位等級分數來衡量企業競爭策略
#' #####plot
#' #####plot
#' #####plot

#' #####2.前四大廠商市場占有率來衡量產業競爭程度
#' #####plot
#' #####plot
#' #####plot

#' #####3.財稅差異來衡量企業從事避稅行為的程度
#' #####plot
#' #####plot
#' #####plot

#' ####六、其他分析
#' #####plot
#' #####plot

#' ##結論
#' #####plot
#' #####plot

#' ##參考文獻
#' table:
#' <br>
#' <br>
#' <br>
#' ----------------本文章到此！down file:
#' `rmarkdown::render('R_code_withRMD.R', encoding = 'UTF-8')`
