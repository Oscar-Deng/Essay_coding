#' ##### 設定讀入資料庫函數
#+ function_readDB
# readDB函數：讀入TEJ之excel檔，並用excel中之屬性表設定欄位
source("settings.R")
readDB <- function(DB_fil = "TEJ1996.xlsx", attr_fil = "DB2.xlsx", attr_sht = "TEJ_attr", xls_sht = "TEJ"){
  DBattr <- read_excel(attr_fil, sheet=attr_sht, col_names = TRUE)
  # read in excel database: DB2.xlsx, excel sheet: TEJ, with column names.
  DBori <- read_excel(DB_fil, sheet=xls_sht, col_names = TRUE, col_types = DBattr$attr)
  # rename columns
  setnames(DBori,old=as.character(DBattr$old), new=as.character(DBattr$new))
  write.csv(na_count(DBori),file="原始資料集缺漏值.csv",row.names=TRUE)
  return(DBori)
}
#' ##### 設定篩選函數，
#+ function_DBfilter
DBfilter <- function(x = TEJ,filename="表1.樣本篩選表",out=c("csv","txt")){
  x$year <- year(x$date)
  x.filt.ind <- filter(x, !TSE_code %in% c("M2800","M9900","M2331","W91",NA))
  x.dropped.ind <- filter(x, TSE_code %in% c("M2800","M9900","M2331","W91",NA))
  # M2800金融業 # M9900其他 # M2331其他電子 # W91存託憑證
  y.na0 <- x.filt.ind %>% filter(FAMILY %in% NA | PB %in% NA | TA %in% NA | NetSales %in% c(NA,0) | employee %in% c(NA,0) | is.na(QUICK) | is.na(OUTINSTI))
  # if want to fill out all nas, use "na.omit"
  # 主要篩了五個變數，家族na、本益比na、總資產na、淨銷貨0.na、員工人數0.NA
  y.full <- x.filt.ind %>% filter(!is.na(FAMILY) & !is.na(PB) & !is.na(TA) & !NetSales %in% c(NA,0) & !employee %in% c(NA,0) & !is.na(QUICK) & !is.na(OUTINSTI))
  y.full.big5 <- y.full %>% group_by(TSE_code,year) %>% filter(NROW(TSE_code) >= 5)
  y.full.small5 <- y.full %>% group_by(TSE_code,year) %>% filter(NROW(TSE_code) < 5)
  tbA1 <- data.frame(
    "intro" = c('2001~2015 original sample amount'
                ,'delete "financial ind."(TSE ind. code: M2800)'
                ,'delete "other ind."(TSE ind. code: M9900)'
                ,'delete "other electronic ind."(TSE ind. code: M2331)'
                ,'delete "DRs"(TSE ind. code: W91)'
                ,'delete empty ind.'
                ,'delete samples which had missing value, and will affect the research'
                ,'delete ind. which sample amount less then 5 firms in a year(list in filt.ind.png)'
                ,'Total sample amount after filtering='),
    "samp.amount" = c(NROW(x) # 原始樣本數
    , -NROW(filter(x.dropped.ind, TSE_code == "M2800")) # 刪除金融
    , -NROW(filter(x.dropped.ind, TSE_code == "M9900")) # 刪除其他
    , -NROW(filter(x.dropped.ind, TSE_code == "M2331")) # 刪除M2331其他電子
    , -NROW(filter(x.dropped.ind, TSE_code == "W91")) # 刪除W91存託憑證
    , -NROW(filter(x.dropped.ind, is.na(TSE_code))) # 刪除缺產業值
    , -NROW(y.na0) # 刪除其他缺漏
    , -NROW(y.full.small5) # 刪除家數不足5筆
    , NROW(y.full.big5)) # 全樣本合計
  )
  tab <- table(y.full.small5$TSE_name,y.full.small5$year)
  tab.ind <- as.data.frame.matrix(tab)
  write.csv(na_count(y.na0),"其他闕漏值參考表.csv",row.names=TRUE)
  if(out=="csv"){
    filename <- capture.output(cat(filename,".csv",sep=""))
  write.csv(tbA1, file=filename,row.names=FALSE)
  write.csv(tab.ind, file="tab.ind.csv",row.names=FALSE)
    }else{
      #filename <- capture.output(cat(filename,".txt",sep=""))
  #write(stargazer(tbA1,type = "text",summary = FALSE,align = c("r","r")),file = filename)
  #write(xtable(tbA1,align="rrr",caption="",display = c(2,3)),file=filename)
      table.png(obj = tbA1, name = filename, align = "llr")
      write.csv(tab.ind, file="tab.ind.csv",row.names=FALSE)
      cat("Also print table ",filename,"in folder, complete.",sep="")
    }
  return(y.full.big5)
  }

#' 將特定欄位之變數缺漏值設為0
#+ function_repNAto0
replaceNAby0 <- function(x,col=c()){ 
x[,col] <- apply(x[,col],2,function(z){replace(z, is.na(z), 0)})
return(x)
}

#+ function_control_var
norm_var <- function(x=TEJ1){
  return(
    x %>% mutate(
                 ROA = as.numeric(PTEBX) / as.numeric(TA), # ROA : NetSales / TotalAssets
                 SIZE = as.numeric(log(x = as.numeric(TA), base = exp(1))), # SIZE : ln(TA)
                 LEV = as.numeric(TL) / as.numeric(TA), # LEV : TL / TA
                 INTANG = as.numeric(INTAN) / as.numeric(TA), # INTANG : intangible assets / TA
                 QUICK = ifelse(is.na(QUICK),0,as.numeric(QUICK)), # QUICK : = QUICK
                 EQINC = as.numeric(-(InvIn + InvLoss)) / as.numeric(TA), # EQINC : (InvIn + InvLos) / -TA
                 OUTINSTI = ifelse(is.na(OUTINSTI),0,as.numeric(OUTINSTI)), # OUTINSTI : = OUTINSTI
                 RELATIN = ifelse(is.na(RELATIN),0,as.numeric(RELATIN)),
                 RELATOUT = ifelse(is.na(RELATOUT),0,as.numeric(RELATOUT)),
                 FAM_Dum = ifelse(FAMILY == 'F', 1, 0),
                 CTP_IFRS = as.numeric(-(CTP_IFRS_CFI + CTP_IFRS_CFO + CTP_IFRS_CFF)),
                 STR_RD = as.numeric(OERD) / as.numeric(NetSales),
                 STR_EMP = as.numeric(employee) / as.numeric(NetSales),
                 STR_MB = as.numeric(PB),
                 STR_MARKET = as.numeric(OEPRO) / as.numeric(NetSales),
                 STR_PPE = as.numeric( FA - Land - LandR ) / as.numeric(TA)
                 ) %>%
      mutate(CTP = ifelse(year >= 2012,CTP_IFRS,CTP_GAAP))
)}

#+ function_dep_var
dep_var <- function(x=TEJ3){
  return( 
    x %>%
      arrange(company,year) %>%
      group_by(company) %>%
      mutate(BTE.rollsum = rollapplyr(BTE,5,sum,partial=TRUE),
             CTP.rollsum = rollapplyr(CTP,5,sum,partial=TRUE),
             PTEBX.rollsum = rollapplyr(PTEBX,5,sum,partial=TRUE),
             RELAT = ifelse(!is.finite(RELATIN/RELATOUT),0,RELATIN/RELATOUT),
             ETR = ifelse(!is.finite(as.numeric(BTE.rollsum/PTEBX.rollsum)),0,as.numeric(BTE.rollsum/PTEBX.rollsum)),
             CETR = ifelse(!is.finite(as.numeric(CTP.rollsum/PTEBX.rollsum)),0,as.numeric(CTP.rollsum/PTEBX.rollsum)))
  )} # add up 5 years moving sum


#+ function_STR
STR <- function(x=TEJ4) {
  prank<-function(x) {ifelse(is.na(x),NA,rank(x,ties.method = 'min')/sum(!is.na(x)))} # STRATEGY ranktile.
  rankscore <- function(x) {ifelse(!is.finite(x) | x == 0,0,ifelse(x>0 & x<=0.2,1,ifelse(x>0.2 & x<=0.4,2,ifelse(x>0.4 & x<=0.6,3,ifelse(x>0.6 & x<=0.8,4,ifelse(x>0.8 & x<=1,5,NA))))))}
  y <- x %>% group_by(company) %>% arrange(year) %>%
    mutate(STR_RD.lag = lag(STR_RD, n=1)
           ,STR_EMP.lag = lag(STR_EMP, n=1)
           ,STR_MB.lag = lag(STR_MB, n=1)
           ,STR_MARKET.lag = lag(STR_MARKET, n=1)
           ,STR_PPE.lag = lag(STR_PPE, n=1)) %>%
    mutate(STR_RD.lagmean = rollapplyr(STR_RD.lag,5,FUN = function(x){mean(x,na.rm=TRUE)},partial=TRUE,fill=NA)
           ,STR_EMP.lagmean = rollapplyr(STR_EMP.lag,5,FUN = function(x){mean(x,na.rm=TRUE)},partial=TRUE,fill=NA)
           ,STR_MB.lagmean = rollapplyr(STR_MB.lag,5,FUN = function(x){mean(x,na.rm=TRUE)},partial=TRUE,fill=NA)
           ,STR_MARKET.lagmean = rollapplyr(STR_MARKET.lag,5,FUN = function(x){mean(x,na.rm=TRUE)},partial=TRUE,fill=NA)
           ,STR_PPE.lagmean = rollapplyr(STR_PPE.lag,5,FUN = function(x){mean(x,na.rm=TRUE)},partial=TRUE,fill=NA)
    )  # mean of 0 is NaN ... 0/0=NaN
  return(
    y %>% group_by(TSE_code,year) %>% arrange(year) %>%
      mutate(STR_RD.perank = prank(STR_RD.lagmean)
             ,STR_EMP.perank = prank(STR_EMP.lagmean)
             ,STR_MB.perank = prank(STR_MB.lagmean)
             ,STR_MARKET.perank = prank(STR_MARKET.lagmean)
             ,STR_PPE.perank = prank(-STR_PPE.lagmean)
      ) %>%
      mutate(RD = rankscore(STR_RD.perank)
             ,EMP = rankscore(STR_EMP.perank)
             ,MB = rankscore(STR_MB.perank)
             ,MARKET = rankscore(STR_MARKET.perank)
             ,PPE = rankscore(STR_PPE.perank)
      ) %>%
      rowwise() %>%
      mutate(STR = sum(RD,EMP,MB,MARKET,PPE,na.rm=TRUE)
             ,STR.rank = sum(STR_RD.perank,STR_EMP.perank,STR_MB.perank,STR_MARKET.perank,STR_PPE.perank,na.rm=TRUE))
  )}


#+ function_fnHHI
fnHHI <- function(x=TEJ6) {
  func <- function(z=y2) {
    rollmn <- function(x) rollapplyr(x, width, function(x) mean(x, na.rm = TRUE), fill=NA)
    mkdt <- capture.output(for(i in 1:20){
      cat('DB',i,"<- z[,.SD[.N==",i,"],by=TSE_code]",sep="",fill=TRUE)
      if(i>5) cat("width <- list(numeric(0),-1,-(1:2),-(1:3),-(1:4)",rep(',-(1:5)',i-5),')',sep="",fill=TRUE)
             if(i==5) cat("width <- list(numeric(0),-1,-(1:2),-(1:3),-(1:4)",')',sep="",fill=TRUE)
                    if(i==4) cat("width <- list(numeric(0),-1,-(1:2),-(1:3)",')',sep="",fill=TRUE)
                           if(i==3) cat("width <- list(numeric(0),-1,-(1:2)",')',sep="",fill=TRUE)
                                  if(i==2) cat("width <- list(numeric(0),-1",')',sep="",fill=TRUE)
                                        if(i==1) cat("width <- list(numeric(0))",sep="",fill=TRUE)
                                        
      cat('DB',i,'<-transform(DB',i, ",HHI = ave(HHIsum, TSE_code, FUN=rollmn))",sep="",fill=TRUE)
    })
    eval(base::parse(text=mkdt))
    DT <- rbind(DB1,DB2,DB3,DB4,DB5,DB6,DB7,DB8,DB9,DB10,DB11,DB12,DB13,DB14,DB15,DB16,DB17,DB18,DB19,DB20)
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
  z1$HHI_original <- ifelse(is.nan(z1$HHI),as.numeric(NA),as.numeric(z1$HHI))
  z1$HHI_Dum <- ifelse(z1$HHI < 0.1,1,0)
  DB <- transform(z1, STR_HHI = as.numeric(STR * HHI_Dum))
  #DBA <- DB[order(TSE_code,year,company)]
  return(DB)
}
#' #####
#+ function_winsorize, eval=TRUE, echo=TRUE, #!asia
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

winsamp1 <- function(x = 'TEJ81', col=c('ETR','CETR','ROA','SIZE','LEV','INTANG','QUICK','EQINC','OUTINSTI','RELAT')
                     , prob=0.01, na.rm=TRUE){
  x1 <- captureOutput(cat('DB1<-',x,sep="",fill=TRUE))
  x2 <- captureOutput(for(y in col){cat('DB1$',y,' <- winsor(',x,'$',y,',trim = ',prob,',na.rm = ',na.rm,')',sep="",fill = TRUE)})
  eval(base::parse(text=x1))
  eval(base::parse(text=x2))
  return(DB1)}

winsamp2 <- function(x = 'TEJ82', col=c('ETR','CETR','ROA','SIZE','LEV','INTANG','QUICK','EQINC','OUTINSTI','RELAT')
                     , prob=0.01){
  x1 <- captureOutput(cat('DB1<-',x,sep="",fill=TRUE))
  x2 <- captureOutput(for(y in col){cat('DB1$',y,' <- winsorized.sample(',x,'$',y,',prob = ',prob,')',sep="",fill = TRUE)})
  eval(base::parse(text=x1))
  eval(base::parse(text=x2))
  return(DB1)}
#winsamp1 <- cmpfun(winsamp1)
#winsamp2 <- cmpfun(winsamp2)


#' #####
#+ function_catchDB
catchDB <- function(x,col=c(company,market,TSE_code,TSE_name,year,
                            ETR,CETR,STR,HHI_Dum,STR_HHI,
                            ROA,SIZE,LEV,INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP,
                            RD,EMP,MB,MARKET,PPE)){
  y <- base::subset(x=x,select=col)
  return(y)}
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

