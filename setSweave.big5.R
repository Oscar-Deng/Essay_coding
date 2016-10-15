source("settings.R")
source("corstars.R")
readDB <- function(DB_fil = "TEJ1996.xlsx", attr_fil = "DB2.xlsx",
                   attr_sht = "TEJ_attr", xls_sht = "TEJ"){
  DBattr <- read_excel(attr_fil, sheet=attr_sht, col_names = TRUE)
  # read in excel database: DB2.xlsx, excel sheet: TEJ, with column names.
  DBori <- read_excel(DB_fil, sheet=xls_sht,
                      col_names = TRUE, col_types = DBattr$attr)
  # rename columns
  setnames(DBori,old=as.character(DBattr$old), new=as.character(DBattr$new))
  write.csv(na_count(DBori),file="原始資料集缺漏值.csv",row.names=TRUE)
  return(DBori)
}
DBfilter <- function(x = TEJ){
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
  # ----2001
  yr  <- c("2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")
  x.2001 <- x %>% filter(year %in% yr)
  x.dropped.ind.2001 <- x.dropped.ind %>% filter(year %in% yr)
  x.filt.ind.2001 <- x.filt.ind %>% filter(year %in% yr)
  y.na0.2001 <- x.filt.ind.2001 %>% 
    filter(FAMILY %in% NA | PB %in% NA | TA %in% NA | NetSales %in% c(NA,0) | employee %in% c(NA,0) | is.na(QUICK) | is.na(OUTINSTI))
  y.full.2001 <- x.filt.ind.2001 %>% 
    filter(!is.na(FAMILY) & !is.na(PB) & !is.na(TA) & !NetSales %in% c(NA,0) & !employee %in% c(NA,0) & !is.na(QUICK) & !is.na(OUTINSTI))
  y.full.big5.2001 <- y.full.2001 %>% group_by(TSE_code,year) %>% 
    filter(NROW(TSE_code) >= 5)
  y.full.small5.2001 <- y.full.2001 %>% group_by(TSE_code,year) %>% 
    filter(NROW(TSE_code) < 5)
  
  tbA1 <- data.frame(
    "intro" = c('2001~2015 original sample amount'
                ,'delete "financial ind."(TSE ind. code: M2800)'
                ,'delete "other ind."(TSE ind. code: M9900)'
                ,'delete "other electronic ind."(TSE ind. code: M2331)'
                ,'delete "DRs"(TSE ind. code: W91)'
                ,'delete empty ind.'
                ,'delete samples which had missing value, and will affect the research'
                ,'delete ind. which sample amount less then 5 firms in a year(list in filt.ind.csv)'
                ,'Total sample amount after filtering='),
    "samp.amount" = c(NROW(x.2001) # 原始樣本數
                      , -NROW(filter(x.dropped.ind.2001, TSE_code == "M2800")) # 刪除金融
                      , -NROW(filter(x.dropped.ind.2001, TSE_code == "M9900")) # 刪除其他
                      , -NROW(filter(x.dropped.ind.2001, TSE_code == "M2331")) # 刪除M2331其他電子
                      , -NROW(filter(x.dropped.ind.2001, TSE_code == "W91")) # 刪除W91存託憑證
                      , -NROW(filter(x.dropped.ind.2001, is.na(TSE_code))) # 刪除缺產業值
                      , -NROW(y.na0.2001) # 刪除其他缺漏
                      , -NROW(y.full.small5.2001) # 刪除家數不足5筆
                      , NROW(y.full.big5.2001)) # 全樣本合計
  )
  tbA2 <- data.frame(
    "intro" = c('1996~2015 original sample amount'
                ,'delete "financial ind."(TSE ind. code: M2800)'
                ,'delete "other ind."(TSE ind. code: M9900)'
                ,'delete "other electronic ind."(TSE ind. code: M2331)'
                ,'delete "DRs"(TSE ind. code: W91)'
                ,'delete empty ind.'
                ,'delete samples which had missing value, and will affect the research'
                ,'delete ind. which sample amount less then 5 firms in a year(list in filt.ind.csv)'
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
  y.full.small5.2001$TSE <- apply(y.full.small5.2001[,c("TSE_code","TSE_name")],1,paste,collapse=".")
  tab.ind.small <- as.data.frame.matrix(table(y.full.small5.2001$TSE,y.full.small5.2001$year))
  y.full.big5.2001$TSE <- apply(y.full.big5.2001[,c("TSE_code","TSE_name")],1,paste,collapse=".")
  tab.ind.big <- as.data.frame.matrix(table(y.full.big5.2001$TSE,y.full.big5.2001$year))
  # output:
  table.png(obj = tbA1, name = "樣本篩選表2001版", align = "llr")
  table.png(obj = tbA2, name = "樣本篩選表1996版", align = "llr")
  write.csv(tab.ind.big, "產業年度表(篩選後).csv")
  write.csv(tab.ind.small, "產業年度表(篩選掉的).csv")
  write.csv(na_count(y.na0),"其他闕漏值參考表.csv",row.names=TRUE)
  print("Print several files in folder, please confirm.")
  return(y.full.big5)
}
replaceNAby0 <- function(x,col=c()){ 
  x[,col] <- apply(x[,col],2,function(z){replace(z, is.na(z), 0)})
  return(x)
}
doNormalization <- function(x){(x-min(x))/(max(x)-min(x))}
norm_var <- function(x=TEJ1){
  return(
    x %>% mutate(
      ROA = as.numeric(PTEBX) / as.numeric(TA), # ROA : NetSales / TotalAssets
      SIZE = as.numeric(log(x = as.numeric(TA), base = exp(1))), # SIZE : ln(TA)
      LEV = as.numeric(TL) / as.numeric(TA), # LEV : TL / TA
      INTANG = as.numeric(INTAN) / as.numeric(TA), # INTANG : intangible assets / TA
      QUICK = ifelse(is.na(QUICK/100),0,as.numeric(QUICK/100)), # QUICK : = QUICK/100
      EQINC = as.numeric(-(InvIn + InvLoss)) / as.numeric(TA), # EQINC : (InvIn + InvLos) / -TA
      OUTINSTI = ifelse(is.na(OUTINSTI/100),0,as.numeric(OUTINSTI/100)), # OUTINSTI : = OUTINSTI/100
      RELATIN = ifelse(is.na(RELATIN/100),0,as.numeric(RELATIN/100)),
      RELATOUT = ifelse(is.na(RELATOUT/100),0,as.numeric(RELATOUT/100)),
      FAM_Dum = ifelse(FAMILY == 'F', 1, 0),
      CTP_IFRS = as.numeric(-(CTP_IFRS_CFI + CTP_IFRS_CFO + CTP_IFRS_CFF)),
      STR_RD = as.numeric(OERD) / as.numeric(NetSales),
      STR_EMP = as.numeric(employee) / as.numeric(NetSales),
      STR_MB = as.numeric(PB),
      STR_MARKET = as.numeric(OEPRO) / as.numeric(NetSales),
      STR_PPE = as.numeric( FA - Land - LandR ) / as.numeric(TA)
    ) %>%  mutate(CTP = ifelse(year >= 2012,CTP_IFRS,CTP_GAAP))
  )}
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
  )
  png(filename = "ETR與CETR分布圖(winsor前).png",width=200,height=200,units="mm",res=500)
  plot(TEJ4$ETR,TEJ4$CETR)
  dev.off()} # add up 5 years moving sum
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
             #,STR.rank = sum(STR_RD.perank,STR_EMP.perank,STR_MB.perank,STR_MARKET.perank,STR_PPE.perank,na.rm=TRUE)
      )
  )}
fnHHI <- function(x=TEJ6.2) {
  class(x) <- c("tbl_df","data.frame") # remove old key
  x1 <- x %>% arrange_(quote(year)) %>% group_by(year,TSE_code) %>%  
    mutate(NSsum = sum(NetSales,na.rm = TRUE)) %>%
    mutate(NSalpha2 = (NetSales/NSsum)^2) %>%
    mutate(HHIsum = sum(NSalpha2,na.rm = TRUE))
  class(x1) <- c("tbl_df","data.frame") # remove old key
  HHI.ind <- x1 %>% select(year,TSE_code,HHIsum) %>% distinct(year,TSE_code,.keep_all = TRUE)
  HHI.ind.rollmean <- HHI.ind %>% arrange(TSE_code,year) %>% group_by(TSE_code) %>% mutate(HHIsum.lag = lag(HHIsum,n=1)) %>% 
    mutate(HHI = rollapplyr(HHIsum.lag,5,FUN=function(x){mean(x,na.rm=TRUE)},partial=TRUE,fill=NA)) %>% select(year,TSE_code,HHI)
  #HHI.ind.rollmean[is.na(HHI.ind.rollmean)] <- 0 # don't do this!
  x2 <- left_join(x1,HHI.ind.rollmean,by=c("year","TSE_code"))
  x2$HHI_Dum <- ifelse(x2$HHI < 0.1,1,0)
  return(
    x2 %>% mutate(STR_HHI = as.numeric(STR * HHI_Dum))
  )
}
fnGDP <- function(x=TEJ7,file="DB2.xlsx",col_sht="GDP_colnames",DB_sht="GDP"){
  GDP_colname <- read_excel(file, sheet=col_sht)
  rGDP <- read_excel(file, sheet=DB_sht)
  setnames(rGDP, old=as.character(GDP_colname$old), new=as.character(GDP_colname$new))
  rGDP$year <- year(rGDP$Date)
  rGDP$GDP <- log(rGDP$Value,base=exp(1))
  GDP <- subset(rGDP,select=c(year,GDP))
  return(merge(x,GDP,by="year"))}
winsamp <- function(x = 'TEJ82', col=c('ETR','CETR','ROA','SIZE','LEV','INTANG','QUICK','EQINC','OUTINSTI','RELAT'), prob=0.01){
  winsorized.sample <- function (x, prob = 0.01) { # remove NA
    n <- length(x)
    n0 <- length(x[!is.na(x)])
    low <- floor(n0 * prob) + 1
    high <- n0 + 1 - low
    idx <- seq(1,n)
    DT <-data.frame(idx,x)
    DT2<-DT %>% arrange(x,idx)
    DT2$x[1:low]<-DT2$x[low]
    DT2$x[high:n0]<-DT2$x[high]
    DT3<-DT2[order(DT2$idx,DT2$x),]
    x2<-DT3$x
    return(x2)}
  x1 <- captureOutput(cat('DB1<-',x,sep="",fill=TRUE))
  x2 <- captureOutput(for(y in col){cat('DB1$',y,' <- winsorized.sample(',x,'$',y,',prob = ',prob,')',sep="",fill = TRUE)})
  #x2 <- captureOutput(for(y in col){cat('DB1$',y,' <- winsor(',x,'$',y,',trim = ',prob,',na.rm = ',na.rm,')',sep="",fill = TRUE)})
  # should add var in func: na.rm=TRUE
  eval(base::parse(text=x1))
  eval(base::parse(text=x2))
  return(DB1)}

TEJ8 <- readDB(DB_fil = "TEJ1996.xlsx", attr_fil = "DB2.xlsx", attr_sht = "TEJ_attr", xls_sht = "TEJ") %>%
  DBfilter() %>%
  replaceNAby0(col=c('OERD','OEPRO','Land','LandR','CTP_IFRS_CFI','CTP_IFRS_CFO','CTP_IFRS_CFF','CTP_GAAP','INTAN','RELATIN')) %>% norm_var() %>% dep_var() %>% STR() %>% 
  select(-c(market,TEJ_name2,TEJ_code2,TEJ_name1,TEJ_code1,CTP_GAAP,CTP_IFRS_CFI,CTP_IFRS_CFO,CTP_IFRS_CFF,STR_RD.lag,STR_EMP.lag,STR_MB.lag,STR_MARKET.lag,STR_PPE.lag)) %>%
  fnHHI() %>% fnGDP(file="DB2.xlsx",col_sht="GDP_colnames",DB_sht="GDP")

samp_col <- c('ETR','CETR','ROA','SIZE','LEV','INTANG','QUICK','EQINC')
TEJ8.1 <- winsamp(x="TEJ8",col=samp_col,prob = 0.005)
TEJ8.1$ETR[TEJ8.1$ETR <= 0] <- 0; TEJ8.1$CETR[TEJ8.1$CETR <=0] <- 0
rm(DBfilter,dep_var,doNormalization,fnGDP,fnHHI,norm_var,pack,readDB,replaceNAby0,STR,winsamp)
rm(samp_col)
write.csv(TEJ8.1,"TEJ8.1.csv",row.names = T)
# how to read TEJ8.1?
# read.csv("TEJ8.1.csv",row.names = 1)
