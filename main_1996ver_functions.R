#' ##### 設定讀入資料庫函數
#+ function_readDB
# readDB函數：讀入TEJ之excel檔，並用excel中之屬性表設定欄位
readDB <- cmpfun(function(DB_fil = "TEJ1996.xlsx", attr_fil = "DB2.xlsx", attr_sht = "TEJ_attr", xls_sht = "TEJ"){
  DBattr <- read_excel(attr_fil, sheet=attr_sht, col_names = TRUE)
  # read in excel database: DB2.xlsx, excel sheet: TEJ, with column names.
  DBori <- read_excel(DB_fil, sheet=xls_sht, col_names = TRUE, col_types = DBattr$attr)
  # rename columns
  setnames(DBori,old=as.character(DBattr$old), new=as.character(DBattr$new))
  return(DBori)
  
})
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

#' #####
#' 將特定欄位之變數缺漏值設為0
#+ function_NAto0
NAto0 <- cmpfun(function(x = 'TEJ01',col=c(NA)){
  x1 <- captureOutput(
    for(y in col){cat(x,'$',y,'[is.na(',x,'$',y,')] <- 0',sep="",fill = TRUE)})
  x2 <- captureOutput(cat('return(',paste(x),')',sep=""))
  xx <- c(x1,x2)
  eval(base::parse(text=xx))}) # replace NA with 0.
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
#+ add.TEJ31, eval=TRUE 
# add.RELATION ratio
fn_relation <- function(x=TEJ3){
  y <- transform(x,RELAT = RELATIN/RELATOUT)}

#' #####
#+ function_dep_var
dep_var <- function(x=TEJ3,k=5){
  DB01 <- x[,.SD[.N >= k],by=company]
  DB02 <- x[,.SD[.N < k],by=company]
  DB1 <- DB01[,`:=`(BTE5yrsum = rollapplyr(BTE, width = 5, FUN = sum, fill = NA),
                    CTP5yrsum = rollapplyr(CTP, width = 5, FUN = sum, fill = NA),
                    PTEBX5yrsum = rollapplyr(PTEBX, width = 5, FUN = sum, fill = NA)),
              by=company]
  DB2 <- DB02[,`:=`(BTE5yrsum = 0,CTP5yrsum = 0,PTEBX5yrsum = 0),by=company]
  DB3 <- rbind(DB1,DB2)
  DB <- transform(DB3,
                  ETR = as.numeric(BTE5yrsum) / as.numeric(PTEBX5yrsum),
                  CETR = as.numeric(CTP5yrsum) / as.numeric(PTEBX5yrsum))
  return(as.data.table(DB[order(DB$company,DB$year),]))} # add up 5 years moving sum

#' #####設定STR函數
#+ function_STR
STR <- function(x=TEJ4) {
  x <- x[order(x$company,x$year),]
  rollmn <- function(x) rollapplyr(x, width, function(x) mean(x, na.rm = TRUE), fill=NA)
  mkdt <- capture.output(for(i in 1:20){
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
  DT <- rbind(DB1,DB2,DB3,DB4,DB5,DB6,DB7,DB8,DB9,DB10,DB11,DB12,DB13,DB14,DB15,DB16,DB17,DB18,DB19,DB20)
  NAto0 <- function(x = 'DT',col=c('STR_RD_mean','STR_EMP_mean','STR_MB_mean','STR_MARKET_mean','STR_PPE_mean')){
    x1 <- captureOutput(
      for(y in col){cat(x,'$',y,'[is.nan(',x,'$',y,')] <- 0',sep="",fill = TRUE)})
    x2 <- captureOutput(cat('return(',paste(x),')',sep=""))
    xx <- c(x1,x2)
    eval(base::parse(text=xx))} # replace NA with 0.
  DT1 <- NAto0(x = 'DT',col=c('STR_RD_mean','STR_EMP_mean','STR_MB_mean','STR_MARKET_mean','STR_PPE_mean'))
  DBA <- as.data.table(DT1[order(DT1$company,DT1$year),])
  #  DBA <- DBA[(DBA$year %in% seq(2001,2015))]
  return(DBA)
}
#' #####設定STR排名函數
#+ function_STRrank
STRrank <- function(x=TEJ5){
  prank<-function(x) {ifelse(is.na(x),NA,rank(x,ties.method = 'min')/sum(!is.na(x)))} # STRATEGY ranktile.
  rankscore <- function(x) {ifelse(!is.finite(x) | x == 0,0,ifelse(x>0 & x<=0.2,1,ifelse(x>0.2 & x<=0.4,2,ifelse(x>0.4 & x<=0.6,3,ifelse(x>0.6 & x<=0.8,4,ifelse(x>0.8 & x<=1,5,NA))))))}
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
  DB2$STR_percent <- as.numeric(DB2$STR_RD_mean_rank) + as.numeric(DB2$STR_EMP_mean_rank) + as.numeric(DB2$STR_MB_mean_rank) + as.numeric(DB2$STR_MARKET_mean_rank) + as.numeric(DB2$STR_PPE_mean_rank)
  return(DB2)} # rank score function

#' #####
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
winsamp1 <- cmpfun(winsamp1)
winsamp2 <- cmpfun(winsamp2)

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
#' #####讀入MNC資料庫(TEJ)
#' 來源檔：TEJ
readMNC <- function(x="MNC.xlsx",DB='MNC',attr='MNC_attr'){
  MNCattr <- read_excel(x, sheet=attr, col_names = TRUE,col_types =as.vector(rep("text",3)))
  MNC <- read_excel(x, sheet=DB, col_names = TRUE)
  setnames(MNC,old=as.character(MNCattr$old), new=as.character(MNCattr$new))
  MNC$year <- year(MNC$date)
  return(MNC)
}
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

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex"),
                    tbtitle = ""){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "****", ifelse(p < .01, "*** ", ifelse(p < .05, "**  ", ifelse(p < .1, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = FALSE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  #Rnew <- cbind(Rnew[1:length(Rnew)-1])
  # edited to:
  Rnew <- cbind(Rnew[1:length(Rnew)])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(
      xtable(Rnew,
             digits=3,
             caption = tbtitle,
             table.placement = "h!",
             caption.placement = "top",
             add.to.row = list(list(2),
                               "\\hline \\multicolumn{14}{L{0.5cm}}{\\textbf{註: } 
                               a.變數定義同前表.}
                               b.****, ***, **, *, represents 0.1%, 1%, 5%, 10% significance.} \\\\")),
      type="html")
    else print(xtable(Rnew), type="latex") 
  }
  # from site: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
} 
