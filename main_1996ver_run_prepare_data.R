# -*- encoding = UTF-8 -*-
# should run under utf-8 environment!
# preparing data to develop linear nodels
#* This part uses DB from year 1996 to 2015, in order to complement the 
#* deficiency of 5yr moving average's lacking data in year 2001.

  # read in DB with attributes and datas
  TEJ <- readDB(DB_fil = "TEJ1996.xlsx", attr_fil = "DB2.xlsx", attr_sht = "TEJ_attr", xls_sht = "TEJ")

  TEJ01 <- DBfilter(x = TEJ,filt = 'filtered')
  tb <- as.data.frame.matrix(table(TEJ01$TSE_code,TEJ01$year))
  write.csv(tb,file="tableofTSE_originalTEJ01.csv")
  TEJ02 <- DBfilter(x = TEJ, filt = 'dropped')
  TEJ1 <- NAto0(x ='TEJ01',col=c('OERD','OEPRO','Land','LandR','CTP_IFRS_CFI','CTP_IFRS_CFO','CTP_IFRS_CFF','CTP_GAAP'))
  TEJ2 <- control_var(x=TEJ1)
  TEJ3 <- exp_var_STR(x=TEJ2)
  TEJ3 <- fn_relation(TEJ3)
  TEJ4 <- dep_var(TEJ3,k=5)
  #STR <- cmpfun(STR)
  TEJ5 <- STR(TEJ4)
  TEJ6 <- STRrank(TEJ5)
  TEJ7 <- fnHHI(TEJ6)
  samp_col <- c('ETR','CETR','ROA','SIZE','LEV','INTANG','QUICK','EQINC','OUTINSTI','RELATIN','RELATOUT')
  #TEJ81 <- TEJ7
  #TEJ81 <- winsamp1(x='TEJ81', col = samp_col, prob = 0.01, na.rm=TRUE)
  #TEJ82 <- TEJ7
  #TEJ82 <- winsamp2(x='TEJ82', col = samp_col, prob = 0.01)
  TEJ101 <- fnGDP(x=TEJ7,file="DB2.xlsx",col_sht="GDP_colnames",DB_sht="GDP")
  #TEJ102 <- fnGDP(x=TEJ82,file="DB2.xlsx",col_sht="GDP_colnames",DB_sht="GDP")
  TEJ101_fill <- TEJ101[(TEJ101$year %in% seq(2001,2015))]
  #TEJ102_fill <- TEJ102[(TEJ102$year %in% seq(2001,2015))]
  NewTEJ101 <- winsamp1(x="TEJ101",col=c("ETR","CETR","ROA","SIZE","LEV","INTANG","QUICK",
                                         "EQINC","OUTINSTI","RELAT","HHI"
  ),prob=0.01,na.rm=TRUE)


write.csv(TEJ101,file="TEJ101.csv")
write.csv(TEJ101_fill,file="TEJ101_fill.csv")


main2010 <- function(){
  -----#####-----
    TEJ01_2010 <- TEJ01[(TEJ01$year %in% seq(2001,2010))]
    TEJ02_2010 <- TEJ02[(TEJ02$year %in% seq(2001,2010))]
    TEJ1_2010 <- NAto0(x ='TEJ01_2010',col=c('OERD','OEPRO','Land','LandR','CTP_IFRS_CFI','CTP_IFRS_CFO','CTP_IFRS_CFF','CTP_GAAP'))
    TEJ2_2010 <- control_var(x=TEJ1_2010)
    TEJ3_2010 <- exp_var_STR(x=TEJ2_2010)
    TEJ3_2010 <- fn_relation(TEJ3_2010)
    TEJ4_2010 <- dep_var(TEJ3_2010,k=5)
    TEJ5_2010 <- STR(TEJ4_2010)
    TEJ6_2010 <- STRrank(TEJ5_2010)
    TEJ7_2010 <- fnHHI(TEJ6_2010)
    TEJ101_2010 <- TEJ101[(TEJ101$year %in% seq(2001,2010))]
    TEJ102_2010 <- TEJ102[(TEJ102$year %in% seq(2001,2010))]
    
    
}
