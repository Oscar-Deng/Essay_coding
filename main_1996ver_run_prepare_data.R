# -*- encoding = UTF-8 -*-
# should run under utf-8 environment!
# preparing data to develop linear nodels
#* This part uses DB from year 1996 to 2015, in order to complement the 
#* deficiency of 5yr moving average's lacking data in year 2001.
# for testing read in x.csv
x <- read.csv("x.csv",row.names = 1)

  # read in DB with attributes and datas
  TEJ <- readDB(DB_fil = "TEJ1996.xlsx", attr_fil = "DB2.xlsx", attr_sht = "TEJ_attr", xls_sht = "TEJ")
  TEJ1 <- DBfilter(x = TEJ,out="txt")
  TEJ2 <- replaceNAby0(x=TEJ1,col=c('OERD','OEPRO','Land','LandR','CTP_IFRS_CFI','CTP_IFRS_CFO','CTP_IFRS_CFF','CTP_GAAP','INTAN','RELATIN'))
  TEJ3 <- norm_var(x=TEJ2)
  TEJ4 <- dep_var(TEJ3)
  #TEJ5 <- STR(TEJ4)
  #TEJ6 <- STRrank(TEJ5)
  #TEJ6.1 <- TEJ6 %>% select(-c(STR_RD.lag,STR_EMP.lag,STR_MB.lag,STR_MARKET.lag,STR_PPE.lag))
  TEJ6 <- STR(TEJ4)
  TEJ6.2 <- TEJ6 %>% select(-c(market,TEJ_name2,TEJ_code2,TEJ_name1,TEJ_code1,CTP_GAAP,CTP_IFRS_CFI,CTP_IFRS_CFO,CTP_IFRS_CFF
                               ,STR_RD.lag,STR_EMP.lag,STR_MB.lag,STR_MARKET.lag,STR_PPE.lag))
  TEJ7 <- fnHHI(TEJ6.2)
  samp_col <- c('ETR','CETR','ROA','SIZE','LEV','INTANG','QUICK','EQINC')
  TEJ8 <- fnGDP(x=TEJ7,file="DB2.xlsx",col_sht="GDP_colnames",DB_sht="GDP")
  # used function merge, so data before 2000 are cleaned
  TEJ8.1 <- winsamp(x="TEJ8",col=samp_col,prob = 0.005)
  TEJ8.1$ETR[TEJ8.1$ETR <= 0] <- 0; TEJ8.1$CETR[TEJ8.1$CETR <=0] <- 0
  plottbA3(TEJ8.1,name = "tbA3_敘述統計表.html")
  #plot
pdf(file = "ETR與CETR分布圖(winsor前後).pdf",paper="a4")
  qplot(ETR,CETR,data=TEJ8);qplot(ETR,CETR,data=TEJ8.1);dev.off()
pdf(file = "ETR長條圖(winsor後).pdf",paper="a4")
  histogram(TEJ8.1$ETR); histogram(TEJ8.1$CETR); histogram(TEJ8.1$ROA); histogram(TEJ8.1$SIZE)
  histogram(TEJ8.1$LEV); histogram(TEJ8.1$INTANG); histogram(TEJ8.1$QUICK); histogram(TEJ8.1$EQINC)
  histogram(TEJ8.1$OUTINSTI); histogram(TEJ8.1$RELAT);histogram(TEJ8.1$RD); histogram(TEJ8.1$EMP)
  histogram(TEJ8.1$MB); histogram(TEJ8.1$MARKET); histogram(TEJ8.1$PPE);dev.off()
  # plot end
  
  TEJ101 <- TEJ101[(TEJ101$year %in% seq(2001,2015))]
  #TEJ102_fill <- TEJ102[(TEJ102$year %in% seq(2001,2015))]
  NewTEJ101 <- winsamp1(x="TEJ101",col=c("ETR","CETR","ROA","SIZE","LEV","INTANG","QUICK",
                                         "EQINC","OUTINSTI","RELAT","HHI","STR"
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
