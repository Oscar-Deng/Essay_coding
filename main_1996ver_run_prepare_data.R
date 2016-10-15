# -*- encoding = UTF-8 -*-
# should run under utf-8 environment!
# preparing data to develop linear nodels
#* This part uses DB from year 1996 to 2015, in order to complement the 
#* deficiency of 5yr moving average's lacking data in year 2001.
# for testing read in x.csv
x <- read.csv("x.csv",row.names = 1)

  
pdf(file = "ETR與CETR分布圖(winsor前後).pdf",paper="a4")
  qplot(ETR,CETR,data=TEJ8);qplot(ETR,CETR,data=TEJ8.1);dev.off()
pdf(file = "ETR長條圖(winsor後).pdf",paper="a4")
  histogram(TEJ8.1$ETR); histogram(TEJ8.1$CETR); histogram(TEJ8.1$ROA); histogram(TEJ8.1$SIZE)
  histogram(TEJ8.1$LEV); histogram(TEJ8.1$INTANG); histogram(TEJ8.1$QUICK); histogram(TEJ8.1$EQINC)
  histogram(TEJ8.1$OUTINSTI); histogram(TEJ8.1$RELAT);histogram(TEJ8.1$RD); histogram(TEJ8.1$EMP)
  histogram(TEJ8.1$MB); histogram(TEJ8.1$MARKET); histogram(TEJ8.1$PPE);dev.off()
  # plot end
  
  TEJ101 <- TEJ101[(TEJ101$year %in% seq(2001,2015))]

  
write.csv(TEJ101,file="TEJ101.csv")
write.csv(TEJ101_fill,file="TEJ101_fill.csv")

