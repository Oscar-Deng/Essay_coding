# ----
# tables
# 樣本篩選表
#setwd("D:\\Documents\\Dropbox\\MyEssay\\Rcoding\\")

#require(gridExtra)
#require(grid)
#require(data.table)

#rm(list=ls())
#TEJ <- as.data.table(read.csv("TEJout.csv",header = TRUE,row.names = 1))
#TEJ01 <- read.csv("TEJ01out.csv",header = TRUE,row.names = 1)
#TEJ02 <- read.csv("TEJ02out.csv",header = TRUE,row.names = 1)
#TEJ102 <- read.csv("TEJ102out.csv",header = TRUE,row.names = 1)




nonNAs <- function(x) {
  as.vector(apply(x, 2, function(x) length(which(!is.na(x)))))
}

# ----
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
  png(filename="table1_樣本篩選表.png",width=125,height = 70,units="mm",res = 500)
  #grid.newpage()
  grid.draw(g1)
  dev.off()
  write.xlsx(tbA1,file="tables.xlsx",sheetName = "table1_樣本篩選表",col.names = TRUE,row.names = FALSE,showNA = FALSE,append = FALSE)
return(tbA1)
  }
tbA1 <- plottbA1()

plottbA2 <- function(Q){
  TEJ01$TSE <- paste(TEJ01$TSE_code,TEJ01$TSE_name,sep=" ")
  tbA2 <- as.data.frame.matrix(table(TEJ01$TSE,TEJ01$year))
  png(filename="table2.png",width=300,height = 200,units="mm",res = 500)
  grid.table(tbA2)
  dev.off()
  write.xlsx(tbA2,file="tables.xlsx",sheetName = "table2",col.names = TRUE,row.names = TRUE,showNA = FALSE,append = TRUE)
return(tbA2)
}
tbA2 <- plottbA2()

#ggplot(as.data.frame(table(tb2)), aes(x=year, y=Freq, fill=TSE)) + geom_bar(stat="identity")

# ----
plottbA3 <- function(){
  #fnmin <- function(x){apply(TEJ101[,6:21,with=FALSE],2,mean(x,na.rm=TRUE))}
  write(stargazer::stargazer(TEJ101,type = "html"),file="table3.html",append = TRUE)
  
}
tbA3 <- plottbA3()

plottbA4 <- function(){}
tbA4 <- plottbA4()

plottbA5 <- function(){}
tbA5 <- plottbA5()

plottbA6 <- function(){}
tbA6 <- plottbA6()

plottbA7 <- function(){}
tbA7 <- plottbA7()


