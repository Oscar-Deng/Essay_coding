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

#' 運行MNC函數
#+ load_readMNC
MNC <- readMNC()
summary(MNC)
#' 列出國外營運公司之所在地統計
MNC_nation <- as.data.frame(table(MNC$nation))
write.csv(MNC_nation,file="MNC_nation.csv")
#' 
#+
TEJ111 <- fnMNC(x=TEJ101,y=MNC,feedback='x')
TEJ112 <- fnMNC(x=TEJ102,y=MNC,feedback='x')
TEJ113 <- fnMNC(x=TEJ103,y=MNC,feedback='x')
TEJ114 <- fnMNC(x=TEJ104,y=MNC,feedback='x')



TEJ101 <- read.csv(file="TEJ101.csv")
TEJ101_fill <- read.csv(file="TEJ101_fill.csv")
#+ linear models, eval=TRUE, echo=TRUE
# fix RELATIN/RELATOUT = NaN, Inf, NA (for modeling can't include thee)

#TEJ_lm101 <- replace(TEJ101,TEJ101$RELAT[is.na(TEJ101$RELAT)|is.nan(TEJ101$RELAT)],0)
# instead!!!
TEJ_lm101 <- TEJ101_fill
TEJ_lm101[!is.finite(TEJ_lm101$RELAT),]$RELAT <- 0
###
#TEJ_lm102 <- replace(TEJ102,TEJ102$RELAT[!is.finite(TEJ102$RELAT)],0)
#TEJ_lm103 <- replace(TEJ103_fill,TEJ103_fill$RELAT[!is.finite(TEJ103_fill$RELAT)],0)
#TEJ_lm104 <- replace(TEJ104,TEJ104$RELAT[!is.finite(TEJ104$RELAT)],0)
# without STR*HHI
ETR_lmodel101_noSH <- lm(ETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm101)
CETR_lmodel101_noSH <- lm(CETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm101)
#
#ETR_lmodel102 <- lm(ETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm102)
#CETR_lmodel102 <- lm(CETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm102)
#
#ETR_lmodel103 <- lm(ETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm103)
#CETR_lmodel103 <- lm(CETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm103)
#
#ETR_lmodel104 <- lm(ETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm104)
#CETR_lmodel104 <- lm(CETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm104)

#ETR_lm_MNC <- lm(ETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP+MNC,TEJ_lm)
#CETR_lm_MNC <- lm(CETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP+MNC,TEJ_lm)

# with STR*HHI
ETR_lmodel101_SH <- lm(ETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm101)
CETR_lmodel101_SH <- lm(CETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm101)

#ETR_lm_MNC2 <- lm(ETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP+MNC,TEJ_lm)
#CETR_lm_MNC2 <- lm(CETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP+MNC,TEJ_lm)


#' ###報表輸出
#' ####一、樣本篩選量表
#' #####表一、
#' function_plottbA1
#' still some problems to fix!!! -*solving*-
plottbA1 <- function(){
  x <- nrow(TEJ)
  x1 <- nrow(TEJ[TEJ$TSE_code=='M2800',])-6
  x2 <- nrow(TEJ[TEJ$TSE_code=='M9900',])-6
  x3 <- nrow(TEJ[TEJ$TSE_code=='M2331',])-6
  x4 <- nrow(TEJ[TEJ$TSE_code=='W91',])-6
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
    "說明" = c('2001~2015 原始樣本總數','刪除金融保險業(TSE產業代碼 M2800)','刪除其他產業(TSE產業代碼 M9900)',
            '刪除其他電子業(TSE產業代碼 M2331)','刪除存託憑證(TSE產業代碼 W91)','刪除當年度產業內公司家數不足5筆之樣本',
            '刪除有缺漏值且足以影響分析之樣本','全樣本合計'),
    "樣本數" = c(x, x1, x2, x3, x4, x6, x8, x9),
    "小計" = c(x, "-","-","-",
           ifelse(x1+x2+x3+x4 == x-x5,x-x5,'error'),
           "-",x6+x8, ifelse(x5-x6-x8 == x9,x9,'error'))
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
  
  m <- format(tbA1, digits = 1, scientific=FALSE ,big.mark = ",")
  g1 <- tableGrob(m, theme = theme1, rows=9)
  png(filename="tbA1_篩選表.png",width=125,height = 70,units="mm",res = 500)
  grid.draw(g1)
  dev.off()
  # write.xlsx(tbA1,file="tables.xlsx",sheetName = "table1",col.names = TRUE,row.names = FALSE,showNA = FALSE,append = FALSE)
  return(tbA1)
}
#' 運行plottbA1
#+ load_plottbA1, fig.width=5, fig.height=5, dpi=500
plottbA1()

#' #####表X、
#' plottbA2
#+ function_plottbA2
#' still some problems to fix!!! -*solving*-
plottbA2 <- function(){
#  tbTEJ4 <- table(TEJ4$TSE_code,TEJ4$year)
  TEJ101$TSE <- paste(TEJ101$TSE_code,TEJ101$TSE_name,sep=" ")
  tbA2 <- as.data.frame.matrix(table(TEJ101$TSE,TEJ101$year))
  tbA2 <- cbind(tbA2, '小計'=rowSums(tbA2,na.rm = TRUE))
  tbA2 <- rbind(tbA2, '合計'=colSums(tbA2,na.rm = TRUE))
  png(filename="tbA2_公司家數統計.png",width=300,height = 200,units="mm",res = 500)
  grid.table(tbA2)
  dev.off()
  # write.xlsx(tbA2,file="tables.xlsx",sheetName = "table2",col.names = TRUE,row.names = TRUE,showNA = FALSE,append = TRUE)
  return(tbA2)
}
#' 運行plottbA2
#+ load_plottbA2, fig.width=10, fig.height=10, dpi=500
plottbA2()


#' ####二、敘述統計表
#' #####表X、
#' plottbA3
#' problems to be fixed!! 
#+ function_plottbA3
plottbA3 <- function(){
  #fnmin <- function(x){apply(TEJ101[,6:21,with=FALSE],2,mean(x,na.rm=TRUE))}
  DT <- base::subset(TEJ101_fill,select=c(ETR,CETR,STR,HHI_Dum,STR_HHI,
                                     ROA,SIZE,LEV,INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP,
                                     RD,EMP,MB,MARKET,PPE))
  write(
    stargazer::stargazer(DT,type = "html"
                         ,summary.stat = c("n","sd","min","p25","median","p75","max","mean")
                         ,column.labels=c("數量","標準差","最小值","第一四分位距","中位數","第三四分位距","最大值","平均數")
                         #,column.separate = rep(1,8)
                         ,table.placement = "h!"
                         ,title = "敘述統計表"
                         ,notes.append = TRUE
                         ,notes = "1.應變數ETR及CETR因第一年不計入，故樣本數較少。
                         2.STRATEGY變數將缺漏值補0。
                         3.其他變數定義請參考前表。"),
        file="tbA3_敘述統計表.html",append = FALSE)
#  write(stargazer::stargazer(TEJ101,type = "latex"),file="table3.pdf",append = FALSE)
}
#' 運行plottbA3
#+ load_plottbA3
plottbA3()


#' #####表X、
#' plottbA4
#+ function_plottbA4
plottbA4 <- function(){
  tbA4_ETR <- base::subset(TEJ101,select=c(ETR,STR,HHI,ROA,SIZE,LEV,INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP
                                          # ,RD,EMP,MB,MARKET,PPE
                                           ))
  tbA4_CETR <- base::subset(TEJ101,select=c(CETR,STR,HHI,ROA,SIZE,LEV,INTANG,QUICK,EQINC,OUTINSTI,RELAT,FAM_Dum,GDP
                                          #  ,RD,EMP,MB,MARKET,PPE
                                            ))
  tbA4_ETR$RELAT[which(!is.finite(tbA4_ETR$RELAT))] <- 1000
  tbA4_CETR$RELAT[which(!is.finite(tbA4_CETR$RELAT))] <- 1000
# write in tables
  write(corstars(tbA4_ETR,method = 'pearson',removeTriangle = 'lower'
                 #,result = 'html'
                 ,tbtitle = "Table?.? 各變數之Pearson相關係數表：應變數為ETR"),
    file="correlation_ETR.html",append=FALSE)
  
  write(corstars(tbA4_CETR,method = 'pearson',removeTriangle = 'lower',result = 'html'
                 ,tbtitle = "Table?.? 各變數之Pearson相關係數表：應變數為CETR"),
    file="correlation_CETR.html",append=FALSE)
  }
#' 運行plottbA4
#+ load_plottbA4
plottbA4()

#' #####表X、
#' plottbA5
#+ function_plottbA5
plottbA5 <- function(){
  TEJ101$TSE <- paste(TEJ101$TSE_code,TEJ101$TSE_name,sep="")
  HHI_DB <- base::subset(TEJ101, select=c(TSE,year,HHI)) %>% distinct
  # 高寡佔I 型≧0.3＞高寡佔II 型≧0.18＞低寡占I 型≧0.14＞低寡占II 型≧0.1＞競爭I 型≧0.05＞競爭II 型
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI >= 0.3,'高寡佔I 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.3 & HHI_DB$HHI >= 0.18,'高寡佔II 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.18 & HHI_DB$HHI >= 0.14,'低寡占I 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.14 & HHI_DB$HHI >= 0.1,'低寡占II 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.1 & HHI_DB$HHI >= 0.05,'競爭I 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,HHI_DB$HHI < 0.05,'競爭II 型')
  HHI_DB$HHI <- replace(HHI_DB$HHI,is.na(HHI_DB$HHI),'-')
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
plottbA5()

#' ####三、相關係數分析
#' #####plot
plottbB1 <- function(){
  gaze1 <- function(){
    stargazer::stargazer(ETR_lmodel101_noSH,CETR_lmodel101_noSH,
                         type='html',
                         style='default',
                         align=TRUE,
                         column.labels = c("TAXAVO_{it}=ETR_{it}","TAXAVO_{it}=CashETR_{it}"),
                         digits=3,
                         dep.var.labels.include=TRUE,
                         dep.var.caption="$$\\textit{TAXAVO_{it} = β_{0} + β_{1}STRATEGY_{it}
                         + β_{2}HHI_{jt} + β_{3}ROA_{it} + β_{4}SIZE_{it} + β_{5}LEV_{it} \n
                         #   + β_{6}INTANG_{it} + β_{7}QUICK_{it} + β_{8}EQINC_{it} + β_{9}OUTINSTI_{it}
                         + β_{10}RELATION_{it} + β_{11}$$\text{FAM_Dum}$$_{it} + β_{12}GDP_{it} + $\varepsilon$_{it} + }$$",
                         
                         ci=TRUE,
                         ci.level=0.99,
                         single.row=TRUE,
                         notes.append=TRUE,
                         title="實證結果─不包含STRATEGY×HHI",
                         notes.align='l',
                         
                         notes = "變數定義同表4-1",
                         out='tbB1.html')
}
  gaze1()
# formulaA <- TeX("$$\\textit{TAXAVO_{it} = β_{0} + β_{1}STRATEGY_{it}
#                 + β_{2}HHI_{jt} + β_{3}ROA_{it} + β_{4}SIZE_{it} + β_{5}LEV_{it}
#                 + β_{6}INTANG_{it} + β_{7}QUICK_{it} + β_{8}EQINC_{it} + β_{9}OUTINSTI_{it}
#                 + β_{10}RELATION_{it} + β_{11}$$\text{FAM_Dum}$$_{it} + β_{12}GDP_{it} + $\varepsilon$_{it} + }$$")
  stargazer::stargazer(ETR_lmodel101_SH,CETR_lmodel101_SH,
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
#  write(
#    xtable(ETR_lmodel),
#    #  xtable(CETR_lmodel,align='r'),
#    file='tbB1_1.html',
#    append = FALSE
#  )
}
plottbB1()
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
