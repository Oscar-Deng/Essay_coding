#' ###報表輸出
#' ####一、樣本篩選量表
#' #####表一、
#' function_plottbA1

plot_filtering <- function(DB=TEJ,file="表1.1樣本篩選表.png"){
  tbA1 <- data.frame(
    "說明" = c('2001~2015 原始樣本總數','刪除金融保險業(TSE產業代碼 M2800)','刪除其他產業(TSE產業代碼 M9900)',
             '刪除其他電子業(TSE產業代碼 M2331)','刪除存託憑證(TSE產業代碼 W91)','刪除當年度產業內公司家數不足5筆之樣本',
             '刪除有缺漏值且足以影響分析之樣本','全樣本合計'),
    "樣本數" = c(),
    "小計" = c())
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
  png(filename=file,width=125,height = 70,units="mm",res = 500)
  grid.draw(g1)
  dev.off()
}
#' 運行plottbA1
#+ load_plottbA1, fig.width=5, fig.height=5, dpi=500
plot_filtering()
#' #####plot
plottbB1 <- function(x){
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
plottbB2 <- function(){
  write(stargazer(ETR_lmodel101_SH,CETR_lmodel101_SH,summary=TRUE,type="text",single.row=FALSE,report="vcstp*"),file="empirical_SH.txt")
  write(stargazer(ETR_lmodel101_noSH,CETR_lmodel101_noSH,summary=TRUE,type="text",single.row=FALSE,report="vcstp*"),file="empirical_noSH.txt")
  write(stargazer(ETR_lmodel101_SH,CETR_lmodel101_SH,summary=TRUE,type="html",single.row=FALSE,report="vcstp*"),file="empirical_SH.html")
  write(stargazer(ETR_lmodel101_noSH,CETR_lmodel101_noSH,summary=TRUE,type="html",single.row=FALSE,report="vcstp*"),file="empirical_noSH.html")
  
  write(stargazer(ETR_lmodelNew101_SH,CETR_lmodel101_SH,summary=TRUE,type="text",single.row=FALSE,report="vcstp*"),file="empirical_SH.txt")
  write(stargazer(ETR_lmodelNew101_noSH,CETR_lmodel101_noSH,summary=TRUE,type="text",single.row=FALSE,report="vcstp*"),file="empirical_noSH.txt")
  write(stargazer(ETR_lmodelNew101_SH,CETR_lmodel101_SH,summary=TRUE,type="html",single.row=FALSE,report="vcstp*"),file="empirical_SH.html")
  write(stargazer(ETR_lmodelNew101_noSH,CETR_lmodel101_noSH,summary=TRUE,type="html",single.row=FALSE,report="vcstp*"),file="empirical_noSH.html")
  
  
}
tbB2 <- plottbB2()
#' #####plot
plottbB3 <- function(){}
tbB3 <- plottbB3()
#' #####plot
plottbB4 <- function(){}
tbB4 <- plottbB4()

#' ####四、實證分析表



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
