#' ##**Ready to run R**
#' <br>
#' 欲建立運行環境，請先至[R的網站](https://cran.r-project.org/mirrors.html)下載新版的R安裝。
#' 
#' >
#' 1. 使用<kbd>Ctrl+F</kbd>搜尋**Taiwan**，並任選一鏡像下載點，或直接[點此下載](http://cran.csie.ntu.edu.tw/)。
#' 2. 請選擇適合自己電腦運行介面的版本，R提供Linux, Mac及Windows三種版本。
#' 3. R支援多國語言，從哪個鏡像下載不影響安裝。
#' 4. 建議版本需**3.3.0**以後。
#' 
#' 再至[Rstudio官網](https://www.rstudio.com/)下載主程式安裝，或[點此](https://www.rstudio.com/products/rstudio/download/)至下載頁面。
#' <br>
#' Rstudio載點快速連結：(**0.99.902**版，於2016/7/28更新)
#' 
#' > 
#' 1. [Windows Vista/7/8/10](https://download1.rstudio.org/RStudio-0.99.902.exe "RStudio-0.99.902.exe")
#' 2. [Mac OS X 10.6+ (64-bit)](https://download1.rstudio.org/RStudio-0.99.902.dmg "RStudio-0.99.902.dmg")
#' 3. [Ubuntu 12.04+/Debian 8+ (64-bit)](https://download1.rstudio.org/rstudio-0.99.902-amd64.deb "rstudio-0.99.902-amd64.deb")
#' <br>
#' 
#' 安裝完成後，請確認Rstudio或RGui之語言及區域(language & locale)設定正確：
#+ locale, eval=FALSE
# 查看環境設定
sessionInfo()
# 查看語言/地區設定
Sys.getlocale(category = "LC_ALL")
# 若上述回傳非顯示相同值，請輸入下方設定 [正在解決這塊的問題]
Sys.setlocale("LC_ALL",locale='cht')


#' 其他疑難排解，請見[手冊](https://github.com/dspim/R/wiki/R-&-RStudio-Troubleshooting-Guide "R & RStudio Troubleshooting Guide")及[下方](#qa "Q&A")說明
#' <br>
#' <br>
#' 
#+ setwd, eval=FALSE
# 清除環境清單
rm(list=ls())
# 在Rstudio，設定工作資料夾(EX: D:\Documents\Dropbox\MyEssay\newRdode)
setwd("D:\\Documents\\Dropbox\\MyEssay\\newRdode")
wd <- getwd()

#' ##### 讀入函數設定
#+ setup, echo=FALSE, eval=TRUE, results="hide"
# Functions
# install all packages and load.
pack <- function(var=0){
# install.packages("compiler")
  library(compiler)
  
  for(pklist in c('readxl','xlsx','plyr','dplyr','knitr','data.table',
                'grid','gridExtra','ggplot2','zoo','R.oo','R.utils','psych',
                'robustHD','rbenchmark','foreign','rgl','stargazer','rmarkdown','DiagrammeR',
                'DiagrammeRsvg','magrittr','svglite','rsvg','png','xtable','latex2exp')){
  Install.pack <- cmpfun(function(lists=pklist){
    pklist <- lists
    new.packages <- pklist[!(pklist %in% installed.packages()[,"Package"])]
    if(length(new.packages)){install.packages(new.packages)}else{update.packages(pklist)}
  })
  Load.pack <- cmpfun(function(list=as.list(packtogo)){lapply(pklist, require, character.only = TRUE)})
  
  if(var==1){
    Install.pack() # 安裝
    Load.pack()} #掛載
  else{Load.pack()}
  #p.s. kable belongs to package:knitr 
  }}

pack(var=0) # var=1 refers to install package
