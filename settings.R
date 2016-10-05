# 查看環境設定
#sessionInfo()
# 查看語言/地區設定
#Sys.getlocale(category = "LC_ALL")
# 若上述回傳非顯示相同值，請輸入下方設定 [正在解決這塊的問題]
#Sys.setlocale("LC_ALL",locale='cht')

#+ setwd, eval=FALSE
# 清除環境清單
#rm(list=ls())
# win10, 在Rstudio，設定工作資料夾(EX: D:\Documents\Dropbox\MyEssay\Essay_coding)
#setwd("D:\\Documents\\Dropbox\\MyEssay\\Essay_coding")
# for ubuntu ed.
#setwd("Essay_coding")
wd <- getwd()

#' ##### 讀入函數設定
#+ setup, echo=FALSE, eval=TRUE, results="hide"
# Functions
# install all packages and load.
pack <- function(cmd=c("install","loadin")){
  # install.packages("compiler")
  library(compiler)
  
  for(pklist in c('readxl','xlsx','plyr','dplyr','knitr','data.table',
                  'grid','gridExtra','ggplot2','zoo','R.oo','R.utils','psych',
                  'robustHD','foreign','rgl','stargazer','rmarkdown','DiagrammeR',
                  'DiagrammeRsvg','magrittr','svglite','rsvg','png','xtable','latex2exp','Hmisc')){
    Install.pack <- cmpfun(function(lists=pklist){
      pklist <- lists
      new.packages <- pklist[!(pklist %in% installed.packages()[,"Package"])]
      if(length(new.packages)){install.packages(new.packages)}else{update.packages(pklist)}
    })
    Load.pack <- cmpfun(function(list=as.list(packtogo)){lapply(pklist, library, character.only = TRUE)})
    if(cmd == "install"){
      Install.pack()
      Load.pack()
      }
    if(cmd == "loadin"){
      Load.pack()
      }
    #p.s. kable belongs to package:knitr 
  }}

pack("loadin") # load package
#pack("install") # install and load packages!
