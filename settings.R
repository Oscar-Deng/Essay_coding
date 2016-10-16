
pack <- function(cmd=c("install","loadin")){
  # install.packages("compiler")
  library(compiler)
  
  for(pklist in c('readxl','xlsx','dplyr','data.table',"RcppRoll",'rgl',
                  #,'Hmisc','plyr','knitr','rmarkdown','robustHD','gdata',
                  #'ggplot2','magrittr','svglite','rsvg',
                  'grid','gridExtra','zoo','R.oo','R.utils','psych',
                  'foreign','stargazer','DiagrammeR',
                  'DiagrammeRsvg','png','xtable','latex2exp'
                  ,'tictoc')){
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
na_count <- function(x){
  if(NCOL(x)==1){sum(length(which(is.na(x))))}
  else{
    data.frame("Number.of.NAs"=sapply(x, function(y) sum(length(which(is.na(y))))))}}

require(compiler)
require(tools)
require(Hmisc)
table.png <- function(obj, name,align=align) { 
  first <- name
  name <- paste(name,".tex",sep="")
  sink(file=name)
  cat('
      \\documentclass{report}
      \\usepackage[paperwidth=5.5in,paperheight=7in,noheadfoot,margin=0in]{geometry}
      \\begin{document}\\pagestyle{empty}
      ')
  print(xtable(obj,align=align))
  cat('
      \\end{document}
      ')
  sink()
  texi2dvi(file=name)
  cmd <- paste("dvipng -T tight", shQuote(paste(first,".dvi",sep="")))
  
  invisible(sys(cmd))
  cleaner <- c(".tex",".aux",".log",".dvi")
  invisible(file.remove(paste(first,cleaner,sep="")))
}
