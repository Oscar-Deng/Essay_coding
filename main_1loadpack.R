pack <- function(cmd=c("install","loadin")){
  require(compiler)
  require(tools)
  require(Hmisc)
  
  for(pklist in c('readxl','xlsx','dplyr','data.table',"RcppRoll",'rgl',"ellipse",
                  'plyr','knitr','rmarkdown',#'Hmisc',
                  'ggplot2','magrittr','svglite','rsvg','robustHD','gdata',
                  'grid','gridExtra','zoo','R.oo','R.utils','psych',
                  'foreign','stargazer','DiagrammeR','corrplot',
                  'DiagrammeRsvg','png','xtable','latex2exp')){
    if(cmd == "install"){
      new.packages <- pklist[!(pklist %in% installed.packages()[,"Package"])]
      if(length(new.packages)){install.packages(new.packages)
        }else{update.packages(pklist)}
      lapply(pklist, library, character.only = TRUE)
    }
    if(cmd == "loadin"){
      lapply(pklist, require, character.only = TRUE)
    }
    #p.s. kable belongs to package:knitr 
  }
}



pack("loadin") # load package
pack("install") # install and load packages!
