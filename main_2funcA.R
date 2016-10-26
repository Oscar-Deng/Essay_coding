na_count <- function(x){
  if(NCOL(x)==1){sum(length(which(is.na(x))))}
  else{
    data.frame("Number.of.NAs"=sapply(x, function(y) sum(length(which(is.na(y))))))}}


# ----
# table.png
# ---- 
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

source("corstars.R")
