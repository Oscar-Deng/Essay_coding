corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                   # result=c("none", "html", "latex"),
                    tbtitle = ""){
  #Compute correlation matrix
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "****", ifelse(p < .01, "*** ", ifelse(p < .05, "**  ", ifelse(p < .1, "*   ", "    "))))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 3))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = FALSE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = FALSE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
# Rnew <- cbind(Rnew[1:length(Rnew)])
#  if (result[1]=="none") return(Rnew)
#  else{
#    if(result[1]=="html") 
      comment <- list()
      comment$pos <- list()
      comment$pos[[1]] <- c(nrow(Rnew))
      comment$command <- c(
paste("\\hline \\multicolumn{14}{L{0.5cm}}{\\textbf{註: }a.變數定義同前表。
                                b.****, ***, **, *, 分別代表 0.1%, 1%, 5%, 10% 顯著水準。} \\\\"))
      print(xtable(Rnew,
             digits=3,
             caption = tbtitle
             #,type="html"
             ),
            type = "html",
            add.to.row = comment,
            table.placement = "h!",
            hline.after = c(-1,0),
            caption.placement = 'top')
 #     else print(xtable(Rnew), type="latex") 
 #    }
  # from site: http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
}
