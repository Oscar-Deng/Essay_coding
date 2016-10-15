# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex"),star=c(3,4)){
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  if(star == 4){
  mystars <- ifelse(p < .001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
  }else {
    mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "*  ", "   ")))
  }
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]

  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = FALSE)] <- ""
    # set diag to true if don't want "1"
    Rnew <- as.data.frame(Rnew)
  } else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = FALSE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## if want to remove last column, add "-1" after length()
  # return the correlation matrix
  #Rnew <- cbind(Rnew[1:length(Rnew)])
  if (result[1]=="none")
    return(Rnew)
  else{ if(result[1]=="html")
    #print(xtable(Rnew), type="html")
    stargazer(Rnew,summary=F,type="html",notes = )
      else{ if(result[1]=="text") 
        stargazer(Rnew,summary=F,type="text")
      else{
        #stargazer(Rnew,summary=F,type="latex")
        xtable(Rnew) 
        }}}
} 
