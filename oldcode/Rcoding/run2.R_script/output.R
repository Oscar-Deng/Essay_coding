# output datasets
outputcsv <- function(){
  cap1to7 <- capture.output(for(i in 1:7){
  cat('TEJ',i,"out<-paste(wd,\"/TEJ",i,"out.csv\",sep=\"\")",sep="",fill=TRUE)
  cat('write.csv(x=TEJ',i,',file=TEJ',i,'out)',sep="",fill=TRUE)
  })
  cap8to10add0 <- capture.output(for(i in c(0,8,9,10)){for(k in c(1,2)){
  cat('TEJ',i,k,"out<-paste(wd,\"/TEJ",i,k,"out.csv\",sep=\"\")",sep="",fill=TRUE)
  cat('write.csv(x=TEJ',i,k,',file=TEJ',i,k,'out)',sep="",fill=TRUE)
  }})
  eval(base::parse(text=cap1to7))
  eval(base::parse(text=cap8to10add0))
  
}
write.csv(x=TEJ,file=paste(wd,"/TEJout.csv",sep=""))


# output SPSS
outputSPSS <- function(){
  spssfile <- paste(wd, "/SPSSout.txt",sep="")
  spssset <- paste(wd, "/SPSSout.sps",sep="")
  write.foreign(TEJ1, spssfile, spssset, package="SPSS")
  }

# output SAS
outputSAS <- function(){
  sasfile <- paste(wd, "/SASout.txt",sep="")
  sasset <- paste(wd, "/SASout.sas",sep="")
  write.foreign(TEJ1, sasfile, sasset, package="SAS")
  }

# export data frame to Stata binary format 
outputSTATA <- function(){
  dtafile <- paste(wd, "/STATAout.dta",sep="")
  write.dta(TEJ1, dtafile)
  }

############# use in practice ################

# a list of count for market and year.
#table(TEJ$market,TEJ$year,useNA = 'ifany') -> table_markyear
# a list of count for TSE and year.
#table(TEJ$TSE_name,TEJ$year,useNA = 'ifany') -> table_TSEyear
#grid.table()



