# for R GUI to run all codes

# set directory to path
setwd("D:\\Documents\\Dropbox\\MyEssay\\Rcoding\\")

# run code : run.R
source("run.R")

# run codes : depVar.R -- dependent variables
source("depVar.R")

# run codes : expVar_01STR.R -- explanatory variables
# strategy : RD & EMP & MB & MARKET & PPE
source("expVar_01STR.R")

# run codes : expVar_02HHI.R -- explanatory variables
# HHI : SumSales = Q, Sales/Q = alpha, HHI=sum [t-1:t-5] alpha / 5
source("expVar_02HHI.R")

# see attributes <rownames & colnames>
attributes(rawTEJ)

# summary
Info <- summary(rawTEJ)

