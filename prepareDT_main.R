# pack function
# ----
source("main_1loadpack.R")

source("main_2funcA.R")
source("main_2funcB.R")

doNormalization <- function(x){(x-mean(x,na.rm = T))/sd(x,na.rm = T)}
rm(fnGDP,fnHHI,pack,readDB,STR,table.png,corstars2,DBfilter,dep_var,replaceNAby0)

library(car)
