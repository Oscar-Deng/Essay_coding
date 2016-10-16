#' #####讀入MNC資料庫(TEJ)
#' 來源檔：TEJ
readMNC <- function(x="MNC.xlsx",DB='MNC',attr='MNC_attr'){
  MNCattr <- read_excel(x, sheet=attr, col_names = TRUE,col_types =as.vector(rep("text",3)))
  MNC <- read_excel(x, sheet=DB, col_names = TRUE)
  setnames(MNC,old=as.character(MNCattr$old), new=as.character(MNCattr$new))
  MNC$year <- year(MNC$date)
  return(MNC)
}
#' #####計算各公司MNC變數
#' 
fnMNC <- function(x=TEJ101,y=MNC,feedback=c('x','plot','table')){
  x <- as.data.table(x)
  y <- as.data.table(y)
  y_TW <- plyr::count(y[y$nation %in% '台灣'],vars=c("company","year"))
  setnames(y_TW,'freq','MNC_TW')
  y_FOR <- plyr::count(y[!(y$nation %in% '台灣')],vars=c("company","year"))
  setnames(y_FOR,'freq','MNC_FOREIGN')
  y_mix <- merge(y_TW,y_FOR,by=c('company','year'),all = TRUE)
  x <- merge(x,y_mix,by=c('company','year'))
  # plot <- 
  
  ifelse(feedback=="x",return(x),ifelse(feedback=="plot",return(plot)))
}

#' 運行MNC函數
#+ load_readMNC
MNC <- readMNC()
summary(MNC)
#' 列出國外營運公司之所在地統計
MNC_nation <- as.data.frame(table(MNC$nation))
write.csv(MNC_nation,file="MNC_nation.csv")
#' 
#+
TEJ111 <- fnMNC(x=TEJ101,y=MNC,feedback='x')
TEJ112 <- fnMNC(x=TEJ102,y=MNC,feedback='x')
TEJ113 <- fnMNC(x=TEJ103,y=MNC,feedback='x')
TEJ114 <- fnMNC(x=TEJ104,y=MNC,feedback='x')

