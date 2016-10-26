# Simple reading in excel:

# use 'readxl' instead of 'XLConnect'

install.packages("readxl") 
require(readxl)

# SAMPLE:
read_excel(path = "datasets.xlsx", sheet = 2, col_names = TRUE, col_types = NULL, na = "", skip = 0)
### sheet can be either a string or an integer. i.g. 2 or "ETR" .
### col_names : TRUE or vector with given names>>>[aa,bb,cc,dd]
### col_types : NULL, "blank", "numeric", "date", "text"
### na : blank is converted to NA by default
### skip : Number of rows to skip before reading any data.


# applications:

datas <- system.file("datasets.xlsx", package = "readxl")
read_excel(datas)
summary(datas)
View(datas)

data2 <- datas[!(is.na(datas$年月)),] # remove NAs in 年月
data2 <- datas[which(年月!=="")]
table(data2$年月) # 查看"年月"是否還有NA及空白


spt_year <- split(data2$M所得稅費用, data2$年月); cbind(data2$公司,spt_year)->a

tapply(data3$M所得稅費用, ) -> data3$"456sum"s
