# -----

STR.plot <- ggplot(
  data = reshape2::melt(TEJ8.1 %>% select(company,year,TSE_code,RD,EMP,MB,PPE,MARKET),id.var=c("company","year","TSE_code"))
  , aes(x=variable, y=value)) + geom_boxplot(aes(fill=TSE_code)) + facet_wrap( ~ variable, scales="free")


# -----
ETR.plot <- 
  ggplot(reshape2::melt(TEJ8.1 %>% mutate(nETR = doNormalization(ETR),nCETR = doNormalization(CETR)) %>% select(company,year,nETR,nCETR)
                        ,id.var=c("company","year"))
  , aes(x=variable, y=value)
  ) + geom_boxplot(aes(fill=year)
      ) + facet_wrap( ~ variable, scales="free")

# ----
var1.plot <- 
  ggplot(reshape2::melt(TEJ8.1 %>% select(year,RD,EMP,MB,MARKET,PPE)
                        ,id.var=c("year"))
         , aes(x=variable, y=value)
  ) + geom_boxplot(aes(fill=year)
  ) + facet_wrap( ~ variable, scales="free")

