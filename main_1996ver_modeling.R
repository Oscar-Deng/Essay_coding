# read in TEJ database to deploy linear model
if(exists("TEJ101")){
  source("main_1996ver_functions_loadin.R")
  } else {
    TEJ101 <- read.csv(file="TEJ101.csv")
    TEJ101_fill <- read.csv(file="TEJ101_fill.csv")
    }

#+ linear models, eval=TRUE, echo=TRUE
# fix RELATIN/RELATOUT = NaN, Inf, NA (for modeling can't include thee)

#TEJ_lmNew101 <- NewTEJ101
#TEJ_lm101 <- TEJ101_fill
#TEJ_lm101[!is.finite(TEJ_lm101$RELAT),]$RELAT <- 0
#TEJ_lm102 <- replace(TEJ102,TEJ102$RELAT[!is.finite(TEJ102$RELAT)],0)
#TEJ_lm103 <- replace(TEJ103_fill,TEJ103_fill$RELAT[!is.finite(TEJ103_fill$RELAT)],0)
#TEJ_lm104 <- replace(TEJ104,TEJ104$RELAT[!is.finite(TEJ104$RELAT)],0)
# without STR*HHI

ETRmdl_noSH <- lm(ETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC
                         +OUTINSTI+RELAT+FAM_Dum+GDP
                  ,TEJ101
                  #,TEJ101_fill
                  )
CETRmdl_noSH <- lm(CETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC
                          +OUTINSTI+RELAT+FAM_Dum+GDP
                   ,TEJ101
                   #,TEJ101_fill
                   )

ETRmdl_noSH_2 <- lm(ETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC
                            +OUTINSTI+RELAT+FAM_Dum+GDP
                    ,NewTEJ101)
CETRmdl_noSH_2 <- lm(CETR ~ STR+HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC
                             +OUTINSTI+RELAT+FAM_Dum+GDP
                     ,NewTEJ101)



# with STR*HHI
ETR_lmodel101_SH <- lm(ETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm101)
CETR_lmodel101_SH <- lm(CETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lm101)

ETR_lmodelNew101_SH <- lm(ETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lmNew101)
CETR_lmodelNew101_SH <- lm(CETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP,TEJ_lmNew101)

#ETR_lm_MNC2 <- lm(ETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP+MNC,TEJ_lm)
#CETR_lm_MNC2 <- lm(CETR ~ STR+HHI+STR_HHI+ROA+SIZE+LEV+INTANG+QUICK+EQINC+OUTINSTI+RELAT+FAM_Dum+GDP+MNC,TEJ_lm)

