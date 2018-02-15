# Author: Marina Ferreira Uchoa
# Data Mining
# Final Project
# 2017-11-10

library(caret)
library(xlsx)
library(plyr)
library("PerformanceAnalytics")
library(ICC)
library(xtable)
library(rattle)
library(e1071)
library(kernlab)
library(doSNOW)
library(doParallel)
library(rgl)
library(xgboost)
library(ROCR)
library(vcd)
library(rpart)
library(neuralnet)
library(ranger)
library(dplyr)
library(ipred)
library(DAAG)

# uncomment below if you want the tables to not have scientific notation
# options(scipen = 10)

###################
### Import data ###
###################
pk_ratings <- read.xlsx("pokerratings.xlsx", 1)
# Rename data frame with excel document name
pokerratings <- pk_ratings

pk_vari <- read.csv("Pokervariables.csv")
pokervariables <- pk_vari

EF <-loadWorkbook("EF and gambling scores.xlsx")
EF_sheets <- names(getSheets(EF))

for(i in 1:length(EF_sheets)){
  assign(paste0("EF_",EF_sheets[i]),read.xlsx("EF and gambling scores.xlsx",i))
}


################################################################
### STANDARDIZE & CLEAN POKERRATINGS CONTENTS (pokerratings) ###
################################################################
# Remove levels with X in their PKMP tag
pokerratings$PKMP.ID <- as.character(pokerratings$PKMP.ID)
for (i in pokerratings$PKMP.ID){
  if (grepl("X",i)) pokerratings <- subset(pokerratings, pokerratings$PKMP.ID!=i)
}

# Remove observations with NA as Player.code
pokerratings <- subset(pokerratings, !(is.na(pokerratings$Player.code)))

# Make PKMP.ID variable have the same format as EF NA. variables
for (i in 1:length(pokerratings$PKMP.ID)){
  if(nchar(pokerratings$PKMP.ID[i])==1) pokerratings$PKMP.ID[i] <- paste0("PKMP00",pokerratings$PKMP.ID[i])
  else pokerratings$PKMP.ID[i] <- paste0("PKMP0",pokerratings$PKMP.ID[i])
}

# Remove columns with only NA observations
pokerratings <- pokerratings[,1:8]

# Substitute pk_ratings with final data
pk_ratings <- pokerratings


# Transform first two rows into factor
class_pk_ratings<-pk_ratings
class_pk_ratings[,1] <- as.factor(class_pk_ratings[,1])


####################################################
### STANDARDIZE & CLEAN POKERVARIABLES (pk_vari) ###
####################################################
# Take only rows where Player equals one of the Player.code in pokerratings (pk_ratings)
pk_vari <- subset(pk_vari, pk_vari$Player %in% pk_ratings$Player.code)

# Transform all columns except the first into character
for (i in 2:ncol(pk_vari)){
  pk_vari[,i] <- as.character(pk_vari[,i])
}

# Remove non-recognized symbols (â,¬) and non-informative symbols
pk_vari$My.C.Won<- gsub("[^[:digit:]./-]", "", pk_vari$My.C.Won)
for (i in 3:5){
  pk_vari[,i] <- gsub(",", "", pk_vari[,i])
}
pk_vari$Stake <- gsub("2 max|6 max|5 max", "", pk_vari$Stake)
pk_vari$Stake <- gsub("[^[:digit:]./]", "", pk_vari$Stake)
pk_vari$Stake <- gsub('(.*?)/.*','\\1', pk_vari$Stake) # if value in the format 0.xx/0.xx, keep only the first part
# remove observations with - as value
for (i in 7:ncol(pk_vari)){
  pk_vari[,i] <- gsub("[^[:digit:].]", "", pk_vari[,i])
}

# Transform all rows except the first into numeric
for (i in 2:ncol(pk_vari)){
  pk_vari[,i] <- as.numeric(pk_vari[,i])
}

# Include column with respective PKMP
PKMP.ID <- c()
for (i in 1:nrow(pk_vari)){
  PKMP.ID[i]<-pokerratings$PKMP.ID[pokerratings$Player.code %in% pk_vari[i,1]]
}
pk_vari <- cbind(PKMP.ID, pk_vari)
rm(PKMP.ID)


# Remove observations with hands lower than 100
pk_vari<- subset(pk_vari, pk_vari$Hands>=100)
 
# Keep only one observation per player
# Keep the one with most hands
pk_vari_reduced <- data.frame()
for(i in unique(pk_vari$PKMP.ID)){
  x<-subset(pk_vari, pk_vari$PKMP.ID==i)
  pk_vari_reduced <- rbind(pk_vari_reduced, subset(x, x$Hands == max(x$Hands)))
}
pk_vari<-pk_vari_reduced
rm(pk_vari_reduced)

# Rename data frame with excel document name
pokervariables <- pk_vari

# Remove player identifier
pk_vari$Player<-NULL

# Sort by ascending order of PKMP.ID
pk_vari<-pk_vari[order(pk_vari$PKMP.ID),]

# Transform first two rows into factor
class_pk_vari<-pk_vari
class_pk_vari[,1] <- as.factor(class_pk_vari[,1])


##########################
### BACK TO pk_ratings ###
##########################
# Remove player identifier
pk_ratings$Player.code<-NULL

# Remove rows regarding users for whom there is no poker variables
pk_ratings <- subset(pk_ratings, pk_ratings$PKMP.ID %in% pk_vari$PKMP.ID)

# Transform first two rows into factor
class_pk_ratings<-pk_ratings
class_pk_ratings[,1] <- as.factor(class_pk_ratings[,1])


#######################
### CLEAN EF TABLES ###
#######################
# Remove rows regarding users for whom there is no rating (users with missing Player.code or with PKMP tag with X) and no poker variables
for (i in EF_sheets){
  x <- get(paste0("EF_",i))
  colnames(x)[1] <- "PKMP.ID"
  assign(paste0("EF_",i), x[x$PKMP.ID %in% pk_vari$PKMP.ID,])
}

### REMOVE CONSTANT VARIABLES ###
### MAKE COLUMNS FORMAT APPROPRIATE ###

## EF_GEN_CHAR
# remove GENDER_F, make GENDER_M as factor
# summary(EF_GEN_CHAR)
EF_GEN_CHAR$GENDER_F<- NULL
EF_GEN_CHAR$MAX_DAILYBET_.1EUR<-NULL
EF_GEN_CHAR$MAX_DAILYBET_1_10EUR<-NULL
EF_GEN_CHAR$GENDER_M<- as.factor(EF_GEN_CHAR$GENDER_M)
EF_GEN_CHAR$MAX_DAILYBET_10_100EUR<- as.factor(EF_GEN_CHAR$MAX_DAILYBET_10_100EUR)
EF_GEN_CHAR$MAX_DAILYBET_100_1.000EUR<- as.factor(EF_GEN_CHAR$MAX_DAILYBET_100_1.000EUR)
EF_GEN_CHAR$MAX_DAILYBET_1.000_10.000<- as.factor(EF_GEN_CHAR$MAX_DAILYBET_1.000_10.000)
EF_GEN_CHAR$MAX_DAILYBET_.10.000<- as.factor(EF_GEN_CHAR$MAX_DAILYBET_.10.000)

## WCST
# remove WCST_T_SEC
EF_WCST$WCST_T_SEC<-NULL
# make WCST_LEARN numerica
EF_WCST$WCST_LEARN<-as.character(EF_WCST$WCST_LEARN)
EF_WCST$WCST_LEARN[23]<- NA
EF_WCST$WCST_LEARN<- as.numeric(EF_WCST$WCST_LEARN)

## FAB
EF_FAB$FAB_MOTOR_0_3<-NULL
EF_FAB$FAB_CONF_INSTR_0_3<-NULL
EF_FAB$FAB_PREHEN_0_3<-NULL


## CLOCK
# remove FDCT_N_COR_ORD_0_1, FDCT_NOROT_0_1, FDCT_2HND_0_1, FDCT_HND_PROP_0_1, FDCT_HND_JOIN_0_1, FDCT_T_SEC
EF_CLOCK$FDCT_N_COR_ORD_0_1 <- NULL
EF_CLOCK$FDCT_NOROT_0_1 <- NULL
EF_CLOCK$FDCT_2HND_0_1 <- NULL
EF_CLOCK$FDCT_HND_PROP_0_1 <- NULL
EF_CLOCK$FDCT_HND_JOIN_0_1 <- NULL
EF_CLOCK$FDCT_T_SEC <- NULL
# turn into categorical variables
EF_CLOCK$FDCT_CONT_ACC_0_1 <- as.factor(EF_CLOCK$FDCT_CONT_ACC_0_1)
EF_CLOCK$FDCT_CONT_NTOO_0_1 <- as.factor(EF_CLOCK$FDCT_CONT_NTOO_0_1)
EF_CLOCK$FDCT_N1.12_0_1 <- as.factor(EF_CLOCK$FDCT_N1.12_0_1)
EF_CLOCK$FDCT_ARABIC_0_1 <- as.factor(EF_CLOCK$FDCT_ARABIC_0_1)
EF_CLOCK$FDCT_N_COR_POS_0_1 <- as.factor(EF_CLOCK$FDCT_N_COR_POS_0_1)
EF_CLOCK$FDCT_N_IN_CONT_0_1 <- as.factor(EF_CLOCK$FDCT_N_IN_CONT_0_1)
EF_CLOCK$FDCT_HN_IND_0_1 <- as.factor(EF_CLOCK$FDCT_HN_IND_0_1)
EF_CLOCK$FDCT_MN_IND_0_1 <- as.factor(EF_CLOCK$FDCT_MN_IND_0_1)
EF_CLOCK$FDCT_NO_XTRAMARK_0_1 <- as.factor(EF_CLOCK$FDCT_NO_XTRAMARK_0_1)
EF_CLOCK$FDCT_CLK_CENTRE_0_1 <- as.factor(EF_CLOCK$FDCT_CLK_CENTRE_0_1)


## SPM_RAVEN
# remove constant variables
EF_SPM_RAVEN$SPM_A_ANSW_0_12 <- NULL
EF_SPM_RAVEN$SPM_B_ANSW_0_12 <- NULL
EF_SPM_RAVEN$SPM_C_ANSW_0_12 <- NULL
EF_SPM_RAVEN$SPM_D_ANSW_0_12 <- NULL


## FLU_P

## FLU_S
# remove constant variables
EF_FLU_S$FLUS_FR_ER_VAR <- NULL
EF_FLU_S$FLUS_AN_ER_VAR <- NULL
EF_FLU_S$FLUS_AN_ER_OTHER <- NULL
EF_FLU_S$FLUS_CA_ER_VAR <- NULL
EF_FLU_S$FLUS_AN_ER_OTHER.1 <- NULL
EF_FLU_S$FLUS_ALLER_VAR <- NULL

## MEM_INT
# remove constant variables
EF_MEM_INT$MEMINT_10SEC_FGL_LP_0_3 <- NULL
EF_MEM_INT$MEMINT_10SEC_FGL_JL_0_3 <- NULL
EF_MEM_INT$MEMINT_10SEC_PMT_JL_0_3 <- NULL
EF_MEM_INT$MEMINT_30SEC_JL_ZLR_0_3 <- NULL
EF_MEM_INT$MEMINT_30SEC_JL_QVS_0_3 <- NULL
EF_MEM_INT$MEMINT_30SEC_JL_DNC_0_3 <- NULL
EF_MEM_INT$MEMINT_30SEC_JL_TOT_0_9 <- NULL

## FRED_CRT
EF_FRED_CRT$NA..1 <- NULL

## FRED_RSBQ
# remove constant variables
EF_FRED_RSBQ$RSBQ_I_100_CERTGAIN <- NULL
EF_FRED_RSBQ$RSBQ_I_25._GAIN_200_LEVG <- NULL
EF_FRED_RSBQ$RSBQ_J_100_CERTGAIN <- NULL
EF_FRED_RSBQ$RSBQ_J_25._GAIN_300_LEVG <- NULL
EF_FRED_RSBQ$RSBQ_N_10_CERTLOSS <- NULL
EF_FRED_RSBQ$RSBQ_N_90._50_LOSS_LEVG <- NULL
EF_FRED_RSBQ$RSBQ_Q_50_CERTLOSS <- NULL
EF_FRED_RSBQ$RSBQ_Q_10._800_LOSS_LEVQ <- NULL

# transform into categorical
for(i in 2:17){
  EF_FRED_RSBQ[,i]<-as.factor(EF_FRED_RSBQ[,i])
}

for(i in 20:25){
  EF_FRED_RSBQ[,i]<-as.factor(EF_FRED_RSBQ[,i])
}

for(i in 28:33){
  EF_FRED_RSBQ[,i]<-as.factor(EF_FRED_RSBQ[,i])
}


## FRED_IBQ
for(i in 2:23){
  EF_FRED_IBQ[,i]<-as.factor(EF_FRED_IBQ[,i])
}


## MAT_ATT
EF_MAT_ATT$MAT_I_RA_0_10<- NULL
EF_MAT_ATT$MAT_I_FA_0_100 <- NULL
EF_MAT_ATT$MAT_I_OM_0_10 <- NULL
EF_MAT_ATT$MAT_I_OOT <- NULL
EF_MAT_ATT$MAT_II_OOT <- NULL


## DIGIT

## TMT

## STROOP
EF_STROOP$STR_RD_ER_0_30<- NULL

## TOL
EF_TOL$TOL_1_TOT_RA<- NULL
EF_TOL$TOL_1_TOT_FA<- NULL
EF_TOL$TOL_1_SCORE_0_3<- NULL
EF_TOL$TOL_2_TOT_RA<- NULL
EF_TOL$TOL_1_SCORE_0_3<- NULL
EF_TOL$TOL_3_NUMBTRY_0_3<- NULL
EF_TOL$TOL_3_TOT_VIOL<- NULL
EF_TOL$TOL_3_TOT_MOVE_3<- NULL
EF_TOL$TOL_3_TOT_RA<- NULL
EF_TOL$TOL_3_TOT_FA<- NULL
EF_TOL$TOL_4_TOT_VIOL<- NULL
EF_TOL$TOL_4_TOT_RA<- NULL
EF_TOL$TOL_5_TOT_VIOL<- NULL
EF_TOL$TOL_5_TOT_RA<- NULL
EF_TOL$TOL_7_NUMBTRY_0_3<- NULL
EF_TOL$TOL_7_TOT_VIOL<- NULL
EF_TOL$TOL_7_TOT_RA<- NULL
EF_TOL$TOL_7_TOT_FA<- NULL
EF_TOL$TOL_8_TOT_RA<- NULL
EF_TOL$TOL_9_TOT_RA<- NULL
EF_TOL$TOL_11_TOT_RA<- NULL
EF_TOL$TOL_12_TOT_RA<- NULL
EF_TOL$TOL_2_SCORE_0_3<- NULL


## STIME
EF_STIME$NA..1<-NULL

## EQ-i s
# summary(`EF_EQ-i s`)
`EF_EQ-i s`$EQUIS_TOTEQ_XLW_.70<- NULL
`EF_EQ-i s`$EQIS_TOTEQ_VHI_120_129<- NULL
`EF_EQ-i s`$EQIS_TOTEQ_XHI_.130<- NULL
`EF_EQ-i s`$EQIS_INTRA_VHI_120_129<- NULL
`EF_EQ-i s`$EQUIS_INTEREQ_XLW_.70<- NULL
`EF_EQ-i s`$EQIS_INTEREQ__VLW_70_79<- NULL
`EF_EQ-i s`$EQUIS_INTEREQ_XHI_.130<- NULL
`EF_EQ-i s`$EQIS__ADEQ_VLW_70_79<- NULL
`EF_EQ-i s`$EQUIS_XHI_.130<- NULL
`EF_EQ-i s`$EQUIS_SMEQ_XLW_.70<- NULL
`EF_EQ-i s`$EQIS__GMEQ_VLW_70_79<- NULL
`EF_EQ-i s`$EQUIS_GMEQ_XHI_.130<- NULL
`EF_EQ-i s`$EQUIS_PIEQ_XHI_.130<- NULL
# `EF_EQ-i s`[,grep("XLW|VLW|LOW|MED|HI|VHI|XHI", names(`EF_EQ-i s`))]
for(i in 1:length(grep("XLW|VLW|LOW|MED|HI|VHI|XHI", names(`EF_EQ-i s`)))){
  `EF_EQ-i s`[,grep("XLW|VLW|LOW|MED|HI|VHI|XHI", names(`EF_EQ-i s`))[i]]<- as.factor(`EF_EQ-i s`[,grep("XLW|VLW|LOW|MED|HI|VHI|XHI", names(`EF_EQ-i s`))[i]])
}

## HAM-D
`EF_HAM-D`$HAMD_MOD_18_24<- NULL
`EF_HAM-D`$HAMD_SEV_.25<- NULL

## HAM-A
`EF_HAM-A`$HAMA_NOA_0_13 <- as.factor(`EF_HAM-A`$HAMA_NOA_0_13)
`EF_HAM-A`$HAMA_ANX_.13<- as.factor(`EF_HAM-A`$HAMA_ANX_.13)


### CHECK FOR CORRELATION IN EF SHEETS ###
## EF_GEN_CHAR
# chart.Correlation(EF_GEN_CHAR[, sapply(EF_GEN_CHAR, is.numeric)], histogram=TRUE, pch=19)

## WCST
# chart.Correlation(EF_WCST[, sapply(EF_WCST, is.numeric)], histogram=F, pch=19)
# w<-findCorrelation(cor(na.omit(EF_WCST[, sapply(EF_WCST, is.numeric)])), cutoff = 0.9) 
# names(EF_WCST[,w])
EF_WCST$WCST_TOT_COR<-NULL
EF_WCST$WCST_TOT_ER<-NULL
EF_WCST$WCST_._ER<- NULL
EF_WCST$WCST_PERS_RESP<- NULL
EF_WCST$WCST_NONPERS_ER<-NULL
EF_WCST$WCST_._NONPERS_ER<-NULL
EF_WCST$WCST_._PERS_ER<-NULL
EF_WCST$WCST_._CL_RESP<-NULL
EF_WCST$WCST_GEN_SCORE<-NULL

## FAB
# chart.Correlation(EF_FAB[, sapply(EF_FAB, is.numeric)], histogram=F, pch=19)
EF_FAB$FAB_GONOGO_0_3<-NULL
EF_FAB$FAB_LEX_FLU_S_TOT_WORDS<-NULL

## CLOCK

## SPM_RAVEN
# chart.Correlation(EF_SPM_RAVEN[, sapply(EF_SPM_RAVEN, is.numeric)], histogram=F, pch=19)
EF_SPM_RAVEN$SPM_TOT_ANSW_0_60<-NULL
EF_SPM_RAVEN$SPM_TOT_COR_0_60<-NULL
EF_SPM_RAVEN$SPM_TOT_T_SEC<-NULL

## FLU_P
# chart.Correlation(EF_FLU_P[, sapply(EF_FLU_P, is.numeric)], histogram=F, pch=19)
EF_FLU_P$FLUF_F_TOT_WORDS<-NULL
EF_FLU_P$FLUF_ALLRA<-NULL
EF_FLU_P$FLUF_P_TOT_WORDS<-NULL
EF_FLU_P$FLUF_ALLER_VAR<-NULL
EF_FLU_P$FLUF_L_TOT_WORDS<-NULL

## FLU_S
# chart.Correlation(EF_FLU_S[, sapply(EF_FLU_S, is.numeric)], histogram=F, pch=19)
EF_FLU_S$FLUS_CA_TOT_ER<-NULL
EF_FLU_S$FLUS_AN_TOT_ER<-NULL
EF_FLU_S$FLUS_ALLER_OTHER<-NULL
EF_FLU_S$FLUS_FR_TOT_WORDS<-NULL
EF_FLU_S$FLUS_ALLWORDS<-NULL
EF_FLU_S$FLUS_ALLER_TOT<-NULL
EF_FLU_S$FLUS_CA_TOT_WORDS<-NULL
EF_FLU_S$FLUS_AN_RA<-NULL

## MEM_INT
# summary(EF_MEM_INT)
# chart.Correlation(EF_MEM_INT[, sapply(EF_MEM_INT, is.numeric)], histogram=F, pch=19)
EF_MEM_INT$MEMINT_ALLPERS_INTRUS_0_18<-NULL
EF_MEM_INT$MEMINT_10SEC_JL_TOT_0_9<-NULL
EF_MEM_INT$MEMINT_30SEC_LP_DNC_0_3<-NULL
EF_MEM_INT$MEMINT_10SEC_CRB_LP_0_3<-NULL
EF_MEM_INT$MEMINT_10SEC_PERS_INTRUS_TOT_0_9<-NULL



## FRED_CRT
# chart.Correlation(EF_FRED_CRT[, sapply(EF_FRED_CRT, is.numeric)], histogram=F, pch=19)
EF_FRED_CRT$CRT_TOT_T_SEC<-NULL

## FRED_RSBQ
# chart.Correlation(EF_FRED_RSBQ[, sapply(EF_FRED_RSBQ, is.numeric)], histogram=F, pch=19)
EF_FRED_RSBQ$RSBQ_TOT_CERTGAIN_VS_HEVG_0_8<-NULL
EF_FRED_RSBQ$RSBQ_TOT_HEVG_VS_CERTGAIN_0_8<-NULL
EF_FRED_RSBQ$RSBQ_TOT_CERTLOSS_VS_LEVG_0_5<-NULL
EF_FRED_RSBQ$RSBQ_TOT_CERTGAIN_VS_LEVG_0_5<-NULL

## FRED_IBQ
# chart.Correlation(EF_FRED_IBQ[, sapply(EF_FRED_IBQ, is.numeric)], histogram=F, pch=19)
EF_FRED_IBQ$IBQ_TOT_FH_IMMED_VS_SEQ_DR_0_3<-NULL
EF_FRED_IBQ$IBQ_TOT_AK_IMMED_VS_PATIENT_OPT_0_11<-NULL
EF_FRED_IBQ$IBQ_TOT_AE_IMMED_VS_LARGER_DR_0_5<-NULL
EF_FRED_IBQ$IBQ_TOT_AK_PATIENT_OPT_VS_IMMED_0_11<-NULL

## MAT_ATT
# chart.Correlation(EF_MAT_ATT[, sapply(EF_MAT_ATT, is.numeric)], histogram=F, pch=19)
EF_MAT_ATT$MAT_OOT_TOT_0_3<-NULL
EF_MAT_ATT$MAT_T_SEC_TOT_0_135<-NULL
EF_MAT_ATT$MAT_INTRUS_TOT_0.40<-NULL
EF_MAT_ATT$MAT_III_INTRUS_0_30<-NULL
EF_MAT_ATT$MAT_II_INTRUS_0_10<-NULL
EF_MAT_ATT$MAT_II_OM_0_20<-NULL
EF_MAT_ATT$MAT_II_T_SEC_0_45<-NULL
EF_MAT_ATT$MAT_III_RA_0_30<-NULL
EF_MAT_ATT$MAT_III_FA_0_80<-NULL
EF_MAT_ATT$MAT_OM_TOT_0_60<-NULL

## DIGIT
# chart.Correlation(EF_DIGIT[, sapply(EF_DIGIT, is.numeric)], histogram=F, pch=19)
EF_DIGIT$DIGIT_FW_RA_0_16<-NULL
EF_DIGIT$DIGIT_BK_RA_0_10<-NULL


## TMT
# chart.Correlation(EF_TMT[, sapply(EF_TMT, is.numeric)], histogram=F, pch=19)

## STROOP
# chart.Correlation(EF_STROOP[, sapply(EF_STROOP, is.numeric)], histogram=F, pch=19)
EF_STROOP$STR_DN_ER_0_30<-NULL
EF_STROOP$STR_III_T_SEC<-NULL
EF_STROOP$STR_EFF_ER<-NULL

## TOL
# X<-EF_TOL[, sapply(EF_TOL, is.numeric)]
# chart.Correlation(X[,c(1:40)], histogram=F, pch=19)
# chart.Correlation(X[,c(41:ncol(X))], histogram=F, pch=19)
# tmp <- cor(X)
# tmp[!lower.tri(tmp)] <- 0
# data.new <- X[,apply(tmp,2,function(x) any(x > 0.9))]
EF_TOL$TOL_1_NUMBTRY_0_3<-NULL
EF_TOL$TOL_1_TOT_TSEC_DEC<-NULL
EF_TOL$TOL_1_TOT_VIOL<-NULL
EF_TOL$TOL_2_NUMBTRY_0_3<-NULL
EF_TOL$TOL_3_TOT_TSEC_EXE<-NULL
EF_TOL$TOL_4_TOT_TSEC_DEC<-NULL
EF_TOL$TOL_4_TOT_TSEC_EXE<-NULL
EF_TOL$TOL_4_TOT_TSEC_TOT<-NULL
EF_TOL$TOL_4_TOT_MOVE_3<-NULL
EF_TOL$TOL_5_NUMBTRY_0_3<-NULL
EF_TOL$TOL_5_TOT_TSEC_DEC<-NULL
EF_TOL$TOL_5_TOT_MOVE_4<-NULL
EF_TOL$TOL_5_TOT_FA<-NULL
EF_TOL$TOL_7_TOT_TSEC_TOT<-NULL
EF_TOL$TOL_8_TOT_TSEC_TOT<-NULL
EF_TOL$TOL_10_TOT_TSEC_DEC<-NULL
EF_TOL$TOL_11_TOT_TSEC_TOT<-NULL
EF_TOL$TOL_12_TOT_TSEC_TOT<-NULL
EF_TOL$TOL_12_TOT_FA<-NULL
EF_TOL$TOL_TSEC_ALLDEC<-NULL
EF_TOL$TOL_6_TOT_TSEC_DEC<-NULL
EF_TOL$TOL_8_TOT_VIOL<-NULL



## STIME
# chart.Correlation(EF_STIME[, sapply(EF_STIME, is.numeric)], histogram=F, pch=19)


## EQ-i s
# chart.Correlation(`EF_EQ-i s`[, sapply(`EF_EQ-i s`, is.numeric)], histogram=F, pch=19)
# ncol(`EF_EQ-i s`)
# X<-`EF_EQ-i s`[, sapply(`EF_EQ-i s`, is.numeric)]
# tmp <- cor(X)
# tmp[!lower.tri(tmp)] <- 0
# data.new <- X[,apply(tmp,2,function(x) any(x > 0.9))]
# chart.Correlation(data.new,histogram = F)
# names(`EF_EQ-i s`)
`EF_EQ-i s`$EQIS_INTRA_EQ_RAW<-NULL
`EF_EQ-i s`$EQIS_INTER_EQ_RAW<-NULL
`EF_EQ-i s`$EQIS_AD_EQ_RAW<-NULL
`EF_EQ-i s`$EQIS_SM_EQ_RAW<-NULL
`EF_EQ-i s`$EQIS_GM_EQ_RAW<-NULL
`EF_EQ-i s`$EQIS_PI_EQ_RAW<-NULL
cat_EF_EQ<- `EF_EQ-i s`[,grep("XLW|VLW|LOW|MED|HI|VHI|XHI", names(`EF_EQ-i s`))]
remove<-c()
for(i in 1:ncol(cat_EF_EQ)){
  for(j in 1:ncol(cat_EF_EQ)){
    if(i!=j){
      x<-assocstats(ftable(cat_EF_EQ[,i],cat_EF_EQ[,j]))$cramer
      if(!is.na(x)&x>0.90){
        remove<-cbind(remove,i)
      }
    }
  }
}
for(i in names(cat_EF_EQ)[unique(as.vector(remove))]){
  `EF_EQ-i s`[,i]<-NULL
}
# names(`EF_EQ-i s`)

## HAM-D
# chart.Correlation(`EF_HAM-D`[, sapply(`EF_HAM-D`, is.numeric)], histogram=F, pch=19)
`EF_HAM-D`$HAMD_LOW_D_8_17<-NULL


## HAM-A


################################
### CLEAN GAMBLING VARIABLES ###
################################

## GRCS
# remove constants
#EF_GRCS<-EF_GRCS[,c("PKMP.ID","GRCS_TOT_SCORE_23_161")]
EF_GRCS$GRCS_TOT_ITEMS<- NULL
EF_GRCS$GRCS_GE_ITEMS<- NULL
EF_GRCS$GRCS_IC_ITEMS<- NULL
EF_GRCS$GRCS_PC_ITEMS<- NULL
EF_GRCS$GRCS_IS_ITEMS<- NULL
EF_GRCS$GRCS_IB_ITEMS<- NULL

## GRCS correlation analysis
# chart.Correlation(EF_GRCS[, sapply(EF_GRCS, is.numeric)], histogram=F, pch=19)
# X<-EF_GRCS[, sapply(EF_GRCS, is.numeric)]
# tmp <- cor(X)
# tmp[!lower.tri(tmp)] <- 0
# data.new <- X[,apply(tmp,2,function(x) any(x > 0.9))]
# names(data.new)
EF_GRCS$GRCS_TOT_AVER_1_7 <-NULL
EF_GRCS$GRCS_GE_AVER_1_7 <-NULL
EF_GRCS$GCRS_PC_AVER_1_7<-NULL
EF_GRCS$GRCS_IC_AVER_1_7<-NULL
EF_GRCS$GRCS_IS_AVER_1_7<-NULL
EF_GRCS$GCRS_IB_AVER_1_7<-NULL


## SOGS
# Keep only SOGS_TOT_0_20
for(i in 3:5){
  EF_SOGS[,i]<-as.factor(EF_SOGS[,i])
}
EF_SOGS_ORIGINAL<- EF_SOGS
EF_SOGS<-EF_SOGS[,c("PKMP.ID","SOGS_TOT_0_20")]

## PGSI
# names(EF_PGSI)
EF_PGSI$PGSI_Q01_0_3<-NULL
EF_PGSI$PGSI_Q02_0_3<-NULL
EF_PGSI$PGSI_Q03_0_3<-NULL
EF_PGSI$PGSI_Q04_0_3<-NULL
EF_PGSI$PGSI_Q05_0_3<-NULL
EF_PGSI$PGSI_Q06_0_3<-NULL
EF_PGSI$PGSI_Q07_0_3<-NULL
EF_PGSI$PGSI_Q08_0_3<-NULL
EF_PGSI$PGSI_Q09_0_3<-NULL
for(i in 3:6){
  EF_PGSI[,i]<-as.factor(EF_PGSI[,i])
}
# summary(EF_PGSI)
EF_PGSI_ORIGINAL<-EF_PGSI
EF_PGSI<-EF_PGSI[,c("PKMP.ID","PGSI_TOTSCORE_0_27")]


#######################################
### JOIN ALL SHEETS IN ONE DOCUMENT ###
#######################################
EF_ALL<-data.frame(PKMP.ID=pk_vari$PKMP.ID)
for (i in EF_sheets){
  EF_ALL<-merge(EF_ALL,get(paste0("EF_",i)))
}



##########################################################################
### Which poker variables are most important for judging poker skills? ###
##########################################################################
avg_rating<-c()
for(i in 1:nrow(pk_vari)){
  avg_rating<-rbind(avg_rating,pk_ratings$Average.rating[pk_vari$PKMP.ID[i]==pk_ratings$PKMP.ID])
}
pk_vari<- cbind(pk_vari,avg_rating)

# chart.Correlation(pk_vari[, sapply(pk_vari, is.numeric)], histogram=F, pch=19)
# matrix_cor<-cor(pk_vari[, sapply(pk_vari, is.numeric)])
# matrix_cor[upper.tri(matrix_cor,diag = T)]<-NA
# matrix_cor

model1<-lm(avg_rating~., data=pk_vari[,-1])
summary(model1)

anova(model1)
anova(update(model1, .~.-WSD))
anova(update(model1, .~.-WSD-All.In.Adj.BB.100))
anova(update(model1, .~.-WSD-All.In.Adj.BB.100-Hands))
anova(update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..))
anova(update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..-VPIP))
anova(update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..-VPIP-PFR))
anova(update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..-VPIP-Total.AF))

anova(model1,
      update(model1, .~.-WSD),
      update(model1, .~.-WSD-All.In.Adj.BB.100),
      update(model1, .~.-WSD-All.In.Adj.BB.100-Hands),
      update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..),
      update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..-VPIP),
      update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..-VPIP-Total.AF),
      update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..-VPIP-Total.AF-My.C.Won))

summary(update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..-VPIP))
summary(update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..-VPIP-PFR))
summary(update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..-VPIP-Total.AF))
summary(update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..-VPIP-Total.AF-My.C.Won))

final_lm<-update(model1, .~.-WSD-All.In.Adj.BB.100-Hands-WTSD..-VPIP-Total.AF-My.C.Won)
summary(final_lm)
xtable(final_lm)

#####################################################################
### Which EF variables are most related to which poker variables? ###
#####################################################################

# Check that the data sets are in the same order
cbind(as.character(pk_vari$PKMP.ID),as.character(EF_ALL$PKMP.ID))

# Compute correlations and ICC
EF_vari<-data.frame()
EF_vari_cat<-data.frame()
for (i in 5:ncol(EF_ALL)){
  for(j in 2:ncol(pk_vari)){
    if (is.numeric(EF_ALL[,i])){
      x<-cor(EF_ALL[,i],pk_vari[,j])
      if(!is.na(x)&x>0.7){
        y<-data.frame(EF_variable=names(EF_ALL)[i],Pk_variable=names(pk_vari)[j],Correlation=x,i=i,j=j)
        EF_vari<-rbind(EF_vari,y)
      }
    } else{
      z<-ICCbare(EF_ALL[,i],pk_vari[,j])
      if(!is.na(z)&z>0.75){
        y<-data.frame(EF_variable=names(EF_ALL)[i],Pk_variable=names(pk_vari)[j],ICC=z,i=i,j=j)
        EF_vari_cat<-rbind(EF_vari_cat,y)
      }
    }
  }
}
xtable(EF_vari[order(EF_vari$Correlation),])
xtable(EF_vari_cat[order(EF_vari_cat$ICC),])

############################################################################
### Which EF variables are best for separating strong from weak players? ###
### Average from strong? And average from weak?                          ###
############################################################################
pk_ratings<-cbind(pk_ratings,poker_ability=c(rep(NA, nrow(pk_ratings))))
for(i in 1:length(pk_ratings$Average.rating)){
  if(pk_ratings$Average.rating[i]<40){
    pk_ratings$poker_ability[i]<-"weak"
  }
  else if (pk_ratings$Average.rating[i]>= 40 & pk_ratings$Average.rating[i]<65){
    pk_ratings$poker_ability[i]<- "medium"
  } 
  else {
    pk_ratings$poker_ability[i] <- "strong"
    }
}
summary(as.factor(pk_ratings$poker_ability))

EF_ABILITY<-merge(cbind(PKMP.ID=pk_ratings$PKMP.ID,poker_ability=pk_ratings$poker_ability), EF_ALL)


# Create data sets for each group of levels
EF_SW<-subset(EF_ABILITY, EF_ABILITY$poker_ability== "strong"|EF_ABILITY$poker_ability== "weak")
EF_SW$poker_ability<-droplevels(EF_SW$poker_ability)
EF_AS<-subset(EF_ABILITY, EF_ABILITY$poker_ability== "strong"|EF_ABILITY$poker_ability== "medium")
EF_AS$poker_ability<-droplevels(EF_AS$poker_ability)
EF_AW<-subset(EF_ABILITY, EF_ABILITY$poker_ability== "medium"|EF_ABILITY$poker_ability== "weak")
EF_AW$poker_ability<-droplevels(EF_AW$poker_ability)

# Select important variables
# STRONG VS WEAK
EF_vari_SW<-data.frame()
EF_vari_cat_SW<-data.frame()
for (i in 5:ncol(EF_SW)){
    if (is.factor(EF_SW[,i])){
      x<-assocstats(ftable(EF_SW[,2],EF_SW[,i]))$cramer
      if(!is.na(x)&x>0.5){
        y<-data.frame(EF_variable=names(EF_SW)[i],CramerV=x,i=i,j=j)
        EF_vari_cat_SW<-rbind(EF_vari_cat_SW,y)
      }
    } else{
      z<-ICCbare(EF_SW[,2],EF_SW[,i])
      if(!is.na(z)&z>0.5){
        y<-data.frame(EF_variable=names(EF_SW)[i],ICC=z,i=i,j=j)
        EF_vari_SW<-rbind(EF_vari_SW,y)
      }
    }
}

# AVERAGE VS STRONG
EF_vari_AS<-data.frame()
EF_vari_cat_AS<-data.frame()
for (i in 5:ncol(EF_AS)){
  if (is.factor(EF_AS[,i])){
    x<-assocstats(ftable(EF_AS[,2],EF_AS[,i]))$cramer
    if(!is.na(x)&x>0.40){
      y<-data.frame(EF_variable=names(EF_AS)[i],CramerV=x,i=i,j=j)
      EF_vari_cat_AS<-rbind(EF_vari_cat_AS,y)
    }
  } else{
    z<-ICCbare(EF_AS[,2],EF_AS[,i])
    if(!is.na(z)&z>0.40){
      y<-data.frame(EF_variable=names(EF_AS)[i],ICC=z,i=i,j=j)
      EF_vari_AS<-rbind(EF_vari_AS,y)
    }
  }
}

# AVERAGE VS WEAK
EF_vari_AW<-data.frame()
EF_vari_cat_AW<-data.frame()
for (i in 5:ncol(EF_AW)){
  if (is.factor(EF_AW[,i])){
    x<-assocstats(ftable(EF_AW[,2],EF_AW[,i]))$cramer
    if(!is.na(x)&x>0.45){
      y<-data.frame(EF_variable=names(EF_AW)[i],CramerV=x,i=i,j=j)
      EF_vari_cat_AW<-rbind(EF_vari_cat_AW,y)
    }
  } else{
    z<-ICCbare(EF_AW[,2],EF_AW[,i])
    if(!is.na(z)&z>0.45){
      y<-data.frame(EF_variable=names(EF_AW)[i],ICC=z,i=i,j=j)
      EF_vari_AW<-rbind(EF_vari_AW,y)
    }
  }
}

## MODEL BUILDING
# STRONG VS WEAK
strong_weak<-glm(EF_SW$poker_ability~.,family = binomial(link = "logit"), data=EF_SW[,c(as.vector(EF_vari_SW$EF_variable), as.vector(EF_vari_cat_SW$EF_variable))])
summary(strong_weak)

anova(strong_weak, test="Chisq")
anova(update(strong_weak, .~.-EQIS_SMEQ_HI_110_119), test="Chisq")
anova(update(strong_weak, .~.-EQIS_SMEQ_HI_110_119-EQIS__LOW_80_89), test="Chisq")
anova(update(strong_weak, .~.-EQIS_SMEQ_HI_110_119-EQIS__LOW_80_89-RSBQ_R_100_CERTLOSS), test="Chisq")
anova(update(strong_weak, .~.-EQIS_SMEQ_HI_110_119-EQIS__LOW_80_89-RSBQ_R_100_CERTLOSS-RSBQ_R_3._7000_LOSS_LEVG), test="Chisq")

anova(strong_weak,
      update(strong_weak, .~.-EQIS_SMEQ_HI_110_119),
      update(strong_weak, .~.-EQIS_SMEQ_HI_110_119-EQIS__LOW_80_89),
      update(strong_weak, .~.-EQIS_SMEQ_HI_110_119-EQIS__LOW_80_89-RSBQ_R_100_CERTLOSS),
      update(strong_weak, .~.-EQIS_SMEQ_HI_110_119-EQIS__LOW_80_89-RSBQ_R_100_CERTLOSS-RSBQ_R_3._7000_LOSS_LEVG),test="Chisq")
# final model for Strong vs Weak
summary(update(strong_weak, .~.-EQIS_SMEQ_HI_110_119-EQIS__LOW_80_89-RSBQ_R_100_CERTLOSS-RSBQ_R_3._7000_LOSS_LEVG))


# AVERAGE VS STRONG
average_strong<-glm(EF_AS$poker_ability~.,family = binomial(link = "logit"), data=EF_AS[,c(as.vector(EF_vari_AS$EF_variable), as.vector(EF_vari_cat_AS$EF_variable))])
summary(average_strong)

anova(average_strong, test="Chisq")
anova(update(average_strong, .~.-EQIS_GMEQ_HI_110_119), test="Chisq")
anova(update(average_strong, .~.-EQIS_GMEQ_HI_110_119-EQIS_ADEQ_HI_110_119), test="Chisq")
anova(update(average_strong, .~.-EQIS_GMEQ_HI_110_119-EQIS_ADEQ_HI_110_119-TOL_12_SCORE_0_3), test="Chisq")

anova(average_strong,
      update(average_strong, .~.-EQIS_GMEQ_HI_110_119),
      update(average_strong, .~.-EQIS_GMEQ_HI_110_119-EQIS_ADEQ_HI_110_119),
      update(average_strong, .~.-EQIS_GMEQ_HI_110_119-EQIS_ADEQ_HI_110_119-TOL_12_SCORE_0_3),test="Chisq")
# final model for Average vs Strong
summary(update(average_strong, .~.-EQIS_GMEQ_HI_110_119-EQIS_ADEQ_HI_110_119-TOL_12_SCORE_0_3))

# AVERAGE VS STRONG
average_weak<-glm(EF_AW$poker_ability~.,family = binomial(link = "logit"), data=EF_AW[,c(as.vector(EF_vari_AW$EF_variable), as.vector(EF_vari_cat_AW$EF_variable))])
anova(average_weak, test = "Chisq")
anova(update(average_weak, .~.-EQIS__LOW_80_89), test="Chisq")
anova(update(average_weak, .~.-EQIS__LOW_80_89-RSBQ_C_1000_CERTGAIN), test="Chisq")
anova(update(average_weak, .~.-EQIS__LOW_80_89-RSBQ_C_1000_CERTGAIN-RSBQ_C_75._4000_GAIN_HEVG), test="Chisq")

anova(average_weak,
      update(average_weak, .~.-EQIS__LOW_80_89),
      update(average_weak, .~.-EQIS__LOW_80_89-RSBQ_C_1000_CERTGAIN),
      update(average_weak, .~.-EQIS__LOW_80_89-RSBQ_C_1000_CERTGAIN-RSBQ_C_75._4000_GAIN_HEVG),test="Chisq")
# final model for Average vs Strong
summary(update(average_weak, .~.-EQIS__LOW_80_89-RSBQ_C_1000_CERTGAIN-RSBQ_C_75._4000_GAIN_HEVG))

#####################################################
### Can poker variables predict problem gambling? ###
#####################################################
##################
## LINEAR MODEL ##
##################

# For GRCS
EF_GRCS_MODEL<-merge(cbind(PKMP.ID=as.character(EF_GRCS$PKMP.ID),GRCS_TOT_SCORE_23_161=as.numeric(EF_GRCS$GRCS_TOT_SCORE_23_161)),pk_vari)
EF_GRCS_MODEL$avg_rating<-NULL
EF_GRCS_MODEL$GRCS_TOT_SCORE_23_161<-as.numeric(as.character(EF_GRCS_MODEL$GRCS_TOT_SCORE_23_161))

GRCS_m1<- lm(GRCS_TOT_SCORE_23_161~.-PKMP.ID, data=EF_GRCS_MODEL)
summary(GRCS_m1)
plot(GRCS_m1)
anova(GRCS_m1, test="Chisq")
anova(update(GRCS_m1, .~.-Total.AF))
anova(update(GRCS_m1, .~.-Total.AF-Hands))
anova(update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq))
anova(update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100))
anova(update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100))
anova(update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100-WTSD..))
anova(update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100-WTSD..-PFR))
anova(update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100-WTSD..-PFR-VPIP))
anova(update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100-WTSD..-PFR-VPIP-WSD))
anova(update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100-WTSD..-PFR-VPIP-WSD-My.C.Won))
anova(update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100-WTSD..-PFR-VPIP-WSD-My.C.Won-Stake))
anova(GRCS_m1,
      update(GRCS_m1, .~.-Total.AF),
      update(GRCS_m1, .~.-Total.AF-Hands),
      update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq),
      update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100),
      update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100),
      update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100-WTSD..),
      update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100-WTSD..-PFR),
      update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100-WTSD..-PFR-VPIP),
      update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100-WTSD..-PFR-VPIP-WSD),
      update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100-WTSD..-PFR-VPIP-WSD-My.C.Won),
      update(GRCS_m1, .~.-Total.AF-Hands-Total.AFq-BB.100-All.In.Adj.BB.100-WTSD..-PFR-VPIP-WSD-My.C.Won-Stake),
      test="Chisq")

chart.Correlation(EF_GRCS_MODEL[,-1], histogram = T)
# there is no linear relationship with poker variables - not predictable with linear model
cv.lm(data=EF_GRCS_MODEL, form.lm = formula(GRCS_TOT_SCORE_23_161 ~ 1), m= nrow(EF_GRCS_MODEL))
sqrt(336) #RMSE


# For PGSI
EF_PGSI_MODEL<-merge(EF_PGSI,pk_vari)
EF_PGSI_MODEL$avg_rating<-NULL
chart.Correlation(EF_PGSI_MODEL[,-1], histogram = T)

PGSI_m1<- lm(PGSI_TOTSCORE_0_27~.-PKMP.ID, data=EF_PGSI_MODEL)
summary(PGSI_m1)
anova(PGSI_m1)
anova(update(PGSI_m1, .~.-Hands))
anova(update(PGSI_m1, .~.-Hands-Stake))
anova(update(PGSI_m1, .~.-Hands-Stake-WTSD..))
anova(update(PGSI_m1, .~.-Hands-Stake-WTSD..-VPIP))
anova(update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF))
anova(update(PGSI_m1, .~.-Hands-Stake-WTSD..-PFR))
AIC(update(PGSI_m1, .~.-Hands-Stake-WTSD..-VPIP))
AIC(update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF)) # keeper
AIC(update(PGSI_m1, .~.-Hands-Stake-WTSD..-PFR))
anova(update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF-My.C.Won))

AIC(update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF-My.C.Won))
AIC(update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF-My.C.Won-PFR))
AIC(update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF-My.C.Won-All.In.Adj.BB.100))
AIC(update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF-My.C.Won-BB.100))# this is the better
AIC(update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF-My.C.Won-WSD))

anova(PGSI_m1,
      update(PGSI_m1, .~.-Hands),
      update(PGSI_m1, .~.-Hands-Stake),
      update(PGSI_m1, .~.-Hands-Stake-WTSD..),
      update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF),
      update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF-My.C.Won),
      update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF-My.C.Won-BB.100),
      test="Chisq")

# final model
summary(update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF-My.C.Won-BB.100))
plot(update(PGSI_m1, .~.-Hands-Stake-WTSD..-Total.AF-My.C.Won-BB.100))

# lm with leave one out cross validation
cv.lm(data=EF_PGSI_MODEL, form.lm = formula(PGSI_TOTSCORE_0_27 ~ All.In.Adj.BB.100 + VPIP + PFR + WSD + Total.AFq), m= nrow(EF_PGSI_MODEL))
sqrt(4.1) #RMSE

# For SOGS
EF_SOGS_MODEL<-merge(EF_SOGS,pk_vari)
EF_SOGS_MODEL$avg_rating<-NULL
chart.Correlation(EF_SOGS_MODEL[,-1], histogram = T)

SOGS_m1<- lm(SOGS_TOT_0_20~.-PKMP.ID, data=EF_SOGS_MODEL)
summary(SOGS_m1)
anova(SOGS_m1)
anova(update(SOGS_m1,.~.-PFR))
anova(update(SOGS_m1,.~.-PFR-My.C.Won))
anova(update(SOGS_m1,.~.-PFR-My.C.Won-WSD))
anova(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..))
anova(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100))
anova(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100))
anova(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100-Total.AFq))
anova(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100-Total.AFq-Hands))
anova(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100-Total.AFq-Hands-Stake-Total.AF))

AIC(SOGS_m1)
AIC(update(SOGS_m1,.~.-PFR))
AIC(update(SOGS_m1,.~.-PFR-My.C.Won))
AIC(update(SOGS_m1,.~.-PFR-My.C.Won-WSD))
AIC(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..))
AIC(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100))
AIC(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100))
AIC(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100-Total.AFq))
AIC(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100-Total.AFq-Hands))
AIC(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100-Total.AFq-Hands-Stake-Total.AF))


anova(SOGS_m1,
      update(SOGS_m1,.~.-PFR),
      update(SOGS_m1,.~.-PFR-My.C.Won),
      update(SOGS_m1,.~.-PFR-My.C.Won-WSD),
      update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..),
      update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100),
      update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100),
      update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100-Total.AFq),
      update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100-Total.AFq-Hands),
      update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100-Total.AFq-Hands-Stake),
      update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100-Total.AFq-Hands-Stake-Total.AF),
      test="Chisq")
# final model 
summary(update(SOGS_m1,.~.-PFR-My.C.Won-WSD-WTSD..-BB.100-All.In.Adj.BB.100-Total.AFq-Hands-Stake-Total.AF))

# lm with leave one out cross validation
cv.lm(data=EF_SOGS_MODEL, form.lm = formula(SOGS_TOT_0_20 ~ VPIP), m= nrow(EF_SOGS_MODEL))
sqrt(1.66) #RMSE

#########
## SVR ##
#########
# function to remove entries from dopar and allow function train to run properly
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
# IT SHOULD BE CALLED AT THE END OF SVR ANALYSIS 

# For GRCS
## Grid search for best parameter ##
epsilon<- seq(0.0000000001,0.02,length.out=100)
c<-seq(1,100,length.out=10)
parameter<-as.data.frame(expand.grid(epsilon=epsilon,c=c))
performance<-as.data.frame(rep(0.0,nrow(parameter)))
## Parallel processing ##
# check number of cores
ncl<-detectCores()
# use that number of cores to do the paralel processing
cl <- makeCluster(ncl)
registerDoParallel(cl)
## SVM with paralel processing 
acur<-foreach(i=(1:nrow(parameter)),.packages="kernlab", .combine='rbind')%dopar% {
  
  x<-parameter[i,1]
  y<-parameter[i,2]
  
  svm<-ksvm(GRCS_TOT_SCORE_23_161 ~.-PKMP.ID, 
            data=EF_GRCS_MODEL,type="eps-svr",C=y,kernel="rbfdot"
            ,epsilon=list(epsilon=x),scale=T,prob.model=TRUE, cross=23)
  
  performance[i,1]<-(svm@cross)
}
parameter$performance<-unname(acur)
stopCluster(cl)
## checking the best parameter
performance<-na.omit(performance)
parameter<-na.omit(parameter)
bestparameter<-parameter[which(parameter$performance==min(parameter$performance))[1],]
sqrt(bestparameter$performance) #RMSE
# ## Ploting grid search
# y<-parameter$epsilon
# x<-parameter$c
# z<-parameter$performance
# plot3d(x, y, z, col="red", size=3,type = "p",xlab="C", ylab="Epsilon",zlab="MSE")



# For PGSI
## Grid search for best parameter ##
epsilon<- seq(0.0000000001,0.02,length.out=100)
c<-seq(1,100,length.out=10)
parameter<-as.data.frame(expand.grid(epsilon=epsilon,c=c))
performance<-as.data.frame(rep(0.0,nrow(parameter)))
## Parallel processing ##
# check number of cores
ncl<-detectCores()
# use that number of cores to do the paralel processing
cl <- makeCluster(ncl)
registerDoParallel(cl)
## SVM with paralel processing 
acur<-foreach(i=(1:nrow(parameter)),.packages="kernlab", .combine='rbind')%dopar% {
  
  x<-parameter[i,1]
  y<-parameter[i,2]
  
  svm<-ksvm(PGSI_TOTSCORE_0_27 ~.-PKMP.ID, 
            data=EF_PGSI_MODEL,type="eps-svr",C=y,kernel="rbfdot"
            ,epsilon=list(epsilon=x),scale=T,prob.model=TRUE, cross=23)
  
  performance[i,1]<-(svm@cross)
}
parameter$performance<-unname(acur)
stopCluster(cl)
## checking the best parameter
performance<-na.omit(performance)
parameter<-na.omit(parameter)
bestparameter<-parameter[which(parameter$performance==min(parameter$performance))[1],]
sqrt(bestparameter$performance) #RMSE


# For SOGS
## Grid search for best parameter ##
epsilon<- seq(0.0000000001,0.02,length.out=100)
c<-seq(1,100,length.out=10)
parameter<-as.data.frame(expand.grid(epsilon=epsilon,c=c))
performance<-as.data.frame(rep(0.0,nrow(parameter)))
## Parallel processing ##
# check number of cores
ncl<-detectCores()
# use that number of cores to do the paralel processing
cl <- makeCluster(ncl)
registerDoParallel(cl)
## SVM with paralel processing 
acur<-foreach(i=(1:nrow(parameter)),.packages="kernlab", .combine='rbind')%dopar% {
  
  x<-parameter[i,1]
  y<-parameter[i,2]
  
  svm<-ksvm(SOGS_TOT_0_20 ~.-PKMP.ID, 
            data=EF_SOGS_MODEL,type="eps-svr",C=y,kernel="rbfdot"
            ,epsilon=list(epsilon=x),scale=T,prob.model=TRUE, cross=23)
  
  performance[i,1]<-(svm@cross)
}
parameter$performance<-unname(acur)
stopCluster(cl)
## checking the best parameter
performance<-na.omit(performance)
parameter<-na.omit(parameter)
bestparameter<-parameter[which(parameter$performance==min(parameter$performance))[1],]
sqrt(bestparameter$performance) #RMSE


# clean dopar lists
unregister()

###################
## OTHER METHODS ##
###################
# define training control
train_control <- trainControl(method="LOOCV")

DMM<-c("ranger","knn","treebag","xgbTree","rpart")

for(i in 1:length(DMM)){
  x<- train(GRCS_TOT_SCORE_23_161 ~ My.C.Won+Hands+BB.100+All.In.Adj.BB.100+Stake+VPIP+PFR+WTSD..+WSD+Total.AF+Total.AFq,
            data=EF_GRCS_MODEL, trControl=train_control, 
            method=DMM[i])
  assign(paste0(DMM[i],"_GRCS_pk_vari"),x)
  y<- train(PGSI_TOTSCORE_0_27 ~  My.C.Won+Hands+BB.100+All.In.Adj.BB.100+Stake+VPIP+PFR+WTSD..+WSD+Total.AF+Total.AFq,
            data=EF_PGSI_MODEL, trControl=train_control, 
            method=DMM[i])
  assign(paste0(DMM[i],"_PGSI_pk_vari"),y)
  z<- train(SOGS_TOT_0_20 ~  My.C.Won+Hands+BB.100+All.In.Adj.BB.100+Stake+VPIP+PFR+WTSD..+WSD+Total.AF+Total.AFq,
            data=EF_SOGS_MODEL, trControl=train_control, 
            method=DMM[i])
  assign(paste0(DMM[i],"_SOGS_pk_vari"),z)
}

for(i in 1:length(DMM)){
  #print(get(paste0(DMM[i],"_GRCS_pk_vari")))
  #print(get(paste0(DMM[i],"_PGSI_pk_vari")))
  print(get(paste0(DMM[i],"_SOGS_pk_vari")))
}


##############################################
### Can EF variables predict poker skills? ###
##############################################
# Create data base with only relevant variables and numeric predictor
EF_RATING<-merge(cbind(PKMP.ID=pk_ratings$PKMP.ID,Average.rating=as.numeric(pk_ratings$Average.rating)),EF_ALL)
EF_RATING$PGSI_TOTSCORE_0_27<-NULL
EF_RATING$SOGS_TOT_0_20<-NULL
EF_RATING$GRCS_IB_TOT_4_28<-NULL
EF_RATING$GRCS_IS_TOT_5_35<-NULL
EF_RATING$GRCS_PC_TOT_6_42<-NULL
EF_RATING$GRCS_IC_TOT_4_28<-NULL
EF_RATING$GRCS_GE_TOT_4_28<-NULL
EF_RATING$GRCS_TOT_SCORE_23_161<-NULL
EF_RATING$GENDER_M<-NULL
EF_RATING$AGE<-NULL
EF_RATING$EDUCATION<-NULL
EF_RATING[,2]<- as.numeric(as.character(EF_RATING[,2]))

EF_vari_rating<-data.frame()
EF_vari_cat_rating<-data.frame()
for (i in 3:ncol(EF_RATING)){
  if (is.numeric(EF_RATING[,i])){
    x<-cor(EF_RATING[,2],EF_RATING[,i])
    if(!is.na(x)&x>0.45){
      y<-data.frame(EF_variable=names(EF_RATING)[i],Correlation=x,i=i)
      EF_vari_rating<-rbind(EF_vari_rating,y)
    }
  } else{
    z<-ICCbare(EF_RATING[,i],EF_RATING[,2])
    if(!is.na(z)&z>0.45){
      y<-data.frame(EF_variable=names(EF_RATING)[i],ICC=z,i=i)
      EF_vari_cat_rating<-rbind(EF_vari_cat_rating,y)
    }
  }
}
EF_RATING_original<-EF_RATING
EF_RATING<-cbind(PKMP.ID=EF_RATING$PKMP.ID, EF_RATING[,c(as.vector(EF_vari_rating$EF_variable), as.vector(EF_vari_cat_rating$EF_variable))],Average.rating=EF_RATING$Average.rating)

# rattle()
# Rattle does not deal well with numeric response (no SVM, Decision Tree only for binary classification).

# Create data base with only relevant variables and categoric predictor
EF_ABILITY_original<-EF_ABILITY

EF_ABILITY$PGSI_TOTSCORE_0_27<-NULL
EF_ABILITY$SOGS_TOT_0_20<-NULL
EF_ABILITY$GRCS_IB_TOT_4_28<-NULL
EF_ABILITY$GRCS_IS_TOT_5_35<-NULL
EF_ABILITY$GRCS_PC_TOT_6_42<-NULL
EF_ABILITY$GRCS_IC_TOT_4_28<-NULL
EF_ABILITY$GRCS_GE_TOT_4_28<-NULL
EF_ABILITY$GRCS_TOT_SCORE_23_161<-NULL
EF_ABILITY$GENDER_M<-NULL
EF_ABILITY$AGE<-NULL
EF_ABILITY$EDUCATION<-NULL

EF_vari_ability<-data.frame()
EF_vari_cat_ability<-data.frame()
for (i in 3:ncol(EF_ABILITY)){
  if (is.numeric(EF_ABILITY[,i])){
    x<-ICCbare(EF_ABILITY[,2],EF_ABILITY[,i])
    if(!is.na(x)&x>0.40){
      y<-data.frame(EF_variable=names(EF_ABILITY)[i],ICC=x,i=i)
      EF_vari_ability<-rbind(EF_vari_ability,y)
    }
  } else{
    z<-assocstats(ftable(EF_ABILITY[,2],EF_ABILITY[,i]))$cramer
    if(!is.na(z)&z>0.40){
      y<-data.frame(EF_variable=names(EF_ABILITY)[i],CramerV=z,i=i)
      EF_vari_cat_ability<-rbind(EF_vari_cat_ability,y)
    }
  }
}
EF_ABILITY_original<-EF_ABILITY
EF_ABILITY<-cbind(PKMP.ID=EF_ABILITY$PKMP.ID, EF_ABILITY[,c(as.vector(EF_vari_ability$EF_variable), as.vector(EF_vari_cat_ability$EF_variable))],poker_ability=EF_ABILITY$poker_ability)

# rattle()
# Rattle does not deal with response with multiple classes.

#### MODEL BUILDING (for EF_RATING) ####
##################
## LINEAR MODEL ##
##################
rating_m1<-lm(Average.rating~.-PKMP.ID, data=EF_RATING)
summary(rating_m1)
anova(rating_m1)
anova(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE))
anova(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129))
anova(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79))
anova(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA))
anova(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89))
anova(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89-EQIS_GMEQ_VHI_120_129))
anova(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89-EQIS_GMEQ_VHI_120_129-TOL_2_TOT_MOVE_2))
anova(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89-EQIS_GMEQ_VHI_120_129-TOL_2_TOT_MOVE_2-EQIS_ADEQ_LOW_80_89))
# removing EQIS_ADEQ_LOW_80_89 increased the residuals and the AIC
AIC(rating_m1)
AIC(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE))
AIC(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129))
AIC(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79))
AIC(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA))
AIC(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89))
AIC(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89-EQIS_GMEQ_VHI_120_129))
AIC(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89-EQIS_GMEQ_VHI_120_129-TOL_2_TOT_MOVE_2))
AIC(update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89-EQIS_GMEQ_VHI_120_129-TOL_2_TOT_MOVE_2-EQIS_ADEQ_LOW_80_89))


# check if reduced models are the same as bigger model
# if p-value higher than 0.05, the models are not statistically distinct
anova(rating_m1,
      update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE),
      update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129),
      update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79),
      update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA),
      update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89),
      update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89-EQIS_GMEQ_VHI_120_129),
      update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89-EQIS_GMEQ_VHI_120_129-TOL_2_TOT_MOVE_2),
      update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89-EQIS_GMEQ_VHI_120_129-TOL_2_TOT_MOVE_2-EQIS_ADEQ_LOW_80_89),
      test="Chisq")
# model without EQIS_ADEQ_LOW_80_89 is statistically distinct from bigger models, therefore we keep this variable
rating_m2<-update(rating_m1, .~.-TOL_7_TOT_TSEC_EXE-EQIS_INTEREQ_VHI_120_129-EQIS__PIEQ_VLW_70_79-TOL_2_TOT_FA-EQIS_TOTEQ_LOW_80_89-EQIS_GMEQ_VHI_120_129-TOL_2_TOT_MOVE_2-EQIS_ADEQ_LOW_80_89)
summary(rating_m2)
plot(resid(rating_m2)~predict(rating_m2))
plot(EF_RATING$Average.rating~predict(rating_m2))
#RMSE
sqrt(mean(rating_m2$residuals^2))
# rmse: sqrt(c(crossprod(rating_m2$residuals))/length(rating_m2$residuals))
# standard error: sqrt(c(crossprod(rating_m2$residuals))/rating_m2$df.residual)

# lm with leave one out cross validation
cv.lm(data=EF_RATING, form.lm = formula(Average.rating ~ SPM_D_T_SEC + FLUS_FR_TOT_ER + EQIS_INTER_EQ_STD_50_150 + EQIS_SM_EQ_STD_50_150), m= nrow(EF_RATING))
sqrt(336) #RMSE

#########
## SVR ##
#########
## Grid search for best parameter ##
epsilon<- seq(0.0000000001,0.02,length.out=100)
c<-seq(1,100,length.out=10)
parameter<-as.data.frame(expand.grid(epsilon=epsilon,c=c))
performance<-as.data.frame(rep(0.0,nrow(parameter)))
## Parallel processing ##
# check number of cores
ncl<-detectCores()
# use that number of cores to do the paralel processing
cl <- makeCluster(ncl)
registerDoParallel(cl)
## SVM with paralel processing 
acur<-foreach(i=(1:nrow(parameter)),.packages="kernlab", .combine='rbind')%dopar% {
  
  x<-parameter[i,1]
  y<-parameter[i,2]
  
  svm<-ksvm(Average.rating ~.-PKMP.ID-TOL_2_TOT_FA-TOL_2_TOT_MOVE_2, 
            data=EF_RATING,type="eps-svr",C=y,kernel="rbfdot"
            ,epsilon=list(epsilon=x),scale=T,prob.model=TRUE, cross=23)
  
  performance[i,1]<-(svm@cross)
}
parameter$performance<-unname(acur)
stopCluster(cl)
## checking the best parameter
performance<-na.omit(performance)
parameter<-na.omit(parameter)
bestparameter<-parameter[which(parameter$performance==min(parameter$performance))[1],]
sqrt(bestparameter$performance) #RMSE
# clean dopar lists
unregister()

#########################################
## DECISION TREE AND TREE BASED MODELS ##
#########################################
# define training control
train_control <- trainControl(method="LOOCV")
## Decision Tree ##
# train the model
DT_EF<- train(Average.rating ~ SPM_D_T_SEC+FLUS_FR_TOT_ER+TOL_7_TOT_TSEC_EXE+EQIS_INTER_EQ_STD_50_150+EQIS_SM_EQ_STD_50_150+EQIS_TOTEQ_LOW_80_89+EQIS_INTEREQ_VHI_120_129+EQIS_ADEQ_LOW_80_89+EQIS_GMEQ_VHI_120_129+EQIS__PIEQ_VLW_70_79,
              data=EF_RATING, trControl=train_control, 
              method="rpart")
print(DT_EF)

## xgBoost ##
# train the model
xgb_EF<- train(Average.rating ~ SPM_D_T_SEC+FLUS_FR_TOT_ER+TOL_7_TOT_TSEC_EXE+EQIS_INTER_EQ_STD_50_150+EQIS_SM_EQ_STD_50_150+EQIS_TOTEQ_LOW_80_89+EQIS_INTEREQ_VHI_120_129+EQIS_ADEQ_LOW_80_89+EQIS_GMEQ_VHI_120_129+EQIS__PIEQ_VLW_70_79,
               data=EF_RATING, trControl=train_control, 
               method="xgbTree")
print(xgb_EF)

## Random Forest for EF_RATING ##
# train the model
RF_EF<- train(Average.rating ~ SPM_D_T_SEC+FLUS_FR_TOT_ER+TOL_7_TOT_TSEC_EXE+EQIS_INTER_EQ_STD_50_150+EQIS_SM_EQ_STD_50_150+EQIS_TOTEQ_LOW_80_89+EQIS_INTEREQ_VHI_120_129+EQIS_ADEQ_LOW_80_89+EQIS_GMEQ_VHI_120_129+EQIS__PIEQ_VLW_70_79,
              data=EF_RATING, trControl=train_control, 
              method="ranger")
print(RF_EF)

## bagging CART for EF_RATING ##
# train the model
bC_EF<- train(Average.rating ~ SPM_D_T_SEC+FLUS_FR_TOT_ER+TOL_7_TOT_TSEC_EXE+EQIS_INTER_EQ_STD_50_150+EQIS_SM_EQ_STD_50_150+EQIS_TOTEQ_LOW_80_89+EQIS_INTEREQ_VHI_120_129+EQIS_ADEQ_LOW_80_89+EQIS_GMEQ_VHI_120_129+EQIS__PIEQ_VLW_70_79,
              data=EF_RATING, trControl=train_control, 
              method="treebag")
print(bC_EF)


#########
## kNN ##
#########
# train the model
kNN_EF<- train(Average.rating ~ SPM_D_T_SEC+FLUS_FR_TOT_ER+TOL_7_TOT_TSEC_EXE+EQIS_INTER_EQ_STD_50_150+EQIS_SM_EQ_STD_50_150+EQIS_TOTEQ_LOW_80_89+EQIS_INTEREQ_VHI_120_129+EQIS_ADEQ_LOW_80_89+EQIS_GMEQ_VHI_120_129+EQIS__PIEQ_VLW_70_79,
               data=EF_RATING, trControl=train_control, 
               method="knn")
print(kNN_EF)






##################################################
### Can EF variables predict problem gambling? ###
##################################################

# Create reduced data set for each gambling test
EF_GAMBLING<-EF_ALL
EF_GAMBLING$GRCS_IB_TOT_4_28<-NULL
EF_GAMBLING$GRCS_IS_TOT_5_35<-NULL
EF_GAMBLING$GRCS_PC_TOT_6_42<-NULL
EF_GAMBLING$GRCS_IC_TOT_4_28<-NULL
EF_GAMBLING$GRCS_GE_TOT_4_28<-NULL
EF_GAMBLING$GENDER_M<-NULL
EF_GAMBLING$AGE<-NULL
EF_GAMBLING$EDUCATION<-NULL

# For GRCS
EF_vari_GRCS<-data.frame()
EF_vari_cat_GRCS<-data.frame()
for (i in 2:300){
  if (is.numeric(EF_GAMBLING[,i])){
    x<-cor(EF_GAMBLING[,301],EF_GAMBLING[,i])
    if(!is.na(x)&x>0.45){
      y<-data.frame(EF_variable=names(EF_GAMBLING)[i],Correlation=x,i=i)
      EF_vari_GRCS<-rbind(EF_vari_GRCS,y)
    }
  } else{
    z<-ICCbare(EF_GAMBLING[,i],EF_GAMBLING[,301])
    if(!is.na(z)&z>0.45){
      y<-data.frame(EF_variable=names(EF_GAMBLING)[i],ICC=z,i=i)
      EF_vari_cat_GRCS<-rbind(EF_vari_cat_GRCS,y)
    }
  }
}
EF_GAMBLING_GRCS<-cbind(PKMP.ID=as.character(EF_GAMBLING$PKMP.ID), EF_GAMBLING[,c(as.vector(EF_vari_GRCS$EF_variable), as.vector(EF_vari_cat_GRCS$EF_variable))],GRCS_TOT_SCORE_23_161=EF_GAMBLING$GRCS_TOT_SCORE_23_161)

# For PGSI
EF_vari_PGSI<-data.frame()
EF_vari_cat_PGSI<-data.frame()
for (i in 2:300){
  if (is.numeric(EF_GAMBLING[,i])){
    x<-cor(EF_GAMBLING[,303],EF_GAMBLING[,i])
    if(!is.na(x)&x>0.45){
      y<-data.frame(EF_variable=names(EF_GAMBLING)[i],Correlation=x,i=i)
      EF_vari_PGSI<-rbind(EF_vari_PGSI,y)
    }
  } else{
    z<-ICCbare(EF_GAMBLING[,i],EF_GAMBLING[,303])
    if(!is.na(z)&z>0.45){
      y<-data.frame(EF_variable=names(EF_GAMBLING)[i],ICC=z,i=i)
      EF_vari_cat_PGSI<-rbind(EF_vari_cat_PGSI,y)
    }
  }
}
EF_GAMBLING_PGSI<-cbind(PKMP.ID=as.character(EF_GAMBLING$PKMP.ID), EF_GAMBLING[,c(as.vector(EF_vari_PGSI$EF_variable), as.vector(EF_vari_cat_PGSI$EF_variable))],PGSI_TOTSCORE_0_27=EF_GAMBLING$PGSI_TOTSCORE_0_27)

# For SOGS
EF_vari_SOGS<-data.frame()
EF_vari_cat_SOGS<-data.frame()
for (i in 2:300){
  if (is.numeric(EF_GAMBLING[,i])){
    x<-cor(EF_GAMBLING[,302],EF_GAMBLING[,i])
    if(!is.na(x)&x>0.45){
      y<-data.frame(EF_variable=names(EF_GAMBLING)[i],Correlation=x,i=i)
      EF_vari_SOGS<-rbind(EF_vari_SOGS,y)
    }
  } else{
    z<-ICCbare(EF_GAMBLING[,i],EF_GAMBLING[,302])
    if(!is.na(z)&z>0.45){
      y<-data.frame(EF_variable=names(EF_GAMBLING)[i],ICC=z,i=i)
      EF_vari_cat_SOGS<-rbind(EF_vari_cat_SOGS,y)
    }
  }
}
EF_GAMBLING_SOGS<-cbind(PKMP.ID=as.character(EF_GAMBLING$PKMP.ID), EF_GAMBLING[,c(as.vector(EF_vari_SOGS$EF_variable), as.vector(EF_vari_cat_SOGS$EF_variable))],SOGS_TOT_0_20=EF_GAMBLING$SOGS_TOT_0_20)


##################
## LINEAR MODEL ##
##################

# For GRCS
GRCS_lm1<- lm(GRCS_TOT_SCORE_23_161~.-PKMP.ID, data=EF_GAMBLING_GRCS)
summary(GRCS_lm1)
anova(GRCS_lm1)
anova(update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN))
anova(update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG))
anova(update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79))
anova(update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79-RSBQ_TOT_CERTCHOICE_VS_RISK_0_18))
anova(update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79-RSBQ_TOT_CERTCHOICE_VS_RISK_0_18-FLUF_ALLER_RIP))
# keep FLUF_ALLER_RIP


anova(GRCS_lm1,
      update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN),
      update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG),
      update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79),
      update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79-RSBQ_TOT_CERTCHOICE_VS_RISK_0_18),
      update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79-RSBQ_TOT_CERTCHOICE_VS_RISK_0_18-FLUF_P_ER_RIP),
      test="Chisq")

anova(GRCS_lm1,
      update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN),
      update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG),
      update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79),
      update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79-RSBQ_TOT_CERTCHOICE_VS_RISK_0_18),
      update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79-RSBQ_TOT_CERTCHOICE_VS_RISK_0_18-FLUF_ALLER_RIP),
      test="Chisq")
AIC(update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79-RSBQ_TOT_CERTCHOICE_VS_RISK_0_18))
AIC(update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79-RSBQ_TOT_CERTCHOICE_VS_RISK_0_18-FLUF_ALLER_RIP))
AIC(update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79-RSBQ_TOT_CERTCHOICE_VS_RISK_0_18-FLUF_P_ER_RIP))
# last model has the lowest AIC

# final model
summary(update(GRCS_lm1, .~.-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79-RSBQ_TOT_CERTCHOICE_VS_RISK_0_18-FLUF_P_ER_RIP))

# lm with leave one out cross validation
cv.lm(data=EF_GAMBLING_GRCS, form.lm = formula(GRCS_TOT_SCORE_23_161 ~ FLUF_ALLER_RIP + IBQ_L_PAY_OVERNIGHT_SHIPPING), m= nrow(EF_GAMBLING_GRCS))
sqrt(188) #RMSE

# For PGSI
PGSI_lm1<- lm(PGSI_TOTSCORE_0_27~.-PKMP.ID, data=EF_GAMBLING_PGSI)
summary(PGSI_lm1)
anova(PGSI_lm1)
anova(update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-TOL_ALLFA_0_36))
summary(update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-TOL_ALLFA_0_36))
anova(update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36))
anova(update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS))
anova(update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE))
anova(update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1))
anova(update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1-TOL_ALLTRY_12_0_36))
anova(update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1-TOL_ALLTRY_12_0_36-TOL_4_NUMBTRY_0_3))
anova(update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1-TOL_ALLTRY_12_0_36-TOL_4_NUMBTRY_0_3-TOL_4_TOT_FA))
anova(update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1-TOL_ALLTRY_12_0_36-TOL_4_NUMBTRY_0_3-TOL_4_TOT_FA-EQIS_SMEQ_VLW_70_79))


anova(PGSI_lm1,
      update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-TOL_ALLFA_0_36),
      update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36),
      update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS),
      update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE),
      update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1),
      update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1-TOL_ALLTRY_12_0_36),
      update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1-TOL_ALLTRY_12_0_36-TOL_4_NUMBTRY_0_3),
      update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1-TOL_ALLTRY_12_0_36-TOL_4_NUMBTRY_0_3-TOL_4_TOT_FA),
      update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1-TOL_ALLTRY_12_0_36-TOL_4_NUMBTRY_0_3-TOL_4_TOT_FA-EQIS_SMEQ_VLW_70_79),
      update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1-TOL_ALLTRY_12_0_36-TOL_4_NUMBTRY_0_3-TOL_4_TOT_FA-EQIS_SMEQ_VLW_70_79-EQUIS_SMEQ_XHI_.130),
      test="Chisq")
# best model
summary(update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1-TOL_ALLTRY_12_0_36-TOL_4_NUMBTRY_0_3-TOL_4_TOT_FA))
# final model
summary(update(PGSI_lm1, .~.-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-TOL_ALLFA_0_36-RSBQ_P_100_CERTLOSS-TOL_TSEC_ALLEXE-FDCT_MN_IND_0_1-TOL_ALLTRY_12_0_36-TOL_4_NUMBTRY_0_3-TOL_4_TOT_FA-EQIS_SMEQ_VLW_70_79-EQUIS_SMEQ_XHI_.130))


# lm with leave one out cross validation
# had two remove two variables which are important for the model EQIS_SMEQ_VLW_70_79 and EQUIS_SMEQ_XHI_.130, because they would turn into constants after removing one obs
cv.lm(data=EF_GAMBLING_PGSI, form.lm = formula(PGSI_TOTSCORE_0_27 ~ SPM_D_T_SEC + TOL_5_TOT_TSEC_EXE + MAX_DAILYBET_100_1.000EUR ), m= nrow(EF_GAMBLING_PGSI))
sqrt(1.73) #RMSE


# For SOGS
SOGS_lm1<- lm(SOGS_TOT_0_20~.-PKMP.ID, data=EF_GAMBLING_SOGS)
summary(SOGS_lm1)
anova(SOGS_lm1)
anova(update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS))
anova(update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG))
anova(update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119))
anova(update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER))
summary(update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER))
anova(update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER-RSBQ_D_75._200_GAIN_HEVG))
summary(update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER-RSBQ_D_75._200_GAIN_HEVG))
anova(update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER-RSBQ_D_75._200_GAIN_HEVG-FAB_LEX_FLU_S_ER_VAR))
summary(update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER-RSBQ_D_75._200_GAIN_HEVG-FAB_LEX_FLU_S_ER_VAR))
anova(update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER-RSBQ_D_75._200_GAIN_HEVG-FAB_LEX_FLU_S_ER_VAR-EQIS_SMEQ_VLW_70_79))

anova(SOGS_lm1,
      update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS),
      update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG),
      update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119),
      update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER),
      update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER-RSBQ_D_75._200_GAIN_HEVG),
      update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER-RSBQ_D_75._200_GAIN_HEVG-FAB_LEX_FLU_S_ER_VAR),
      update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER-RSBQ_D_75._200_GAIN_HEVG-FAB_LEX_FLU_S_ER_VAR-EQIS_SMEQ_VLW_70_79),
      test="Chisq")
# best model
summary(update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER-RSBQ_D_75._200_GAIN_HEVG-FAB_LEX_FLU_S_ER_VAR))
#final model
summary(update(SOGS_lm1, .~.-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-FLUF_L_TOT_ER-RSBQ_D_75._200_GAIN_HEVG-FAB_LEX_FLU_S_ER_VAR-EQIS_SMEQ_VLW_70_79))

# lm with leave one out cross validation
# had to remove a variable with only one obs in one category
cv.lm(data=EF_GAMBLING_SOGS, form.lm = formula(SOGS_TOT_0_20 ~ SPM_A_COR_0_12 + SPM_B_COR_0_12 + 
                                                 FLUF_L_ER_VAR + RSBQ_D_100_CERTGAIN  + 
                                                 EQIS_SMEQ_HI_110_119 + EQIS__MED_90_109), m= nrow(EF_GAMBLING_SOGS))
sqrt(0.53) #RMSE

#########
## SVR ##
#########
## For GRCS
## Grid search for best parameter ##
epsilon<- seq(0.0000000001,0.02,length.out=100)
c<-seq(1,100,length.out=10)
parameter<-as.data.frame(expand.grid(epsilon=epsilon,c=c))
performance<-as.data.frame(rep(0.0,nrow(parameter)))
## Parallel processing ##
# check number of cores
ncl<-detectCores()
# use that number of cores to do the paralel processing
cl <- makeCluster(ncl)
registerDoParallel(cl)
## SVM with paralel processing 
# have to remove from analysis variables with only one obs in a category
acur<-foreach(i=(1:nrow(parameter)),.packages="kernlab", .combine='rbind')%dopar% {
  
  x<-parameter[i,1]
  y<-parameter[i,2]
  
  svm<-ksvm(GRCS_TOT_SCORE_23_161 ~.-PKMP.ID-RSBQ_B_100_CERTGAIN-RSBQ_B_90._500_GAIN_HEVG-EQIS_INTRA_VLW_70_79, 
            data=EF_GAMBLING_GRCS,type="eps-svr",C=y,kernel="rbfdot"
            ,epsilon=list(epsilon=x),scale=T,prob.model=TRUE, cross=23)
  
  performance[i,1]<-(svm@cross)
}
parameter$performance<-unname(acur)
stopCluster(cl)
## checking the best parameter
performance<-na.omit(performance)
parameter<-na.omit(parameter)
bestparameter<-parameter[which(parameter$performance==min(parameter$performance))[1],]
sqrt(bestparameter$performance) #RMSE

# For GRCS WITH SAME DATA AS LM 
## Grid search for best parameter ##
epsilon<- seq(0.0000000001,0.02,length.out=100)
c<-seq(1,100,length.out=10)
parameter<-as.data.frame(expand.grid(epsilon=epsilon,c=c))
performance<-as.data.frame(rep(0.0,nrow(parameter)))
## Parallel processing ##
# check number of cores
ncl<-detectCores()
# use that number of cores to do the paralel processing
cl <- makeCluster(ncl)
registerDoParallel(cl)
## SVM with paralel processing 
# have to remove from analysis variables with only one obs in a category
acur<-foreach(i=(1:nrow(parameter)),.packages="kernlab", .combine='rbind')%dopar% {
  
  x<-parameter[i,1]
  y<-parameter[i,2]
  
  svm<-ksvm(GRCS_TOT_SCORE_23_161 ~ FLUF_ALLER_RIP + IBQ_L_PAY_OVERNIGHT_SHIPPING, 
            data=EF_GAMBLING_GRCS,type="eps-svr",C=y,kernel="rbfdot"
            ,epsilon=list(epsilon=x),scale=T,prob.model=TRUE, cross=23)
  
  performance[i,1]<-(svm@cross)
}
parameter$performance<-unname(acur)
stopCluster(cl)
## checking the best parameter
performance<-na.omit(performance)
parameter<-na.omit(parameter)
bestparameter<-parameter[which(parameter$performance==min(parameter$performance))[1],]
sqrt(bestparameter$performance) #RMSE



## For PGSI
## Grid search for best parameter ##
epsilon<- seq(0.0000000001,0.02,length.out=100)
c<-seq(1,100,length.out=10)
parameter<-as.data.frame(expand.grid(epsilon=epsilon,c=c))
performance<-as.data.frame(rep(0.0,nrow(parameter)))
## Parallel processing ##
# check number of cores
ncl<-detectCores()
# use that number of cores to do the paralel processing
cl <- makeCluster(ncl)
registerDoParallel(cl)
## SVM with paralel processing 
# have to remove from analysis variables with only one obs in a category
acur<-foreach(i=(1:nrow(parameter)),.packages="kernlab", .combine='rbind')%dopar% {
  
  x<-parameter[i,1]
  y<-parameter[i,2]
  
  svm<-ksvm(PGSI_TOTSCORE_0_27 ~.-PKMP.ID-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-EQIS_SMEQ_VLW_70_79-EQUIS_SMEQ_XHI_.130, 
            data=EF_GAMBLING_PGSI,type="eps-svr",C=y,kernel="rbfdot"
            ,epsilon=list(epsilon=x),scale=T,prob.model=TRUE, cross=23)
  
  performance[i,1]<-(svm@cross)
}
parameter$performance<-unname(acur)
stopCluster(cl)
## checking the best parameter
performance<-na.omit(performance)
parameter<-na.omit(parameter)
bestparameter<-parameter[which(parameter$performance==min(parameter$performance))[1],]
sqrt(bestparameter$performance) #RMSE


## For SOGS
## Grid search for best parameter ##
epsilon<- seq(0.0000000001,0.02,length.out=100)
c<-seq(1,100,length.out=10)
parameter<-as.data.frame(expand.grid(epsilon=epsilon,c=c))
performance<-as.data.frame(rep(0.0,nrow(parameter)))
## Parallel processing ##
# check number of cores
ncl<-detectCores()
# use that number of cores to do the paralel processing
cl <- makeCluster(ncl)
registerDoParallel(cl)
## SVM with paralel processing 
# have to remove from analysis variables with only one obs in a category
acur<-foreach(i=(1:nrow(parameter)),.packages="kernlab", .combine='rbind')%dopar% {
  
  x<-parameter[i,1]
  y<-parameter[i,2]
  
  svm<-ksvm(SOGS_TOT_0_20 ~.-PKMP.ID-RSBQ_P_100_CERTLOSS-RSBQ_P_50._300_LOSS_LEVG-EQIS_INTRA_HI_110_119-EQIS_SMEQ_VLW_70_79, 
            data=EF_GAMBLING_SOGS,type="eps-svr",C=y,kernel="rbfdot"
            ,epsilon=list(epsilon=x),scale=T,prob.model=TRUE, cross=23)
  
  performance[i,1]<-(svm@cross)
}
parameter$performance<-unname(acur)
stopCluster(cl)
## checking the best parameter
performance<-na.omit(performance)
parameter<-na.omit(parameter)
bestparameter<-parameter[which(parameter$performance==min(parameter$performance))[1],]
sqrt(bestparameter$performance) #RMSE
# clean dopar entries
unregister()

###################
## OTHER METHODS ##
###################
# ranger -> random forest
# knn
# treebag -> bagged CART
# xgbTree -> xgboost
# rpart -> decision tree

# define methods
DMM<-c("ranger","knn","treebag","xgbTree","rpart")
# define training control
train_control <- trainControl(method="LOOCV")

for(i in 1:length(DMM)){
  x<- train(GRCS_TOT_SCORE_23_161 ~ FLUF_P_ER_RIP+FLUF_ALLER_RIP+IBQ_L_PAY_OVERNIGHT_SHIPPING
            +RSBQ_B_100_CERTGAIN+RSBQ_B_90._500_GAIN_HEVG+RSBQ_TOT_CERTCHOICE_VS_RISK_0_18
            +EQIS_INTRA_VLW_70_79,
            data=EF_GAMBLING_GRCS, trControl=train_control, 
            method=DMM[i])
  assign(paste0(DMM[i],"_GRCS"),x)
  y<- train(PGSI_TOTSCORE_0_27 ~ SPM_D_T_SEC+TOL_4_NUMBTRY_0_3+TOL_4_TOT_FA
            +TOL_5_TOT_TSEC_EXE+TOL_TSEC_ALLEXE+TOL_ALLTRY_12_0_36
            +TOL_ALLFA_0_36+MAX_DAILYBET_100_1.000EUR+FDCT_MN_IND_0_1+RSBQ_P_100_CERTLOSS
            +RSBQ_P_50._300_LOSS_LEVG+EQIS_INTRA_HI_110_119+EQIS_SMEQ_VLW_70_79+EQUIS_SMEQ_XHI_.130,
            data=EF_GAMBLING_PGSI, trControl=train_control, 
            method=DMM[i])
  assign(paste0(DMM[i],"_PGSI"),y)
  z<- train(SOGS_TOT_0_20 ~ FAB_LEX_FLU_S_ER_VAR+SPM_A_COR_0_12+SPM_B_COR_0_12
            +FLUF_L_ER_VAR+FLUF_L_TOT_ER+RSBQ_D_100_CERTGAIN
            +RSBQ_D_75._200_GAIN_HEVG+RSBQ_P_100_CERTLOSS+RSBQ_P_50._300_LOSS_LEVG
            +EQIS_INTRA_HI_110_119+EQIS_SMEQ_VLW_70_79+EQIS_SMEQ_HI_110_119+EQIS__MED_90_109,
            data=EF_GAMBLING_SOGS, trControl=train_control, 
            method=DMM[i])
  assign(paste0(DMM[i],"_SOGS"),z)
}

for(i in 1:length(DMM)){
  print(get(paste0(DMM[i],"_GRCS")))
  # print(get(paste0(DMM[i],"_PGSI")))
  # print(get(paste0(DMM[i],"_SOGS")))
}

# For GRCS WITH SAME DATA AS LM
# define methods
DMM<-c("ranger","knn","treebag","xgbTree","rpart")
# define training control
train_control <- trainControl(method="LOOCV")

for(i in 1:length(DMM)){
  x<- train(GRCS_TOT_SCORE_23_161 ~ FLUF_ALLER_RIP + IBQ_L_PAY_OVERNIGHT_SHIPPING,
            data=EF_GAMBLING_GRCS, trControl=train_control, 
            method=DMM[i])
  assign(paste0(DMM[i],"_GRCS_LM"),x)
}

for(i in 1:length(DMM)){
  print(get(paste0(DMM[i],"_GRCS_LM")))
}



