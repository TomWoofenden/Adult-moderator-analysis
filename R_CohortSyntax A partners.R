############################# EarlyCause T3.1.2 - Investigation of the relationship between childhood maltreatment and PCM multimorbidity in adult cohorts #############################

# Note1: The data needs to be uploaded in wide format with one case per row and the different variables in columns
# Note2: Please, keep the same order of the predictors when building the models as in the code below
# Note3: When running the function on the models, warning messages about the vif.default(model) and argument returning NA are to be expected


#################### 0. General settings ####################

# Set working directory, adjust with your own pathway
setwd("./results")

# Upload packages
library('foreign')
library('table1')
library('tidyverse')
library('dplyr')
library('gt')
library('plyr')
library('nnet')
library('pscl')
library('broom')

#TW This is a file which is modified by UoB, with the inlcusion of ALSPAC partner data 

#################### 1. Load datasets at baseline #################### 
#TW adding 
# descriptives_T0 <- read.spss('N1_A100R.sav',            #Upload descriptives data (sex, age, education)
#                              use.value.labels=FALSE,
#                              to.data.frame=TRUE,
#                              max.value.labels=Inf)
# 
# smoking_T0 <- read.spss('N1_A200D2 (various smoking history variables).sav',    #Upload smoking data
#                         use.value.labels=FALSE,
#                         to.data.frame=TRUE,
#                         max.value.labels=Inf)
# 
# drinking <- read.spss('N1_A202R.sav',                   #Upload drinking data
#                       use.value.labels=FALSE,
#                       to.data.frame=TRUE,
#                       max.value.labels=Inf)
# 
# activity_T0 <- read.spss('N1_A206R.sav',                #Upload physical activity data
#                          use.value.labels=FALSE,
#                          to.data.frame=TRUE,
#                          max.value.labels=Inf)
# 
# CMD_T0 <- read.spss('N1_A250R.sav',                     #Upload cardiometabolic disease data
#                     use.value.labels=FALSE,
#                     to.data.frame=TRUE,
#                     max.value.labels=Inf)
#   
# NEMESIS_T0 <- read.spss('N1_A252D_updated.sav',         #Upload childhood maltreatment data
#                         use.value.labels=FALSE,
#                         to.data.frame=TRUE,
#                         max.value.labels=Inf)
# 
# CTTypes_T0 <- read.spss('N1_A252R.sav',                 #Upload childhood maltreatment types data
#                         use.value.labels=FALSE,
#                         to.data.frame=TRUE,
#                         max.value.labels=Inf)
# 
# CIDIDep_T0 <- read.spss('N1_A256D.sav',                 #Upload depression diagnosis data
#                         use.value.labels=FALSE,
#                         to.data.frame=TRUE,
#                         max.value.labels=Inf)
# 
# CIDIDepDerived_T0 <- read.spss('N1_A257D.sav',          #Upload derived depression diagnosis data
#                                use.value.labels=FALSE,
#                                to.data.frame=TRUE,
#                                max.value.labels=Inf)
# 
# Medicine_T0 <- read.spss('N1_A354D.sav',                 #Upload medicine intake data
#                          use.value.labels=FALSE,
#                          to.data.frame=TRUE,
#                          max.value.labels=Inf)
# 
# MedicineRaw_T0 <- read.spss('N1_A354R.sav',              #Upload raw medicine data
#                             use.value.labels=FALSE,
#                             to.data.frame=TRUE,
#                             max.value.labels=Inf)
# 
# PsychoPharma_T0 <- read.spss('N1_A355R_new.sav',         #Upload psychopharmacotherapy
#                             use.value.labels=FALSE,
#                             to.data.frame=TRUE,
#                             max.value.labels=Inf)
# 
# 
# 
# #################### 2. Rename ID column of all baseline datasets to 'pident' #################### 
# 
# list <- list(descriptives_T0,
#              NEMESIS_T0,
#              CTTypes_T0,
#              smoking_T0,
#              drinking,
#              activity_T0,
#              CMD_T0,
#              CIDIDep_T0,
#              CIDIDepDerived_T0,
#              Medicine_T0,
#              MedicineRaw_T0,
#              PsychoPharma_T0)
# 
# list_data <- lapply(list, function(x) {colnames(x)[1] <- 'pident'; x})
# 
# 
# 
# #################### 3. Merge all datasets #################### 
# 
# Data <- Reduce(function(dft1,dft2) merge(dft1, dft2, by='pident', all=TRUE),
#                x = list_data)

Data <- read.spss("M:\\projects\\ieu2\\p5\\066\\working\\data\\results\\CRP_EWAS_CHARGE\\epiPRS\\EarlyCause\\SPSS\\EarlyCause.sav",            #Upload descriptives data (sex, age, education)
                  use.value.labels=FALSE,
                  to.data.frame=TRUE,
                  max.value.labels=Inf)

# ALSPAC: check for duplicates
table(duplicated(Data[,c("aln")]))

Data=Data %>%
  distinct(aln, .keep_all = TRUE)

table(duplicated(Data[,c("aln")]))


#################### 4. Code all variables #################### 

# 4.1. Code childhood maltreatment variables (cf. Appendix A of Analysis Plan)
#alspac physical abuse variables: pf2062, pb466a, these have different outcome values, so i'm recoding before including in same dataframe.

# EW: edited some R basics; the code below does not make sense
# pf2062 <- if (pf2062 == "1"){print("1")} else if (pf2062 == 2){print("1")} else if (pf2062 == 3){print("0")} else {print("NA")}
# pb466a <- if (pb466a == "1"){print("1")} else if (pb466a == 2){print("0")} else {print("NA")}
# APAC_PA <- data.frame(pf2062, pb466a)
# Data$CM_PA <- APAC_PA

Data$CM_PA <- as.factor(ifelse(Data$pf2062 == 1 | Data$pf2062 == 2 | Data$pb466a == 1, 1, 
                               ifelse(Data$pf2062 == 3 & Data$pb466a == 2, 0, NA)))


Data$CM_EA <- as.factor(ifelse(Data$pb470a == 1 | Data$pb470a == 1, 1, ifelse(Data$pb470a == 2 & Data$pb470a == 2, 0, NA)))

Data$CM_SA <- as.factor(ifelse(Data$pb472a == 1 | Data$pb472a == 1, 1, ifelse(Data$pb472a == 2 & Data$pb472a == 2, 0, NA)))


Data$CM <- as.factor(ifelse(Data$CM_PA == 1 | Data$CM_EA == 1 | Data$CM_SA == 1, 1,
             ifelse(Data$CM_PA == 0 & Data$CM_EA == 0 & Data$CM_SA == 0, 0, NA)))

Data$CM_sev <- as.factor(rowSums(data.frame(as.numeric(as.character(Data$CM_PA)),
                                  as.numeric(as.character(Data$CM_EA)),
                                  as.numeric(as.character(Data$CM_SA)))))

# 4.2. Code depression-related variables (cf. Appendix B of Analysis Plan)
#if pb261 (EPDS total) is equal or bigger than 13 mdd = 1, less than = 0 


 
Data$MDD <- as.factor(ifelse(Data$fa3255 >= 13, 1, ifelse(Data$fa3255 < 13 & Data$fa3255 >= 0, 0, NA)))

# EW: already dichotomized
#Data$AntidepMed <- as.factor(Data$fa5404 == 1 | Data$fa5404 == 2 | Data$fa5404 == 3, 1, ifelse(Data$fa5404 == 4, 0, NA))
Data$AntidepMed <- as.factor(Data$fa5404)
                             
Data$MDD_med <- as.factor(ifelse(Data$MDD == 0 & Data$AntidepMed == 0, 0,
                       ifelse(Data$MDD == 1 | Data$AntidepMed == 1, 1, NA)))

# 4.3. Code cardiometabolic-related variables (cf. Appendix C of Analysis Plan)

Data$CD <- as.factor(ifelse(Data$fa5000 == 1, 1,
                  ifelse(Data$fa5000 == 2, 0, NA)))

Data$CVD1 <- as.factor(ifelse(Data$fa5100 == 1, 1,
                    ifelse(Data$fa5100 == 2, 0, NA)))

#here added varibale not inlucded in Camilles plan, as this was blank for partners (EW: that makes sense, yes?)
# EW: minor edit
Data$CVD2 <- Data$CVD1

#Come back to ask Camille about coding on this part (EW: is this solved?)
# EW: minor edit
Data$CVDMed <- Data$CD

Data$CVD1_med <- as.factor(ifelse(Data$CVD1 == 0 & Data$CVDMed == 0, 0,
                        ifelse(Data$CVD1 == 1 | Data$CVDMed == 1, 1, NA)))
#diabtes scores
#ever had diabetes
Data$DM <- as.factor(ifelse(Data$fa5230 == 1, 1,
                  ifelse(Data$fa5230 == 2, 0, NA)))

# EW: minor error
Data$DiabMed <- Data$DM

Data$DM_med <- as.factor(ifelse(Data$DM == 0 & Data$DiabMed == 0, 0,
                      ifelse(Data$DM == 1 | Data$DiabMed == 1, 1, NA)))

# 4.4. Code PCM multimorbidity variables (cf. Appendix D of Analysis Plan)
Data$CMD <- as.factor(ifelse(Data$CVD1 == 0 & Data$DM == 0,0,
                   ifelse(Data$CVD1 == 1 | Data$DM == 1,1,NA)))

Data$CardiacMD <- as.factor(ifelse(Data$CD == 0 & Data$DM == 0,0,
                         ifelse(Data$CD == 1 | Data$DM == 1,1,NA)))

Data$CVD2MD <- as.factor(ifelse(Data$CVD2 == 0 & Data$DM == 0,0,
                      ifelse(Data$CVD2 == 1 | Data$DM == 1,1,NA)))

Data$CMD_med <- as.factor(ifelse(Data$CVD1_med == 0 & Data$DM == 0,0,
                       ifelse(Data$CVD1_med == 1 | Data$DM == 1,1,NA)))

Data$PCM <- as.factor(ifelse(Data$MDD == 0 & Data$CMD == 0, 0,
                   ifelse(Data$MDD == 1 & Data$CMD == 0, 1,
                          ifelse(Data$MDD == 0 & Data$CMD == 1, 2,
                                 ifelse(Data$MDD == 1 & Data$CMD == 1, 3, NA)))))

Data$PCM_CD <- as.factor(ifelse(Data$MDD == 0 & Data$CardiacMD == 0, 0,
                      ifelse(Data$MDD == 1 & Data$CardiacMD == 0, 1,
                             ifelse(Data$MDD == 0 & Data$CardiacMD == 1, 2,
                                    ifelse(Data$MDD == 1 & Data$CardiacMD == 1, 3, NA)))))

Data$PCM_CVD2 <- as.factor(ifelse(Data$MDD == 0 & Data$CVD2MD == 0, 0,
                        ifelse(Data$MDD == 1 & Data$CVD2MD == 0, 1,
                               ifelse(Data$MDD == 0 & Data$CVD2MD == 1, 2,
                                      ifelse(Data$MDD == 1 & Data$CVD2MD == 1, 3, NA)))))

Data$PCM_med <- as.factor(ifelse(Data$MDD_med == 0 & Data$CMD_med == 0, 0,
                       ifelse(Data$MDD_med == 1 & Data$CMD_med == 0, 1,
                              ifelse(Data$MDD_med == 0 & Data$CMD_med == 1, 2,
                                     ifelse(Data$MDD_med == 1 & Data$CMD_med == 1, 3, NA)))))

Data$PCV <- as.factor(ifelse(Data$MDD == 0 & Data$CVD1 == 0, 0,
                   ifelse(Data$MDD == 1 & Data$CVD1 == 0, 1,
                          ifelse(Data$MDD == 0 & Data$CVD1 == 1, 2,
                                 ifelse(Data$MDD == 1 & Data$CVD1 == 1, 3, NA)))))

Data$PM <- as.factor(ifelse(Data$MDD == 0 & Data$DM == 0, 0,
                  ifelse(Data$MDD == 1 & Data$DM == 0, 1,
                         ifelse(Data$MDD == 0 & Data$DM == 1, 2,
                                ifelse(Data$MDD == 1 & Data$DM == 1, 3, NA)))))

# 4.5. Code covariate variables (cf. Appendix E of Analysis Plan)
Data$Age <- Data$fa9992 #add ALSPAC age variable (partner) #pb910 the earliest timepoint available for age

#Esther to add gender variable for partner
# EW: all males
#Data$Sex <- revalue(factor(Data$Sexe), c("1"="0", "2"="1")) #not sure about this, Q for Esther: sex available?

#TW below 3 = low education, 4 = Alevel, 5= Degree
# EW: reworked; please check
# Data$Educ <- as.factor(ifelse(Data$pb325a == 1 | Data$pb325a == 2 | Data$pb325a == 3 | Data$pb325a == 0, 0,
#                            ifelse(Data$pb325a == 4, 1, ifelse(Data$pb325a == 5, 2, NA))))

Data$Educ <- ifelse(Data$pb312 == 2, 0,
                              ifelse(Data$pb312 == 1, 1, NA))

Data$Educ[Data$pb321 == 1] = 2
Data$Educ=as.factor(Data$Educ)

# Hours per week spent doing vigorous physical activitiy
# EW: incomplete or wrong syntax; I assumed Mod and Wal is meant to be the average; please check
# Data$VigPhyAct <- ifelse(Data$fa3051 < 0, NA)          #aipaq1b = number of days per week where vigorous physical activity occurred
#  
# Data$ModPhyAct <- ((ifelse(Data$fa3022 < 0, NA))/2 + (ifelse(Data$fa3023 <0, NA))/2)                      
# 
# Data$WalPhyAct <- ((ifelse(Data$fa3020 < 0, NA))/2 + (ifelse(Data$fa3021 <0, NA))/2) 

Data$VigPhyAct <- Data$fa3051  
Data$ModPhyAct <- rowMeans(Data[,c("fa3022","fa3023")], na.rm = T)                      
Data$WalPhyAct <- rowMeans(Data[,c("fa3020","fa3021")], na.rm = T) 

# Hours per week spent doing any type of physical activity
Data$PhyAct <- ifelse(!(is.na(Data$VigPhyAct & Data$ModPhyAct & is.na(Data$WalPhyAct))), 
                      rowSums(cbind(Data$VigPhyAct,Data$ModPhyAct, Data$WalPhyAct), na.rm=T), NA)

# EW: this code is wrong and I can't decipher what this is meant to do; I recoded into smkoker/non-smoker; please check
#Data$Smoke <- (((ifelse(Data$pq1300 < 0, NA)*5)) + ((ifelse(Data$pq1301 < 0, NA)*2))/7)
Data$Smoke <- as.factor(ifelse(Data$pq1300 == 0 & Data$pq1301 == 0, 0, 
                               ifelse(Data$pq1300 > 0 | Data$pq1301 > 0, 1, NA)))
# EW: changed to numeric
Data$Alcohol <- as.numeric(ifelse(Data$ph6190 == 1, 0, 
                       ifelse(Data$ph6190 == 2, 0.5, 
                              ifelse(Data$ph6190 == 3, 3, 
                                     ifelse(Data$pph6190 == 4, 10.5, 
                                            ifelse(Data$ph6190 == 5, 42, 
                                                   ifelse(Data$ph6190 == 6, 70, NA)))))))

# EW: from here onwards, I did not comment on every single edit I made.
# General edits were:
# remove sex throughout
# simplified copy-pasting of ggplots
# ignored any errors re alc or physact

#################### 5. Create Sample A and Sample B #################### 

# 5.1. Sample A

SampleA <- Data[!is.na(Data$CM_PA) & !is.na(Data$CM_EA), ] 

SampleA <- SampleA[,c('MDD', 
                      'DM',
                     'CM', 
                     'Age', 
#                     'Sex', 
                     'Educ',
                     'CVD1',
                     'CMD',
                     'PCM',
                     'Smoke',
                     'PhyAct',
                     'Alcohol',
                     'PCV',
                     'PM',
                     'PCM_CD',
                     'PCM_CVD2',
                     'PCM_med',
                     'CM_PA',
                     'CM_EA',
                     'CM_SA',
                     'CM_sev')]

# 5.2. Save characteristics of Sample A
N_A <- nrow(SampleA)
#n_males_A <- nrow(SampleA[SampleA$Sex==0,])
n_males_A <- nrow(SampleA)
n_MDD_A <- nrow(SampleA[SampleA$MDD==1,])
n_CVD1_A <- nrow(SampleA[SampleA$CVD1==1,])
n_DM_A <- nrow(SampleA[SampleA$DM==1,])
n_CMD_A <- nrow(SampleA[SampleA$CMD==1,])
n_PCM0_A <- nrow(SampleA[SampleA$PCM==0,])
n_PCM1_A <- nrow(SampleA[SampleA$PCM==1,])
n_PCM2_A <- nrow(SampleA[SampleA$PCM==2,])
n_PCM3_A <- nrow(SampleA[SampleA$PCM==3,])
mean_age_A <- mean(SampleA$Age, na.rm=T)
median_age_A <- median(SampleA$Age, na.rm=T)
sd_age_A <- sd(SampleA$Age, na.rm=T)
n_Educ0_A <- nrow(SampleA[SampleA$Educ==0,])
n_Educ1_A <- nrow(SampleA[SampleA$Educ==1,])
n_Educ2_A <- nrow(SampleA[SampleA$Educ==2,])
n_Smoke0_A <- nrow(SampleA[SampleA$Smoke==0,])
n_Smoke1_A <- nrow(SampleA[SampleA$Smoke==1,])
mean_PhyAct_A <- mean(SampleA$PhyAct, na.rm=T)
median_PhyAct_A <- median(SampleA$PhyAct, na.rm=T)
sd_PhyAct_A <- sd(SampleA$PhyAct, na.rm=T)
mean_Alcohol_A <- mean(SampleA$Alcohol, na.rm=T)
median_Alcohol_A <- median(SampleA$Alcohol, na.rm=T)
sd_Alcohol_A <- sd(SampleA$Alcohol, na.rm=T)

SampleA_Characteristics <- data.frame(N_A,
                                     n_males_A,
                                     n_MDD_A,
                                     n_CVD1_A,
                                     n_DM_A,
                                     n_CMD_A,
                                     n_PCM0_A,
                                     n_PCM1_A,
                                     n_PCM2_A,
                                     n_PCM3_A,
                                     mean_age_A,
                                     median_age_A,
                                     sd_age_A,
                                     n_Educ0_A,
                                     n_Educ1_A,
                                     n_Educ2_A,
                                     n_Smoke0_A,
                                     n_Smoke1_A,
                                     mean_PhyAct_A,
                                     median_PhyAct_A,
                                     sd_PhyAct_A,
                                     mean_Alcohol_A,
                                     median_Alcohol_A,
                                     sd_Alcohol_A)

# 5.3. Export sample A characteristics (cohort's descriptive statistics) into a csv file 
write.csv(SampleA_Characteristics,"ALSPAC.partner_descriptives_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date

# 5.4. Sample B

# EW: confused about next two lines; check whether my edits do what you intended
# SampleB <- SampleA[SampleA$MDD!='NA' & SampleA$CMD!='NA',]
# 
# SampleB <- SampleA[!is.na(SampleA$MDD) & is.na(SampleA$CMD), ] 

SampleB <- SampleA[!(is.na(SampleA$MDD) & is.na(SampleA$CMD)), ] 

SampleB <- SampleB[,c('MDD', 
                        'CM', 
                        'Age', 
                        #'Sex', 
                        'Educ', 
                        'CMD',
                        'PCM',
                        'Smoke',
                        'PhyAct',
                        'Alcohol',
                        'PCV',
                        'PM',
                        'PCM_CD',
                        'PCM_CVD2',
                        'PCM_med',
                        'CM_PA',
                        'CM_EA',
                        'CM_SA',
                        'CM_sev')]

# Sample B - men only
SampleBmen <- SampleB

# Sample B - women only
#SampleBwomen <- SampleB[SampleB$Sex == 1,]



#################### 6. Statistical analysis Model #1 #################### 

SampleA$MDD <- relevel(SampleA$MDD, ref='0')
model1 <- glm(MDD ~ CM + Age  + Educ, family = binomial(link='logit'), data = SampleA)

# 6.1 Test assumptions for logistic regression (source: http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/)
# Linearity assumption
probabilities1 <- predict(model1, type = "response", na.rm=T)
mydata1 <- SampleA[,c('MDD','CM','Age','Educ')]
mydata1 <- mydata1[complete.cases(mydata1),]
mydata1_num <- mydata1 %>% dplyr::select_if(is.numeric)
predictors1 <- colnames(mydata1_num)
mydata1_num <- mydata1_num %>%
  mutate(logit = log(probabilities1/(1-probabilities1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

pdf("models.partners.pdf")

ggplot(mydata1_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, Model #1")                #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #1"

# 6.2. Model #1 characteristics
model1and2_info <- function(model){
  #Select only complete cases in the dataset
  mydata <- SampleA[,all.vars(formula(model))]
  mydata <- mydata[complete.cases(mydata),]
  # Create objects to store desired model characteristics
  model.data <- augment(model) %>% 
    dplyr::mutate(index = 1:n())
  inflobs <- model.data %>% filter(abs(.std.resid) > 3) 
  N_inflobs <- nrow(inflobs)
  vif <- car::vif(model)['CM','GVIF']
  N <- nrow(mydata)
  n_males <- nrow(mydata)
  mean_age <- mean(mydata$Age,na.rm=T)
  median_age <- median(mydata$Age,na.rm = T)
  sd_age <- sd(mydata$Age,na.rm = T)
  n_educ0 <- table(mydata$Educ)[1]
  n_educ1 <- table(mydata$Educ)[2]
  n_educ2 <- table(mydata$Educ)[3]
  source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
  crosstab <- crosstab(mydata, row.vars = "CM", col.vars = all.vars(formula(model))[1], type = "f")
  n_CM0_outcome0 <- crosstab$crosstab['0','0']
  n_CM1_outcome0 <- crosstab$crosstab['1','0']
  n_CMsum_outcome0 <- crosstab$crosstab['Sum','0']
  n_CM0_outcome1 <- crosstab$crosstab['0','1']
  n_CM1_outcome1 <- crosstab$crosstab['1','1']
  n_CMsum_outcome1 <- crosstab$crosstab['Sum','1']
  n_CM0_outcomesum <- crosstab$crosstab['0','Sum']
  n_CM1_outcomesum <- crosstab$crosstab['1','Sum']
  n_CMsum_outcomesum <- crosstab$crosstab['Sum','Sum']
  # Record coefficient estimates of log(OR), standard errors and 95% CI of log(OR) of predictors' effects on the dependent variable
  Estimate_CM1 <- summary(model)$coefficients['CM1','Estimate']
  Std.Error_CM1 <- summary(model)$coefficients['CM1','Std. Error']
  CI_LL_logOR_CM1 <- confint.default(model, level = 0.95)['CM1',1]
  CI_UL_logOR_CM1 <- confint.default(model, level = 0.95)['CM1',2]
  Estimate_Age <- summary(model)$coefficients['Age','Estimate']
  Std.Error_Age <- summary(model)$coefficients['Age','Std. Error']
  CI_LL_logOR_Age <- confint.default(model, level = 0.95)['Age',1]
  CI_UL_logOR_Age <- confint.default(model, level = 0.95)['Age',2]
  # Estimate_Sex1 <- summary(model)$coefficients['Sex1','Estimate']
  # Std.Error_Sex1 <- summary(model)$coefficients['Sex1','Std. Error']
  # CI_LL_logOR_Sex1 <- confint.default(model, level = 0.95)['Sex1',1]
  # CI_UL_logOR_Sex1 <- confint.default(model, level = 0.95)['Sex1',2]
  Estimate_Educ1 <- summary(model)$coefficients['Educ1','Estimate']
  Std.Error_Educ1 <- summary(model)$coefficients['Educ1','Std. Error']
  CI_LL_logOR_Educ1 <- confint.default(model, level = 0.95)['Educ1',1]
  CI_UL_logOR_Educ1 <- confint.default(model, level = 0.95)['Educ1',2]
  Estimate_Educ2 <- summary(model)$coefficients['Educ2','Estimate']
  Std.Error_Educ2 <- summary(model)$coefficients['Educ2','Std. Error']
  CI_LL_logOR_Educ2 <- confint.default(model, level = 0.95)['Educ2',1]
  CI_UL_logOR_Educ2 <- confint.default(model, level = 0.95)['Educ2',2]
  # Gather all model information in dataframe
  Stats_model <- data.frame(N_inflobs,
                             vif,
                             N,
                             n_males,
                             mean_age,
                             median_age,
                             sd_age,
                             n_educ0,
                             n_educ1,
                             n_educ2,
                             n_CM0_outcome0,
                             n_CM1_outcome0,
                             n_CMsum_outcome0,
                             n_CM0_outcome1,
                             n_CM1_outcome1,
                             n_CMsum_outcome1,
                             n_CM0_outcomesum,
                             n_CM1_outcomesum,
                             n_CMsum_outcomesum,
                             Estimate_CM1,
                             Std.Error_CM1,
                             CI_LL_logOR_CM1,
                             CI_UL_logOR_CM1,
                             Estimate_Age,
                             Std.Error_Age,
                             CI_LL_logOR_Age,
                             CI_UL_logOR_Age,
                             # Estimate_Sex1,
                             # Std.Error_Sex1,
                             # CI_LL_logOR_Sex1,
                             # CI_UL_logOR_Sex1,
                             Estimate_Educ1,
                             Std.Error_Educ1,
                             CI_LL_logOR_Educ1,
                             CI_UL_logOR_Educ1,
                             Estimate_Educ2,
                             Std.Error_Educ2,
                             CI_LL_logOR_Educ2,
                             CI_UL_logOR_Educ2)
  return(Stats_model)
}

Stats_model1 <- model1and2_info(model1)

# 6.3. Export dataframe of summary statistics of model #1 into a csv file 
write.csv(Stats_model1,"ALSPAC.partner_MODEL1_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 7. Statistical analysis Model #2 #################### 

SampleA$CMD <- relevel(SampleA$CMD, ref='0')
model2 <- glm(CMD ~ CM + Age  + Educ, family = binomial(link='logit'), data = SampleA)

# 7.1. Test assumptions for logistic regression
# Linearity assumption
probabilities2 <- predict(model2, type = "response", na.rm=T)
mydata2 <- SampleA[,c('CMD','CM','Age','Educ')]
mydata2 <- mydata2[complete.cases(mydata2),]
mydata2_num <- mydata2 %>% dplyr::select_if(is.numeric)
predictors2 <- colnames(mydata2_num)
mydata2_num <- mydata2_num %>%
  mutate(logit = log(probabilities2/(1-probabilities2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata2_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, Model #2")                #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #2"

# 7.2. Model #2 characteristics
Stats_model2 <- model1and2_info(model2)

# 7.3. Export dataframe of summary statistics of model #2 into a csv file 
write.csv(Stats_model2,"ALSPAC.partner_MODEL2_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 8. Statistical analysis Model #3 #################### 

SampleB$PCM <- relevel(SampleB$PCM, ref='0')
model3 <- multinom(PCM ~ CM + Age  + Educ + Smoke + PhyAct + Alcohol, data = SampleB)

# 8.1. Test assumptions for multinomial regression - Model #3
# Linearity assumption
probabilities3 <- predict(model3, type = "prob", na.rm=T)
probablities3_PCM1 <- probabilities3[,'1']
probablities3_PCM2 <- probabilities3[,'2']
probablities3_PCM3 <- probabilities3[,'3']
mydata3 <- SampleB[,c('PCM','CM','Age','Educ','Smoke','PhyAct','Alcohol')]
mydata3 <- mydata3[complete.cases(mydata3),]
mydata3_num <- mydata3 %>% dplyr::select_if(is.numeric)
predictors3 <- colnames(mydata3_num)

mydata3_PCM1Age_num <- data.frame(mydata3_num[,'Age']) %>%
  mutate(logit = log(probablities3_PCM1/(1-probablities3_PCM1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata3_PCM1PhyAct_num <- data.frame(mydata3_num[,'PhyAct']) %>%
  mutate(logit = log(probablities3_PCM1/(1-probablities3_PCM1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata3_PCM1Alcohol_num <- data.frame(mydata3_num[,'Alcohol']) %>%
  mutate(logit = log(probablities3_PCM1/(1-probablities3_PCM1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata3_PCM2Age_num <- data.frame(mydata3_num[,'Age']) %>%
  mutate(logit = log(probablities3_PCM2/(1-probablities3_PCM2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata3_PCM2PhyAct_num <- data.frame(mydata3_num[,'PhyAct']) %>%
  mutate(logit = log(probablities3_PCM2/(1-probablities3_PCM2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata3_PCM2Alcohol_num <- data.frame(mydata3_num[,'Alcohol']) %>%
  mutate(logit = log(probablities3_PCM2/(1-probablities3_PCM2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata3_PCM3Age_num <- data.frame(mydata3_num[,'Age']) %>%
  mutate(logit = log(probablities3_PCM3/(1-probablities3_PCM3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata3_PCM3PhyAct_num <- data.frame(mydata3_num[,'PhyAct']) %>%
  mutate(logit = log(probablities3_PCM3/(1-probablities3_PCM3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata3_PCM3Alcohol_num <- data.frame(mydata3_num[,'Alcohol']) %>%
  mutate(logit = log(probablities3_PCM3/(1-probablities3_PCM3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata3_PCM1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 1, Model #3")                        #Copy paste this graph in the Excel document General_Info_Models.xlsx", under tab "model #3"

ggplot(mydata3_PCM1PhyAct_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="PhyAct per logit, at PCM = 1, Model #3")                     #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #3"

ggplot(mydata3_PCM1Alcohol_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Alcohol per logit, at PCM = 1, Model #3")                    #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #3"

ggplot(mydata3_PCM2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 2, Model #3")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #3"

ggplot(mydata3_PCM2PhyAct_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="PhyAct per logit, at PCM = 2, Model #3")                     #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #3"

ggplot(mydata3_PCM2Alcohol_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Alcohol per logit, at PCM = 2, Model #3")                    #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #3"

ggplot(mydata3_PCM3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 3, Model #3")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #3"

ggplot(mydata3_PCM3PhyAct_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="PhyAct per logit, at PCM = 3, Model #3")                     #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #3"

ggplot(mydata3_PCM3Alcohol_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Alcohol per logit, at PCM = 3, Model #3")                    #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #3"

# 8.2. Model #3 characeristics
model3to9and13to15_info <- function(model){
  #Select only complete cases in the dataset
  mydata <- SampleB[,all.vars(formula(model))]
  mydata <- mydata[complete.cases(mydata),]
  # Create objects to store desired model characteristics
  vif <- as.data.frame(car::vif(model))[grep("^CM", rownames(as.data.frame(car::vif(model))),value=T),'GVIF']
  N <- nrow(mydata)
  n_males <- nrow(mydata)
  mean_age <- mean(mydata$Age,na.rm=T)
  median_age <- median(mydata$Age,na.rm = T)
  sd_age <- sd(mydata$Age,na.rm = T)
  n_educ0 <- table(mydata$Educ)[1]
  n_educ1 <- table(mydata$Educ)[2]
  n_educ2 <- table(mydata$Educ)[3]
  mean_phyact <- mean(mydata$PhyAct, na.rm =T)
  median_phyact <- ifelse(sum(all.vars(formula(model)) == "PhyAct") == 1, median(mydata$PhyAct, na.rm = T), NA)
  sd_phyact <- sd(mydata$PhyAct, na.rm = T)
  n_smoke <- nrow(mydata[mydata$Smoke == 1,])
  mean_alc <- mean(mydata$Alcohol, na.rm =T)
  median_alc <- ifelse(sum(all.vars(formula(model)) == "PhyAct") == 1, median(mydata$Alcohol, na.rm = T), NA)
  sd_alc <- sd(mydata$Alcohol, na.rm = T)
  source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
  crosstab <- crosstab(mydata, row.vars = all.vars(formula(model))[2], col.vars = all.vars(formula(model))[1], type = "f")
  n_CM0_outcome0 <- crosstab$crosstab['0','0']
  n_CM1_outcome0 <- crosstab$crosstab['1','0']
  n_CMsum_outcome0 <- crosstab$crosstab['Sum','0']
  n_CM0_outcome1 <- crosstab$crosstab['0','1']
  n_CM1_outcome1 <- crosstab$crosstab['1','1']
  n_CMsum_outcome1 <- crosstab$crosstab['Sum','1']
  n_CM0_outcome2 <- crosstab$crosstab['0','2']
  n_CM1_outcome2 <- crosstab$crosstab['1','2']
  n_CMsum_outcome2 <- crosstab$crosstab['Sum','2']
  n_CM0_outcome3 <- crosstab$crosstab['0','3']
  n_CM1_outcome3 <- crosstab$crosstab['1','3']
  n_CMsum_outcome3 <- crosstab$crosstab['Sum','3']
  n_CM0_outcomesum <- crosstab$crosstab['0','Sum']
  n_CM1_outcomesum <- crosstab$crosstab['1','Sum']
  n_CMsum_outcomesum <- crosstab$crosstab['Sum','Sum']
  # Record coefficient estimates log(OR), standard errors, and 95% CI of log(OR) of predictors' effects on the three levels of the outcome variable
  Estimate_CM1_outcome1 <- summary(model)$coefficients[1,grep("^CM",colnames(summary(model)$coefficients))]
  Std.Error_CM1_outcome1 <- summary(model)$standard.errors[1,grep("^CM",colnames(summary(model)$coefficients))]
  CI_LL_logOR_CM1_outcome1 <- confint(model)[grep("^CM",rownames(confint(model))),1,1]
  CI_UL_logOR_CM1_outcome1 <- confint(model)[grep("^CM",rownames(confint(model))),2,1]
  Estimate_CM1_outcome2 <- summary(model)$coefficients[2,grep("^CM",colnames(summary(model)$coefficients))]
  Std.Error_CM1_outcome2 <- summary(model)$standard.errors[2,grep("^CM",colnames(summary(model)$coefficients))]
  CI_LL_logOR_CM1_outcome2 <- confint(model)[grep("^CM",rownames(confint(model))),1,2]
  CI_UL_logOR_CM1_outcome2 <- confint(model)[grep("^CM",rownames(confint(model))),2,2]
  Estimate_CM1_outcome3 <- summary(model)$coefficients[3,grep("^CM",colnames(summary(model)$coefficients))]
  Std.Errors_CM1_outcome3 <- summary(model)$standard.errors[3,grep("^CM",colnames(summary(model)$coefficients))]
  CI_LL_logOR_CM1_outcome3 <- confint(model)[grep("^CM",rownames(confint(model))),1,3]
  CI_UL_logOR_CM1_outcome3 <- confint(model)[grep("^CM",rownames(confint(model))),2,3]
  Estimate_Age_outcome1 <- summary(model)$coefficients['1','Age']
  Std.Error_Age_outcome1 <- summary(model)$standard.errors[1,'Age']
  CI_LL_logOR_Age_outcome1 <- confint(model)['Age',1,1]
  CI_UL_logOR_Age_outcome1 <- confint(model)['Age',2,1]
  # Estimate_Sex1_outcome1 <- summary(model)$coefficients['1','Sex1']
  # Std.Error_Sex1_outcome1 <- summary(model)$standard.errors[1,'Sex1']
  # CI_LL_logOR_Sex1_outcome1 <- confint(model)['Sex1',1,1]
  # CI_UL_logOR_Sex1_outcome1 <- confint(model)['Sex1',2,1]
  Estimate_Educ1_outcome1 <- summary(model)$coefficients['1','Educ1']
  Std.Error_Educ1_outcome1 <- summary(model)$standard.errors[1,'Educ1']
  CI_LL_logOR_Educ1_outcome1 <- confint(model)['Educ1',1,1]
  CI_UL_logOR_Educ1_outcome1 <- confint(model)['Educ1',2,1]
  Estimate_Educ2_outcome1 <- summary(model)$coefficients['1','Educ2']
  Std.Error_Educ2_outcome1 <- summary(model)$standard.errors['1','Educ2']
  CI_LL_logOR_Educ2_outcome1 <- confint(model)['Educ2',1,1]
  CI_UL_logOR_Educ2_outcome1 <- confint(model)['Educ2',2,1]
  Estimate_Age_outcome2 <- summary(model)$coefficients['2','Age']
  Std.Error_Age_outcome2 <- summary(model)$standard.errors[2,'Age']
  CI_LL_logOR_Age_outcome2 <- confint(model)['Age',1,2]
  CI_UL_logOR_Age_outcome2 <- confint(model)['Age',2,2]
  # Estimate_Sex1_outcome2 <- summary(model)$coefficients['2','Sex1']
  # Std.Error_Sex1_outcome2 <- summary(model)$standard.errors[2,'Sex1']
  # CI_LL_logOR_Sex1_outcome2 <- confint(model)['Sex1',1,2]
  # CI_UL_logOR_Sex1_outcome2 <- confint(model)['Sex1',2,2]
  Estimate_Educ1_outcome2 <- summary(model)$coefficients['2','Educ1']
  Std.Error_Educ1_outcome2 <- summary(model)$standard.errors[2,'Educ1']
  CI_LL_logOR_Educ1_outcome2 <- confint(model)['Educ1',1,2]
  CI_UL_logOR_Educ1_outcome2 <- confint(model)['Educ1',2,2]
  Estimate_Educ2_outcome2 <- summary(model)$coefficients['2','Educ2']
  Std.Error_Educ2_outcome2 <- summary(model)$standard.errors['2','Educ2']
  CI_LL_logOR_Educ2_outcome2 <- confint(model)['Educ2',1,2]
  CI_UL_logOR_Educ2_outcome2 <- confint(model)['Educ2',2,2]
  Estimate_Age_outcome3 <- summary(model)$coefficients['3','Age']
  Std.Error_Age_outcome3 <- summary(model)$standard.errors[3,'Age']
  CI_LL_logOR_Age_outcome3 <- confint(model)['Age',1,3]
  CI_UL_logOR_Age_outcome3 <- confint(model)['Age',2,3]
  # Estimate_Sex1_outcome3 <- summary(model)$coefficients['3','Sex1']
  # Std.Error_Sex1_outcome3 <- summary(model)$standard.errors[3,'Sex1']
  # CI_LL_logOR_Sex1_outcome3 <- confint(model)['Sex1',1,3]
  # CI_UL_logOR_Sex1_outcome3 <- confint(model)['Sex1',2,3]
  Estimate_Educ1_outcome3 <- summary(model)$coefficients['3','Educ1']
  Std.Error_Educ1_outcome3 <- summary(model)$standard.errors[3,'Educ1']
  CI_LL_logOR_Educ1_outcome3 <- confint(model)['Educ1',1,3]
  CI_UL_logOR_Educ1_outcome3 <- confint(model)['Educ1',2,3]
  Estimate_Educ2_outcome3 <- summary(model)$coefficients['3','Educ2']
  Std.Error_Educ2_outcome3 <- summary(model)$standard.errors['3','Educ2']
  CI_LL_logOR_Educ2_outcome3 <- confint(model)['Educ2',1,3]
  CI_UL_logOR_Educ2_outcome3 <- confint(model)['Educ2',2,3]
  # Gather all model information in dataframe
  Stats_model <- data.frame(vif,
                            N,
                            n_males,
                            mean_age,
                            median_age,
                            sd_age,
                            n_educ0,
                            n_educ1,
                            n_educ2,
                            mean_phyact,
                            median_phyact,
                            sd_phyact,
                            n_smoke,
                            mean_alc,
                            median_alc,
                            sd_alc,
                            n_CM0_outcome0,
                            n_CM1_outcome0,
                            n_CMsum_outcome0,
                            n_CM0_outcome1,
                            n_CM1_outcome1,
                            n_CMsum_outcome1,
                            n_CM0_outcome2,
                            n_CM1_outcome2,
                            n_CMsum_outcome2,
                            n_CM0_outcome3,
                            n_CM1_outcome3,
                            n_CMsum_outcome3,
                            n_CM0_outcomesum,
                            n_CM1_outcomesum,
                            n_CMsum_outcomesum,
                            Estimate_CM1_outcome1,
                            Std.Error_CM1_outcome1,
                            CI_LL_logOR_CM1_outcome1,
                            CI_UL_logOR_CM1_outcome1,
                            Estimate_CM1_outcome2,
                            Std.Error_CM1_outcome2,
                            CI_LL_logOR_CM1_outcome2,
                            CI_UL_logOR_CM1_outcome2,
                            Estimate_CM1_outcome3,
                            Std.Errors_CM1_outcome3,
                            CI_LL_logOR_CM1_outcome3,
                            CI_UL_logOR_CM1_outcome3,
                            Estimate_Age_outcome1,
                            Std.Error_Age_outcome1,
                            CI_LL_logOR_Age_outcome1,
                            CI_UL_logOR_Age_outcome1,
                            # Estimate_Sex1_outcome1,
                            # Std.Error_Sex1_outcome1,
                            # CI_LL_logOR_Sex1_outcome1,
                            # CI_UL_logOR_Sex1_outcome1,
                            Estimate_Educ1_outcome1,
                            Std.Error_Educ1_outcome1,
                            CI_LL_logOR_Educ1_outcome1,
                            CI_UL_logOR_Educ1_outcome1,
                            Estimate_Educ2_outcome1,
                            Std.Error_Educ2_outcome1,
                            CI_LL_logOR_Educ2_outcome1,
                            CI_UL_logOR_Educ2_outcome1,
                            Estimate_Age_outcome2,
                            Std.Error_Age_outcome2,
                            CI_LL_logOR_Age_outcome2,
                            CI_UL_logOR_Age_outcome2,
                            # Estimate_Sex1_outcome2,
                            # Std.Error_Sex1_outcome2,
                            # CI_LL_logOR_Sex1_outcome2,
                            # CI_UL_logOR_Sex1_outcome2,
                            Estimate_Educ1_outcome2,
                            Std.Error_Educ1_outcome2,
                            CI_LL_logOR_Educ1_outcome2,
                            CI_UL_logOR_Educ1_outcome2,
                            Estimate_Educ2_outcome2,
                            Std.Error_Educ2_outcome2,
                            CI_LL_logOR_Educ2_outcome2,
                            CI_UL_logOR_Educ2_outcome2,
                            Estimate_Age_outcome3,
                            Std.Error_Age_outcome3,
                            CI_LL_logOR_Age_outcome3,
                            CI_UL_logOR_Age_outcome3,
                            # Estimate_Sex1_outcome3,
                            # Std.Error_Sex1_outcome3,
                            # CI_LL_logOR_Sex1_outcome3,
                            # CI_UL_logOR_Sex1_outcome3,
                            Estimate_Educ1_outcome3,
                            Std.Error_Educ1_outcome3,
                            CI_LL_logOR_Educ1_outcome3,
                            CI_UL_logOR_Educ1_outcome3,
                            Estimate_Educ2_outcome3,
                            Std.Error_Educ2_outcome3,
                            CI_LL_logOR_Educ2_outcome3,
                            CI_UL_logOR_Educ2_outcome3)
  return(Stats_model)
}

Stats_model3 <- model3to9and13to15_info(model3)

# 8.3. Export dataframe of summary statistics of model #3 into a csv file 
write.csv(Stats_model3,"ALSPAC.partner_MODEL3_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 9. Statistical analysis Model #4 #################### 

SampleB$PCM <- relevel(SampleB$PCM, ref='0')
model4 <- multinom(PCM ~ CM + Age  + Educ , data = SampleB)

# 9.1. Test assumptions for multinomial regression - Model #4
# Linearity assumption
probabilities4 <- predict(model4, type = "prob", na.rm=T)
probablities4_PCM1 <- probabilities4[,'1']
probablities4_PCM2 <- probabilities4[,'2']
probablities4_PCM3 <- probabilities4[,'3']
mydata4 <- SampleB[,c('PCM','CM','Age','Educ')]
mydata4 <- mydata4[complete.cases(mydata4),]
mydata4_num <- mydata4 %>% dplyr::select_if(is.numeric)
predictors4 <- colnames(mydata4_num)

mydata4_PCM1Age_num <- data.frame(mydata4_num[,'Age']) %>%
  mutate(logit = log(probablities4_PCM1/(1-probablities4_PCM1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata4_PCM2Age_num <- data.frame(mydata4_num[,'Age']) %>%
  mutate(logit = log(probablities4_PCM2/(1-probablities4_PCM2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata4_PCM3Age_num <- data.frame(mydata4_num[,'Age']) %>%
  mutate(logit = log(probablities4_PCM3/(1-probablities4_PCM3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata4_PCM1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 1, Model #4")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #4"

ggplot(mydata4_PCM2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 2, Model #4")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #4"

ggplot(mydata4_PCM3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 3, Model #4")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #4"

# 9.2. Model #4 characteristics
Stats_model4 <- model3to9and13to15_info(model4)

# 9.3. Export dataframe of summary statistics of model #4 into a csv file 
write.csv(Stats_model4,"ALSPAC.partner_MODEL4_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 10. Statistical analysis Model #5 #################### 

SampleB$PCV <- relevel(SampleB$PCV, ref='0')
model5 <- multinom(PCV ~ CM + Age  + Educ, data = SampleB)

# 10.1. Test assumptions for multinomial regression - Model #5
# Linearity assumption
probabilities5 <- predict(model5, type = "prob", na.rm=T)
probablities5_PCV1 <- probabilities5[,'1']
probablities5_PCV2 <- probabilities5[,'2']
probablities5_PCV3 <- probabilities5[,'3']
mydata5 <- SampleB[,c('PCV','CM','Age','Educ')]
mydata5 <- mydata5[complete.cases(mydata5),]
mydata5_num <- mydata5 %>% dplyr::select_if(is.numeric)
predictors5 <- colnames(mydata5_num)

mydata5_PCV1Age_num <- data.frame(mydata5_num[,'Age']) %>%
  mutate(logit = log(probablities5_PCV1/(1-probablities5_PCV1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata5_PCV2Age_num <- data.frame(mydata5_num[,'Age']) %>%
  mutate(logit = log(probablities5_PCV2/(1-probablities5_PCV2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata5_PCV3Age_num <- data.frame(mydata5_num[,'Age']) %>%
  mutate(logit = log(probablities5_PCV3/(1-probablities5_PCV3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata5_PCV1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 1, Model #5")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #5"

ggplot(mydata5_PCV2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 2, Model #5")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #5"

ggplot(mydata5_PCV3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 3, Model #5")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #5"

# 10.2. Model #5 characeristics
Stats_model5 <- model3to9and13to15_info(model5)

# 10.3. Export dataframe of summary statistics of model #5 into a csv file 
write.csv(Stats_model5,"ALSPAC.partner_MODEL5_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 11. Statistical analysis Model #6 #################### 

SampleB$PM <- relevel(SampleB$PM, ref='0')
model6 <- multinom(PM ~ CM + Age  + Educ, data = SampleB)

# 11.1. Test assumptions for multinomial regression - Model #6
# Linearity assumption
probabilities6 <- predict(model6, type = "prob", na.rm=T)
probablities6_PM1 <- probabilities6[,'1']
probablities6_PM2 <- probabilities6[,'2']
probablities6_PM3 <- probabilities6[,'3']
mydata6 <- SampleB[,c('PM','CM','Age','Educ')]
mydata6 <- mydata6[complete.cases(mydata6),]
mydata6_num <- mydata6 %>% dplyr::select_if(is.numeric)
predictors6 <- colnames(mydata6_num)

mydata6_PM1Age_num <- data.frame(mydata6_num[,'Age']) %>%
  mutate(logit = log(probablities6_PM1/(1-probablities6_PM1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata6_PM2Age_num <- data.frame(mydata6_num[,'Age']) %>%
  mutate(logit = log(probablities6_PM2/(1-probablities6_PM2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata6_PM3Age_num <- data.frame(mydata6_num[,'Age']) %>%
  mutate(logit = log(probablities6_PM3/(1-probablities6_PM3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata6_PM1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PM = 1, Model #6")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #6"

ggplot(mydata6_PM2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PM = 2, Model #6")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #6"

ggplot(mydata6_PM3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PM = 3, Model #6")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #6"

# 11.2. Model #6 characeristics
Stats_model6 <- model3to9and13to15_info(model6)

# 11.3. Export dataframe of summary statistics of model #6 into a csv file 
write.csv(Stats_model6,"ALSPAC.partner_MODEL6_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 12. Statistical analysis Model #7 #################### 

SampleB$PCM_CD <- relevel(SampleB$PCM_CD, ref='0')
model7 <- multinom(PCM_CD ~ CM + Age  + Educ, data = SampleB)

# 12.1. Test assumptions for multinomial regression - Model #7
# Linearity assumption
probabilities7 <- predict(model7, type = "prob", na.rm=T)
probablities7_PCM_CD1 <- probabilities7[,'1']
probablities7_PCM_CD2 <- probabilities7[,'2']
probablities7_PCM_CD3 <- probabilities7[,'3']
mydata7 <- SampleB[,c('PCM_CD','CM','Age','Educ')]
mydata7 <- mydata7[complete.cases(mydata7),]
mydata7_num <- mydata7 %>% dplyr::select_if(is.numeric)
predictors7 <- colnames(mydata7_num)

mydata7_PCM_CD1Age_num <- data.frame(mydata7_num[,'Age']) %>%
  mutate(logit = log(probablities7_PCM_CD1/(1-probablities7_PCM_CD1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata7_PCM_CD2Age_num <- data.frame(mydata7_num[,'Age']) %>%
  mutate(logit = log(probablities7_PCM_CD2/(1-probablities7_PCM_CD2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata7_PCM_CD3Age_num <- data.frame(mydata7_num[,'Age']) %>%
  mutate(logit = log(probablities7_PCM_CD3/(1-probablities7_PCM_CD3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata7_PCM_CD1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM_CD = 1, Model #7")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #7"

ggplot(mydata7_PCM_CD2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM_CD = 2, Model #7")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #7"

ggplot(mydata7_PCM_CD3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM_CD = 3, Model #7")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #7"

# 12.2. Model #7 characeristics
Stats_model7 <- model3to9and13to15_info(model7)

# 12.3. Export dataframe of summary statistics of model #7 into a csv file 
write.csv(Stats_model7,"ALSPAC.partner_MODEL7_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 13. Statistical analysis Model #8 #################### 

SampleB$PCM_CVD2 <- relevel(SampleB$PCM_CVD2, ref='0')
model8 <- multinom(PCM_CVD2 ~ CM + Age  + Educ, data = SampleB)

# 13.1. Test assumptions for multinomial regression - Model #8
# Linearity assumption
probabilities8 <- predict(model8, type = "prob", na.rm=T)
probablities8_PCM_CVD21 <- probabilities8[,'1']
probablities8_PCM_CVD22 <- probabilities8[,'2']
probablities8_PCM_CVD23 <- probabilities8[,'3']
mydata8 <- SampleB[,c('PCM_CVD2','CM','Age','Educ')]
mydata8 <- mydata8[complete.cases(mydata8),]
mydata8_num <- mydata8 %>% dplyr::select_if(is.numeric)
predictors8 <- colnames(mydata8_num)

mydata8_PCM_CVD21Age_num <- data.frame(mydata8_num[,'Age']) %>%
  mutate(logit = log(probablities8_PCM_CVD21/(1-probablities8_PCM_CVD21))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata8_PCM_CVD22Age_num <- data.frame(mydata8_num[,'Age']) %>%
  mutate(logit = log(probablities8_PCM_CVD22/(1-probablities8_PCM_CVD22))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata8_PCM_CVD23Age_num <- data.frame(mydata8_num[,'Age']) %>%
  mutate(logit = log(probablities8_PCM_CVD23/(1-probablities8_PCM_CVD23))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata8_PCM_CVD21Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM_CVD2 = 1, Model #8")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #8"

ggplot(mydata8_PCM_CVD22Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM_CVD2 = 2, Model #8")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #8"

ggplot(mydata8_PCM_CVD23Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM_CVD2 = 3, Model #8")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #8"

# 13.2. Model #8 characeristics
Stats_model8 <- model3to9and13to15_info(model8)

# 13.3. Export dataframe of summary statistics of model #8 into a csv file 
write.csv(Stats_model8,"ALSPAC.partner_MODEL8_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 14. Statistical analysis Model #9 #################### 

SampleB$PCM_med <- relevel(SampleB$PCM_med, ref='0')
model9 <- multinom(PCM_med ~ CM + Age  + Educ, data = SampleB)

# 14.1. Test assumptions for multinomial regression - Model #9
# Linearity assumption
probabilities9 <- predict(model9, type = "prob", na.rm=T)
probablities9_PCM_med1 <- probabilities9[,'1']
probablities9_PCM_med2 <- probabilities9[,'2']
probablities9_PCM_med3 <- probabilities9[,'3']
mydata9 <- SampleB[,c('PCM_med','CM','Age','Educ')]
mydata9 <- mydata9[complete.cases(mydata9),]
mydata9_num <- mydata9 %>% dplyr::select_if(is.numeric)
predictors9 <- colnames(mydata9_num)

mydata9_PCM_med1Age_num <- data.frame(mydata9_num[,'Age']) %>%
  mutate(logit = log(probablities9_PCM_med1/(1-probablities9_PCM_med1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata9_PCM_med2Age_num <- data.frame(mydata9_num[,'Age']) %>%
  mutate(logit = log(probablities9_PCM_med2/(1-probablities9_PCM_med2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata9_PCM_med3Age_num <- data.frame(mydata9_num[,'Age']) %>%
  mutate(logit = log(probablities9_PCM_med3/(1-probablities9_PCM_med3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata9_PCM_med1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM_med = 1, Model #9")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #9"

ggplot(mydata9_PCM_med2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM_med = 2, Model #9")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #9"

ggplot(mydata9_PCM_med3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM_med2 = 3, Model #9")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #9"

# 14.2. Model #9 characeristics
Stats_model9 <- model3to9and13to15_info(model9)

# 14.3. Export dataframe of summary statistics of model #9 into a csv file 
write.csv(Stats_model9,"ALSPAC.partner_MODEL9_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 15. Statistical analysis Model #10 #################### 

SampleBmen$PCM <- relevel(SampleBmen$PCM, ref='0')
model10 <- multinom(PCM ~ CM + Age  + Educ, data = SampleBmen)

# 15.1. Test assumptions for multinomial regression - Model #10 
# Linearity assumption
probabilities10 <- predict(model10, type = "prob", na.rm=T)
probablities10_PCM1 <- probabilities10[,'1']
probablities10_PCM2 <- probabilities10[,'2']
probablities10_PCM3 <- probabilities10[,'3']
mydata10 <- SampleBmen[,c('PCM','CM','Age','Educ')]
mydata10 <- mydata10[complete.cases(mydata10),]
mydata10_num <- mydata10 %>% dplyr::select_if(is.numeric)
predictors10 <- colnames(mydata10_num)

mydata10_PCM1Age_num <- data.frame(mydata10_num[,'Age']) %>%
  mutate(logit = log(probablities10_PCM1/(1-probablities10_PCM1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata10_PCM2Age_num <- data.frame(mydata10_num[,'Age']) %>%
  mutate(logit = log(probablities10_PCM2/(1-probablities10_PCM2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata10_PCM3Age_num <- data.frame(mydata10_num[,'Age']) %>%
  mutate(logit = log(probablities10_PCM3/(1-probablities10_PCM3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata10_PCM1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 1, Model #10")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #10"

ggplot(mydata10_PCM2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 2, Model #10")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #10"

ggplot(mydata10_PCM3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 3, Model #10")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #10"

# 15.2. Model #10 characteristics
model10_info <- function(model){
  #Select only complete cases in the dataset
  mydata <- SampleBmen[,all.vars(formula(model))]
  mydata <- mydata[complete.cases(mydata),]
  # Create objects to store desired model characteristics
  N <- nrow(mydata)
  n_males <- nrow(mydata)
  mean_age <- mean(mydata$Age,na.rm=T)
  median_age <- median(mydata$Age,na.rm = T)
  sd_age <- sd(mydata$Age,na.rm = T)
  n_educ0 <- table(mydata$Educ)[1]
  n_educ1 <- table(mydata$Educ)[2]
  n_educ2 <- table(mydata$Educ)[3]
  source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
  crosstab <- crosstab(mydata, row.vars = "CM", col.vars = all.vars(formula(model))[1], type = "f")
  n_CM0_outcome0 <- crosstab$crosstab['0','0']
  n_CM1_outcome0 <- crosstab$crosstab['1','0']
  n_CMsum_outcome0 <- crosstab$crosstab['Sum','0']
  n_CM0_outcome1 <- crosstab$crosstab['0','1']
  n_CM1_outcome1 <- crosstab$crosstab['1','1']
  n_CMsum_outcome1 <- crosstab$crosstab['Sum','1']
  n_CM0_outcome2 <- crosstab$crosstab['0','2']
  n_CM1_outcome2 <- crosstab$crosstab['1','2']
  n_CMsum_outcome2 <- crosstab$crosstab['Sum','2']
  n_CM0_outcome3 <- crosstab$crosstab['0','3']
  n_CM1_outcome3 <- crosstab$crosstab['1','3']
  n_CMsum_outcome3 <- crosstab$crosstab['Sum','3']
  n_CM0_outcomesum <- crosstab$crosstab['0','Sum']
  n_CM1_outcomesum <- crosstab$crosstab['1','Sum']
  n_CMsum_outcomesum <- crosstab$crosstab['Sum','Sum']
  # Record coefficient estimates log(OR), standard errors, and 95% CI of log(OR) of predictors' effects on the three levels of the outcome variable
  Estimate_CM1_outcome1 <- summary(model)$coefficients[1,'CM1']
  Std.Error_CM1_outcome1 <- summary(model)$standard.errors[1,'CM1']
  CI_LL_logOR_CM1_outcome1 <- confint(model)['CM1',1,1]
  CI_UL_logOR_CM1_outcome1 <- confint(model)['CM1',2,1]
  Estimate_CM1_outcome2 <- summary(model)$coefficients[2,'CM1']
  Std.Error_CM1_outcome2 <- summary(model)$standard.errors[2,'CM1']
  CI_LL_logOR_CM1_outcome2 <- confint(model)['CM1',1,2]
  CI_UL_logOR_CM1_outcome2 <- confint(model)['CM1',2,2]
  Estimate_CM1_outcome3 <- summary(model)$coefficients[3,'CM1']
  Std.Errors_CM1_outcome3 <- summary(model)$standard.errors[3,'CM1']
  CI_LL_logOR_CM1_outcome3 <- confint(model)['CM1',1,3]
  CI_UL_logOR_CM1_outcome3 <- confint(model)['CM1',2,3]
  Estimate_Age_outcome1 <- summary(model)$coefficients['1','Age']
  Std.Error_Age_outcome1 <- summary(model)$standard.errors[1,'Age']
  CI_LL_logOR_Age_outcome1 <- confint(model)['Age',1,1]
  CI_UL_logOR_Age_outcome1 <- confint(model)['Age',2,1]
  # Estimate_Sex1_outcome1 <- summary(model)$coefficients['1','Sex1']
  # Std.Error_Sex1_outcome1 <- summary(model)$standard.errors[1,'Sex1']
  # CI_LL_logOR_Sex1_outcome1 <- confint(model)['Sex1',1,1]
  # CI_UL_logOR_Sex1_outcome1 <- confint(model)['Sex1',2,1]
  Estimate_Educ1_outcome1 <- summary(model)$coefficients['1','Educ1']
  Std.Error_Educ1_outcome1 <- summary(model)$standard.errors[1,'Educ1']
  CI_LL_logOR_Educ1_outcome1 <- confint(model)['Educ1',1,1]
  CI_UL_logOR_Educ1_outcome1 <- confint(model)['Educ1',2,1]
  Estimate_Educ2_outcome1 <- summary(model)$coefficients['1','Educ2']
  Std.Error_Educ2_outcome1 <- summary(model)$standard.errors['1','Educ2']
  CI_LL_logOR_Educ2_outcome1 <- confint(model)['Educ2',1,1]
  CI_UL_logOR_Educ2_outcome1 <- confint(model)['Educ2',2,1]
  Estimate_Age_outcome2 <- summary(model)$coefficients['2','Age']
  Std.Error_Age_outcome2 <- summary(model)$standard.errors[2,'Age']
  CI_LL_logOR_Age_outcome2 <- confint(model)['Age',1,2]
  CI_UL_logOR_Age_outcome2 <- confint(model)['Age',2,2]
  # Estimate_Sex1_outcome2 <- summary(model)$coefficients['2','Sex1']
  # Std.Error_Sex1_outcome2 <- summary(model)$standard.errors[2,'Sex1']
  # CI_LL_logOR_Sex1_outcome2 <- confint(model)['Sex1',1,2]
  # CI_UL_logOR_Sex1_outcome2 <- confint(model)['Sex1',2,2]
  Estimate_Educ1_outcome2 <- summary(model)$coefficients['2','Educ1']
  Std.Error_Educ1_outcome2 <- summary(model)$standard.errors[2,'Educ1']
  CI_LL_logOR_Educ1_outcome2 <- confint(model)['Educ1',1,2]
  CI_UL_logOR_Educ1_outcome2 <- confint(model)['Educ1',2,2]
  Estimate_Educ2_outcome2 <- summary(model)$coefficients['2','Educ2']
  Std.Error_Educ2_outcome2 <- summary(model)$standard.errors['2','Educ2']
  CI_LL_logOR_Educ2_outcome2 <- confint(model)['Educ2',1,2]
  CI_UL_logOR_Educ2_outcome2 <- confint(model)['Educ2',2,2]
  Estimate_Age_outcome3 <- summary(model)$coefficients['3','Age']
  Std.Error_Age_outcome3 <- summary(model)$standard.errors[3,'Age']
  CI_LL_logOR_Age_outcome3 <- confint(model)['Age',1,3]
  CI_UL_logOR_Age_outcome3 <- confint(model)['Age',2,3]
  # Estimate_Sex1_outcome3 <- summary(model)$coefficients['3','Sex1']
  # Std.Error_Sex1_outcome3 <- summary(model)$standard.errors[3,'Sex1']
  # CI_LL_logOR_Sex1_outcome3 <- confint(model)['Sex1',1,3]
  # CI_UL_logOR_Sex1_outcome3 <- confint(model)['Sex1',2,3]
  Estimate_Educ1_outcome3 <- summary(model)$coefficients['3','Educ1']
  Std.Error_Educ1_outcome3 <- summary(model)$standard.errors[3,'Educ1']
  CI_LL_logOR_Educ1_outcome3 <- confint(model)['Educ1',1,3]
  CI_UL_logOR_Educ1_outcome3 <- confint(model)['Educ1',2,3]
  Estimate_Educ2_outcome3 <- summary(model)$coefficients['3','Educ2']
  Std.Error_Educ2_outcome3 <- summary(model)$standard.errors['3','Educ2']
  CI_LL_logOR_Educ2_outcome3 <- confint(model)['Educ2',1,3]
  CI_UL_logOR_Educ2_outcome3 <- confint(model)['Educ2',2,3]
  # Gather all model information in dataframe
  Stats_model <- data.frame(N,
                            n_males,
                            mean_age,
                            median_age,
                            sd_age,
                            n_educ0,
                            n_educ1,
                            n_educ2,
                            n_CM0_outcome0,
                            n_CM1_outcome0,
                            n_CMsum_outcome0,
                            n_CM0_outcome1,
                            n_CM1_outcome1,
                            n_CMsum_outcome1,
                            n_CM0_outcome2,
                            n_CM1_outcome2,
                            n_CMsum_outcome2,
                            n_CM0_outcome3,
                            n_CM1_outcome3,
                            n_CMsum_outcome3,
                            n_CM0_outcomesum,
                            n_CM1_outcomesum,
                            n_CMsum_outcomesum,
                            Estimate_CM1_outcome1,
                            Std.Error_CM1_outcome1,
                            CI_LL_logOR_CM1_outcome1,
                            CI_UL_logOR_CM1_outcome1,
                            Estimate_CM1_outcome2,
                            Std.Error_CM1_outcome2,
                            CI_LL_logOR_CM1_outcome2,
                            CI_UL_logOR_CM1_outcome2,
                            Estimate_CM1_outcome3,
                            Std.Errors_CM1_outcome3,
                            CI_LL_logOR_CM1_outcome3,
                            CI_UL_logOR_CM1_outcome3,
                            Estimate_Age_outcome1,
                            Std.Error_Age_outcome1,
                            CI_LL_logOR_Age_outcome1,
                            CI_UL_logOR_Age_outcome1,
                            # Estimate_Sex1_outcome1,
                            # Std.Error_Sex1_outcome1,
                            # CI_LL_logOR_Sex1_outcome1,
                            # CI_UL_logOR_Sex1_outcome1,
                            Estimate_Educ1_outcome1,
                            Std.Error_Educ1_outcome1,
                            CI_LL_logOR_Educ1_outcome1,
                            CI_UL_logOR_Educ1_outcome1,
                            Estimate_Educ2_outcome1,
                            Std.Error_Educ2_outcome1,
                            CI_LL_logOR_Educ2_outcome1,
                            CI_UL_logOR_Educ2_outcome1,
                            Estimate_Age_outcome2,
                            Std.Error_Age_outcome2,
                            CI_LL_logOR_Age_outcome2,
                            CI_UL_logOR_Age_outcome2,
                            # Estimate_Sex1_outcome2,
                            # Std.Error_Sex1_outcome2,
                            # CI_LL_logOR_Sex1_outcome2,
                            # CI_UL_logOR_Sex1_outcome2,
                            Estimate_Educ1_outcome2,
                            Std.Error_Educ1_outcome2,
                            CI_LL_logOR_Educ1_outcome2,
                            CI_UL_logOR_Educ1_outcome2,
                            Estimate_Educ2_outcome2,
                            Std.Error_Educ2_outcome2,
                            CI_LL_logOR_Educ2_outcome2,
                            CI_UL_logOR_Educ2_outcome2,
                            Estimate_Age_outcome3,
                            Std.Error_Age_outcome3,
                            CI_LL_logOR_Age_outcome3,
                            CI_UL_logOR_Age_outcome3,
                            # Estimate_Sex1_outcome3,
                            # Std.Error_Sex1_outcome3,
                            # CI_LL_logOR_Sex1_outcome3,
                            # CI_UL_logOR_Sex1_outcome3,
                            Estimate_Educ1_outcome3,
                            Std.Error_Educ1_outcome3,
                            CI_LL_logOR_Educ1_outcome3,
                            CI_UL_logOR_Educ1_outcome3,
                            Estimate_Educ2_outcome3,
                            Std.Error_Educ2_outcome3,
                            CI_LL_logOR_Educ2_outcome3,
                            CI_UL_logOR_Educ2_outcome3)
  return(Stats_model)
}

Stats_model10 <- model10_info(model10)

# 15.3 Export dataframe of statistics of model #10 into a csv file 
write.csv(Stats_model10,"ALSPAC.partner_MODEL10_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date


# EW: excluded next model (no women)
# #################### 16. Statistical analysis Model #11 #################### 
# SampleBwomen$PCM <- relevel(SampleBwomen$PCM, ref='0')
# model11 <- multinom(PCM ~ CM + Age  + Educ, data = SampleBwomen)
# 
# # 16.1. Test assumptions for multinomial regression - Model #11
# #Linearity
# probabilities11 <- predict(model11, type = "prob", na.rm=T)
# probablities11_PCM1 <- probabilities11[,'1']
# probablities11_PCM2 <- probabilities11[,'2']
# probablities11_PCM3 <- probabilities11[,'3']
# mydata11 <- SampleBwomen[,c('PCM','CM','Age','Educ')]
# mydata11 <- mydata11[complete.cases(mydata11),]
# mydata11_num <- mydata11 %>% dplyr::select_if(is.numeric)
# predictors11 <- colnames(mydata11_num)
# 
# mydata11_PCM1Age_num <- data.frame(mydata11_num[,'Age']) %>%
#   mutate(logit = log(probablities11_PCM1/(1-probablities11_PCM1))) %>%
#   gather(key = "predictors", value = "predictor.value", -logit)
# mydata11_PCM2Age_num <- data.frame(mydata11_num[,'Age']) %>%
#   mutate(logit = log(probablities11_PCM2/(1-probablities11_PCM2))) %>%
#   gather(key = "predictors", value = "predictor.value", -logit)
# mydata11_PCM3Age_num <- data.frame(mydata11_num[,'Age']) %>%
#   mutate(logit = log(probablities11_PCM3/(1-probablities11_PCM3))) %>%
#   gather(key = "predictors", value = "predictor.value", -logit)
# 
# ggplot(mydata11_PCM1Age_num, aes(logit, predictor.value))+
#   geom_point(size = 0.5, alpha = 0.5) +
#   geom_smooth(method = "loess") + 
#   theme_bw() + 
#   facet_wrap(~predictors, scales = "free_y") +
#   labs(title="Age per logit, at PCM = 1, Model #11")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #11"
# ggplot(mydata11_PCM2Age_num, aes(logit, predictor.value))+
#   geom_point(size = 0.5, alpha = 0.5) +
#   geom_smooth(method = "loess") + 
#   theme_bw() + 
#   facet_wrap(~predictors, scales = "free_y") +
#   labs(title="Age per logit, at PCM = 2, Model #11")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #11"
# ggplot(mydata11_PCM3Age_num, aes(logit, predictor.value))+
#   geom_point(size = 0.5, alpha = 0.5) +
#   geom_smooth(method = "loess") + 
#   theme_bw() + 
#   facet_wrap(~predictors, scales = "free_y") +
#   labs(title="Age per logit, at PCM = 3, Model #11")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #11"
# 
# # 16.2. Model #11 characteristics
# model11_info <- function(model){
#   #Select only complete cases in the dataset
#   mydata <- SampleBwomen[,all.vars(formula(model))]
#   mydata <- mydata[complete.cases(mydata),]
#   # Create objects to store desired model characteristics
#   N <- nrow(mydata)
#   n_males <- nrow(mydata)
#   mean_age <- mean(mydata$Age,na.rm=T)
#   median_age <- median(mydata$Age,na.rm = T)
#   sd_age <- sd(mydata$Age,na.rm = T)
#   n_educ0 <- table(mydata$Educ)[1]
#   n_educ1 <- table(mydata$Educ)[2]
#   n_educ2 <- table(mydata$Educ)[3]
#   source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
#   crosstab <- crosstab(mydata, row.vars = "CM", col.vars = all.vars(formula(model))[1], type = "f")
#   n_CM0_outcome0 <- crosstab$crosstab['0','0']
#   n_CM1_outcome0 <- crosstab$crosstab['1','0']
#   n_CMsum_outcome0 <- crosstab$crosstab['Sum','0']
#   n_CM0_outcome1 <- crosstab$crosstab['0','1']
#   n_CM1_outcome1 <- crosstab$crosstab['1','1']
#   n_CMsum_outcome1 <- crosstab$crosstab['Sum','1']
#   n_CM0_outcome2 <- crosstab$crosstab['0','2']
#   n_CM1_outcome2 <- crosstab$crosstab['1','2']
#   n_CMsum_outcome2 <- crosstab$crosstab['Sum','2']
#   n_CM0_outcome3 <- crosstab$crosstab['0','3']
#   n_CM1_outcome3 <- crosstab$crosstab['1','3']
#   n_CMsum_outcome3 <- crosstab$crosstab['Sum','3']
#   n_CM0_outcomesum <- crosstab$crosstab['0','Sum']
#   n_CM1_outcomesum <- crosstab$crosstab['1','Sum']
#   n_CMsum_outcomesum <- crosstab$crosstab['Sum','Sum']
#   # Record coefficient estimates log(OR), standard errors, and 95% CI of log(OR) of predictors' effects on the three levels of the outcome variable
#   Estimate_CM1_outcome1 <- summary(model)$coefficients[1,'CM1']
#   Std.Error_CM1_outcome1 <- summary(model)$standard.errors[1,'CM1']
#   CI_LL_logOR_CM1_outcome1 <- confint(model)['CM1',1,1]
#   CI_UL_logOR_CM1_outcome1 <- confint(model)['CM1',2,1]
#   Estimate_CM1_outcome2 <- summary(model)$coefficients[2,'CM1']
#   Std.Error_CM1_outcome2 <- summary(model)$standard.errors[2,'CM1']
#   CI_LL_logOR_CM1_outcome2 <- confint(model)['CM1',1,2]
#   CI_UL_logOR_CM1_outcome2 <- confint(model)['CM1',2,2]
#   Estimate_CM1_outcome3 <- summary(model)$coefficients[3,'CM1']
#   Std.Errors_CM1_outcome3 <- summary(model)$standard.errors[3,'CM1']
#   CI_LL_logOR_CM1_outcome3 <- confint(model)['CM1',1,3]
#   CI_UL_logOR_CM1_outcome3 <- confint(model)['CM1',2,3]
#   Estimate_Age_outcome1 <- summary(model)$coefficients['1','Age']
#   Std.Error_Age_outcome1 <- summary(model)$standard.errors[1,'Age']
#   CI_LL_logOR_Age_outcome1 <- confint(model)['Age',1,1]
#   CI_UL_logOR_Age_outcome1 <- confint(model)['Age',2,1]
#   # Estimate_Sex1_outcome1 <- summary(model)$coefficients['1','Sex1']
#   # Std.Error_Sex1_outcome1 <- summary(model)$standard.errors[1,'Sex1']
#   # CI_LL_logOR_Sex1_outcome1 <- confint(model)['Sex1',1,1]
#   # CI_UL_logOR_Sex1_outcome1 <- confint(model)['Sex1',2,1]
#   Estimate_Educ1_outcome1 <- summary(model)$coefficients['1','Educ1']
#   Std.Error_Educ1_outcome1 <- summary(model)$standard.errors[1,'Educ1']
#   CI_LL_logOR_Educ1_outcome1 <- confint(model)['Educ1',1,1]
#   CI_UL_logOR_Educ1_outcome1 <- confint(model)['Educ1',2,1]
#   Estimate_Educ2_outcome1 <- summary(model)$coefficients['1','Educ2']
#   Std.Error_Educ2_outcome1 <- summary(model)$standard.errors['1','Educ2']
#   CI_LL_logOR_Educ2_outcome1 <- confint(model)['Educ2',1,1]
#   CI_UL_logOR_Educ2_outcome1 <- confint(model)['Educ2',2,1]
#   Estimate_Age_outcome2 <- summary(model)$coefficients['2','Age']
#   Std.Error_Age_outcome2 <- summary(model)$standard.errors[2,'Age']
#   CI_LL_logOR_Age_outcome2 <- confint(model)['Age',1,2]
#   CI_UL_logOR_Age_outcome2 <- confint(model)['Age',2,2]
#   # Estimate_Sex1_outcome2 <- summary(model)$coefficients['2','Sex1']
#   # Std.Error_Sex1_outcome2 <- summary(model)$standard.errors[2,'Sex1']
#   # CI_LL_logOR_Sex1_outcome2 <- confint(model)['Sex1',1,2]
#   # CI_UL_logOR_Sex1_outcome2 <- confint(model)['Sex1',2,2]
#   Estimate_Educ1_outcome2 <- summary(model)$coefficients['2','Educ1']
#   Std.Error_Educ1_outcome2 <- summary(model)$standard.errors[2,'Educ1']
#   CI_LL_logOR_Educ1_outcome2 <- confint(model)['Educ1',1,2]
#   CI_UL_logOR_Educ1_outcome2 <- confint(model)['Educ1',2,2]
#   Estimate_Educ2_outcome2 <- summary(model)$coefficients['2','Educ2']
#   Std.Error_Educ2_outcome2 <- summary(model)$standard.errors['2','Educ2']
#   CI_LL_logOR_Educ2_outcome2 <- confint(model)['Educ2',1,2]
#   CI_UL_logOR_Educ2_outcome2 <- confint(model)['Educ2',2,2]
#   Estimate_Age_outcome3 <- summary(model)$coefficients['3','Age']
#   Std.Error_Age_outcome3 <- summary(model)$standard.errors[3,'Age']
#   CI_LL_logOR_Age_outcome3 <- confint(model)['Age',1,3]
#   CI_UL_logOR_Age_outcome3 <- confint(model)['Age',2,3]
#   # Estimate_Sex1_outcome3 <- summary(model)$coefficients['3','Sex1']
#   # Std.Error_Sex1_outcome3 <- summary(model)$standard.errors[3,'Sex1']
#   # CI_LL_logOR_Sex1_outcome3 <- confint(model)['Sex1',1,3]
#   # CI_UL_logOR_Sex1_outcome3 <- confint(model)['Sex1',2,3]
#   Estimate_Educ1_outcome3 <- summary(model)$coefficients['3','Educ1']
#   Std.Error_Educ1_outcome3 <- summary(model)$standard.errors[3,'Educ1']
#   CI_LL_logOR_Educ1_outcome3 <- confint(model)['Educ1',1,3]
#   CI_UL_logOR_Educ1_outcome3 <- confint(model)['Educ1',2,3]
#   Estimate_Educ2_outcome3 <- summary(model)$coefficients['3','Educ2']
#   Std.Error_Educ2_outcome3 <- summary(model)$standard.errors['3','Educ2']
#   CI_LL_logOR_Educ2_outcome3 <- confint(model)['Educ2',1,3]
#   CI_UL_logOR_Educ2_outcome3 <- confint(model)['Educ2',2,3]
#   # Gather all model information in dataframe
#   Stats_model <- data.frame(N,
#                             n_males,
#                             mean_age,
#                             median_age,
#                             sd_age,
#                             n_educ0,
#                             n_educ1,
#                             n_educ2,
#                             n_CM0_outcome0,
#                             n_CM1_outcome0,
#                             n_CMsum_outcome0,
#                             n_CM0_outcome1,
#                             n_CM1_outcome1,
#                             n_CMsum_outcome1,
#                             n_CM0_outcome2,
#                             n_CM1_outcome2,
#                             n_CMsum_outcome2,
#                             n_CM0_outcome3,
#                             n_CM1_outcome3,
#                             n_CMsum_outcome3,
#                             n_CM0_outcomesum,
#                             n_CM1_outcomesum,
#                             n_CMsum_outcomesum,
#                             Estimate_CM1_outcome1,
#                             Std.Error_CM1_outcome1,
#                             CI_LL_logOR_CM1_outcome1,
#                             CI_UL_logOR_CM1_outcome1,
#                             Estimate_CM1_outcome2,
#                             Std.Error_CM1_outcome2,
#                             CI_LL_logOR_CM1_outcome2,
#                             CI_UL_logOR_CM1_outcome2,
#                             Estimate_CM1_outcome3,
#                             Std.Errors_CM1_outcome3,
#                             CI_LL_logOR_CM1_outcome3,
#                             CI_UL_logOR_CM1_outcome3,
#                             Estimate_Age_outcome1,
#                             Std.Error_Age_outcome1,
#                             CI_LL_logOR_Age_outcome1,
#                             CI_UL_logOR_Age_outcome1,
#                             # Estimate_Sex1_outcome1,
#                             # Std.Error_Sex1_outcome1,
#                             # CI_LL_logOR_Sex1_outcome1,
#                             # CI_UL_logOR_Sex1_outcome1,
#                             Estimate_Educ1_outcome1,
#                             Std.Error_Educ1_outcome1,
#                             CI_LL_logOR_Educ1_outcome1,
#                             CI_UL_logOR_Educ1_outcome1,
#                             Estimate_Educ2_outcome1,
#                             Std.Error_Educ2_outcome1,
#                             CI_LL_logOR_Educ2_outcome1,
#                             CI_UL_logOR_Educ2_outcome1,
#                             Estimate_Age_outcome2,
#                             Std.Error_Age_outcome2,
#                             CI_LL_logOR_Age_outcome2,
#                             CI_UL_logOR_Age_outcome2,
#                             # Estimate_Sex1_outcome2,
#                             # Std.Error_Sex1_outcome2,
#                             # CI_LL_logOR_Sex1_outcome2,
#                             # CI_UL_logOR_Sex1_outcome2,
#                             Estimate_Educ1_outcome2,
#                             Std.Error_Educ1_outcome2,
#                             CI_LL_logOR_Educ1_outcome2,
#                             CI_UL_logOR_Educ1_outcome2,
#                             Estimate_Educ2_outcome2,
#                             Std.Error_Educ2_outcome2,
#                             CI_LL_logOR_Educ2_outcome2,
#                             CI_UL_logOR_Educ2_outcome2,
#                             Estimate_Age_outcome3,
#                             Std.Error_Age_outcome3,
#                             CI_LL_logOR_Age_outcome3,
#                             CI_UL_logOR_Age_outcome3,
#                             # Estimate_Sex1_outcome3,
#                             # Std.Error_Sex1_outcome3,
#                             # CI_LL_logOR_Sex1_outcome3,
#                             # CI_UL_logOR_Sex1_outcome3,
#                             Estimate_Educ1_outcome3,
#                             Std.Error_Educ1_outcome3,
#                             CI_LL_logOR_Educ1_outcome3,
#                             CI_UL_logOR_Educ1_outcome3,
#                             Estimate_Educ2_outcome3,
#                             Std.Error_Educ2_outcome3,
#                             CI_LL_logOR_Educ2_outcome3,
#                             CI_UL_logOR_Educ2_outcome3)
#   return(Stats_model)
# }
# 
# Stats_model11 <- model11_info(model11)
# 
# # 16.3 Export dataframe of statistics of model #11 into a csv file 
# write.csv(Stats_model11,"ALSPAC.partner_MODEL11_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 17. Statistical analysis Model #12 #################### 
SampleB$CM_PA <- relevel(SampleB$CM_PA, ref='0')
SampleB$CM_EA <- relevel(SampleB$CM_EA, ref='0')
SampleB$CM_SA <- relevel(SampleB$CM_SA, ref='0')
model12 <- multinom(PCM ~ CM_PA + CM_EA + CM_SA + Age  + Educ, data = SampleB)

# 17.1. Test assumptions for multinomial regression - Model #12
# Linearity assumtion
probabilities12 <- predict(model12, type = "prob", na.rm=T)
probablities12_PCM1 <- probabilities12[,'1']
probablities12_PCM2 <- probabilities12[,'2']
probablities12_PCM3 <- probabilities12[,'3']
mydata12 <- SampleB[,c('PCM','CM_PA','CM_EA', 'CM_SA','Age','Educ')]
mydata12 <- mydata12[complete.cases(mydata12),]
mydata12_num <- mydata12 %>% dplyr::select_if(is.numeric)
predictors12 <- colnames(mydata12_num)

mydata12_PCM1Age_num <- data.frame(mydata12_num[,'Age']) %>%
  mutate(logit = log(probablities12_PCM1/(1-probablities12_PCM1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata12_PCM2Age_num <- data.frame(mydata12_num[,'Age']) %>%
  mutate(logit = log(probablities12_PCM2/(1-probablities12_PCM2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata12_PCM3Age_num <- data.frame(mydata12_num[,'Age']) %>%
  mutate(logit = log(probablities12_PCM3/(1-probablities12_PCM3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata12_PCM1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 1, Model #12")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #12"

ggplot(mydata12_PCM2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 2, Model #12")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #12"

ggplot(mydata12_PCM3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 3, Model #12")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #12"

# 17.2. Model #12 charactieristics
model12_info <- function(model){
  #Select only complete cases in the dataset
  mydata <- SampleB[,all.vars(formula(model))]
  mydata <- mydata[complete.cases(mydata),]
  # Create objects to store desired model characteristics
  vif_CM_PA <- car::vif(model)['CM_PA','GVIF']
  vif_CM_EA <- car::vif(model)['CM_EA','GVIF']
  vif_CM_SA <- car::vif(model)['CM_SA','GVIF']
  N <- nrow(mydata)
  n_males <- nrow(mydata)
  mean_age <- mean(mydata$Age,na.rm=T)
  median_age <- median(mydata$Age,na.rm = T)
  sd_age <- sd(mydata$Age,na.rm = T)
  n_educ0 <- table(mydata$Educ)[1]
  n_educ1 <- table(mydata$Educ)[2]
  n_educ2 <- table(mydata$Educ)[3]
  source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
  crosstabCM_PA <- crosstab(mydata, row.vars = "CM_PA", col.vars = "PCM", type = "f")
  crosstabCM_EA <- crosstab(mydata, row.vars = "CM_EA", col.vars = "PCM", type = "f")
  crosstabCM_SA <- crosstab(mydata, row.vars = "CM_SA", col.vars = "PCM", type = "f")
  n_CM_PA0_outcome0 <- crosstabCM_PA$crosstab['0','0']
  n_CM_PA1_outcome0 <- crosstabCM_PA$crosstab['1','0']
  n_CM_PAsum_outcome0 <- crosstabCM_PA$crosstab['Sum','0']
  n_CM_PA0_outcome1 <- crosstabCM_PA$crosstab['0','1']
  n_CM_PA1_outcome1 <- crosstabCM_PA$crosstab['1','1']
  n_CM_PAsum_outcome1 <- crosstabCM_PA$crosstab['Sum','1']
  n_CM_PA0_outcome2 <- crosstabCM_PA$crosstab['0','2']
  n_CM_PA1_outcome2 <- crosstabCM_PA$crosstab['1','2']
  n_CM_PAsum_outcome2 <- crosstabCM_PA$crosstab['Sum','2']
  n_CM_PA0_outcome3 <- crosstabCM_PA$crosstab['0','3']
  n_CM_PA1_outcome3 <- crosstabCM_PA$crosstab['1','3']
  n_CM_PAsum_outcome3 <- crosstabCM_PA$crosstab['Sum','3']
  n_CM_PA0_outcomesum <- crosstabCM_PA$crosstab['0','Sum']
  n_CM_PA1_outcomesum <- crosstabCM_PA$crosstab['1','Sum']
  n_CM_PAsum_outcomesum <- crosstabCM_PA$crosstab['Sum','Sum']
  n_CM_EA0_outcome0 <- crosstabCM_EA$crosstab['0','0']
  n_CM_EA1_outcome0 <- crosstabCM_EA$crosstab['1','0']
  n_CM_EAsum_outcome0 <- crosstabCM_EA$crosstab['Sum','0']
  n_CM_EA0_outcome1 <- crosstabCM_EA$crosstab['0','1']
  n_CM_EA1_outcome1 <- crosstabCM_EA$crosstab['1','1']
  n_CM_EAsum_outcome1 <- crosstabCM_EA$crosstab['Sum','1']
  n_CM_EA0_outcome2 <- crosstabCM_EA$crosstab['0','2']
  n_CM_EA1_outcome2 <- crosstabCM_EA$crosstab['1','2']
  n_CM_EAsum_outcome2 <- crosstabCM_EA$crosstab['Sum','2']
  n_CM_EA0_outcome3 <- crosstabCM_EA$crosstab['0','3']
  n_CM_EA1_outcome3 <- crosstabCM_EA$crosstab['1','3']
  n_CM_EAsum_outcome3 <- crosstabCM_EA$crosstab['Sum','3']
  n_CM_EA0_outcomesum <- crosstabCM_EA$crosstab['0','Sum']
  n_CM_EA1_outcomesum <- crosstabCM_EA$crosstab['1','Sum']
  n_CM_EAsum_outcomesum <- crosstabCM_EA$crosstab['Sum','Sum']
  n_CM_SA0_outcome0 <- crosstabCM_SA$crosstab['0','0']
  n_CM_SA1_outcome0 <- crosstabCM_SA$crosstab['1','0']
  n_CM_SAsum_outcome0 <- crosstabCM_SA$crosstab['Sum','0']
  n_CM_SA0_outcome1 <- crosstabCM_SA$crosstab['0','1']
  n_CM_SA1_outcome1 <- crosstabCM_SA$crosstab['1','1']
  n_CM_SAsum_outcome1 <- crosstabCM_SA$crosstab['Sum','1']
  n_CM_SA0_outcome2 <- crosstabCM_SA$crosstab['0','2']
  n_CM_SA1_outcome2 <- crosstabCM_SA$crosstab['1','2']
  n_CM_SAsum_outcome2 <- crosstabCM_SA$crosstab['Sum','2']
  n_CM_SA0_outcome3 <- crosstabCM_SA$crosstab['0','3']
  n_CM_SA1_outcome3 <- crosstabCM_SA$crosstab['1','3']
  n_CM_SAsum_outcome3 <- crosstabCM_SA$crosstab['Sum','3']
  n_CM_SA0_outcomesum <- crosstabCM_SA$crosstab['0','Sum']
  n_CM_SA1_outcomesum <- crosstabCM_SA$crosstab['1','Sum']
  n_CM_SAsum_outcomesum <- crosstabCM_SA$crosstab['Sum','Sum']
  # Record coefficient estimates log(OR), standard errors, and 95% CI of log(OR) of predictors' effects on the three levels of the outcome variable
  Estimate_CM_PA_outcome1 <- summary(model)$coefficients[1,'CM_PA1']
  Std.Error_CM_PA_outcome1 <- summary(model)$standard.errors[1,'CM_PA1']
  CI_LL_logOR_CM_PA_outcome1 <- confint(model)['CM_PA1',1,1]
  CI_UL_logOR_CM_PA_outcome1 <- confint(model)['CM_PA1',2,1]
  Estimate_CM_PA_outcome2 <- summary(model)$coefficients[2,'CM_PA1']
  Std.Error_CM_PA_outcome2 <- summary(model)$standard.errors[2,'CM_PA1']
  CI_LL_logOR_CM_PA_outcome2 <- confint(model)['CM_PA1',1,2]
  CI_UL_logOR_CM_PA_outcome2 <- confint(model)['CM_PA1',2,2]
  Estimate_CM_PA_outcome3 <- summary(model)$coefficients[3,'CM_PA1']
  Std.Errors_CM_PA_outcome3 <- summary(model)$standard.errors[3,'CM_PA1']
  CI_LL_logOR_CM_PA_outcome3 <- confint(model)['CM_PA1',1,3]
  CI_UL_logOR_CM_PA_outcome3 <- confint(model)['CM_PA1',2,3]
  Estimate_CM_EA_outcome1 <- summary(model)$coefficients[1,'CM_EA1']
  Std.Error_CM_EA_outcome1 <- summary(model)$standard.errors[1,'CM_EA1']
  CI_LL_logOR_CM_EA_outcome1 <- confint(model)['CM_EA1',1,1]
  CI_UL_logOR_CM_EA_outcome1 <- confint(model)['CM_EA1',2,1]
  Estimate_CM_EA_outcome2 <- summary(model)$coefficients[2,'CM_EA1']
  Std.Error_CM_EA_outcome2 <- summary(model)$standard.errors[2,'CM_EA1']
  CI_LL_logOR_CM_EA_outcome2 <- confint(model)['CM_EA1',1,2]
  CI_UL_logOR_CM_EA_outcome2 <- confint(model)['CM_EA1',2,2]
  Estimate_CM_EA_outcome3 <- summary(model)$coefficients[3,'CM_EA1']
  Std.Errors_CM_EA_outcome3 <- summary(model)$standard.errors[3,'CM_EA1']
  CI_LL_logOR_CM_EA_outcome3 <- confint(model)['CM_EA1',1,3]
  CI_UL_logOR_CM_EA_outcome3 <- confint(model)['CM_EA1',2,3]
  Estimate_CM_SA_outcome1 <- summary(model)$coefficients[1,'CM_SA1']
  Std.Error_CM_SA_outcome1 <- summary(model)$standard.errors[1,'CM_SA1']
  CI_LL_logOR_CM_SA_outcome1 <- confint(model)['CM_SA1',1,1]
  CI_UL_logOR_CM_SA_outcome1 <- confint(model)['CM_SA1',2,1]
  Estimate_CM_SA_outcome2 <- summary(model)$coefficients[2,'CM_SA1']
  Std.Error_CM_SA_outcome2 <- summary(model)$standard.errors[2,'CM_SA1']
  CI_LL_logOR_CM_SA_outcome2 <- confint(model)['CM_SA1',1,2]
  CI_UL_logOR_CM_SA_outcome2 <- confint(model)['CM_SA1',2,2]
  Estimate_CM_SA_outcome3 <- summary(model)$coefficients[3,'CM_SA1']
  Std.Errors_CM_SA_outcome3 <- summary(model)$standard.errors[3,'CM_SA1']
  CI_LL_logOR_CM_SA_outcome3 <- confint(model)['CM_SA1',1,3]
  CI_UL_logOR_CM_SA_outcome3 <- confint(model)['CM_SA1',2,3]
  Estimate_Age_outcome1 <- summary(model)$coefficients['1','Age']
  Std.Error_Age_outcome1 <- summary(model)$standard.errors[1,'Age']
  CI_LL_logOR_Age_outcome1 <- confint(model)['Age',1,1]
  CI_UL_logOR_Age_outcome1 <- confint(model)['Age',2,1]
  # Estimate_Sex1_outcome1 <- summary(model)$coefficients['1','Sex1']
  # Std.Error_Sex1_outcome1 <- summary(model)$standard.errors[1,'Sex1']
  # CI_LL_logOR_Sex1_outcome1 <- confint(model)['Sex1',1,1]
  # CI_UL_logOR_Sex1_outcome1 <- confint(model)['Sex1',2,1]
  Estimate_Educ1_outcome1 <- summary(model)$coefficients['1','Educ1']
  Std.Error_Educ1_outcome1 <- summary(model)$standard.errors[1,'Educ1']
  CI_LL_logOR_Educ1_outcome1 <- confint(model)['Educ1',1,1]
  CI_UL_logOR_Educ1_outcome1 <- confint(model)['Educ1',2,1]
  Estimate_Educ2_outcome1 <- summary(model)$coefficients['1','Educ2']
  Std.Error_Educ2_outcome1 <- summary(model)$standard.errors['1','Educ2']
  CI_LL_logOR_Educ2_outcome1 <- confint(model)['Educ2',1,1]
  CI_UL_logOR_Educ2_outcome1 <- confint(model)['Educ2',2,1]
  Estimate_Age_outcome2 <- summary(model)$coefficients['2','Age']
  Std.Error_Age_outcome2 <- summary(model)$standard.errors[2,'Age']
  CI_LL_logOR_Age_outcome2 <- confint(model)['Age',1,2]
  CI_UL_logOR_Age_outcome2 <- confint(model)['Age',2,2]
  # Estimate_Sex1_outcome2 <- summary(model)$coefficients['2','Sex1']
  # Std.Error_Sex1_outcome2 <- summary(model)$standard.errors[2,'Sex1']
  # CI_LL_logOR_Sex1_outcome2 <- confint(model)['Sex1',1,2]
  # CI_UL_logOR_Sex1_outcome2 <- confint(model)['Sex1',2,2]
  Estimate_Educ1_outcome2 <- summary(model)$coefficients['2','Educ1']
  Std.Error_Educ1_outcome2 <- summary(model)$standard.errors[2,'Educ1']
  CI_LL_logOR_Educ1_outcome2 <- confint(model)['Educ1',1,2]
  CI_UL_logOR_Educ1_outcome2 <- confint(model)['Educ1',2,2]
  Estimate_Educ2_outcome2 <- summary(model)$coefficients['2','Educ2']
  Std.Error_Educ2_outcome2 <- summary(model)$standard.errors['2','Educ2']
  CI_LL_logOR_Educ2_outcome2 <- confint(model)['Educ2',1,2]
  CI_UL_logOR_Educ2_outcome2 <- confint(model)['Educ2',2,2]
  Estimate_Age_outcome3 <- summary(model)$coefficients['3','Age']
  Std.Error_Age_outcome3 <- summary(model)$standard.errors[3,'Age']
  CI_LL_logOR_Age_outcome3 <- confint(model)['Age',1,3]
  CI_UL_logOR_Age_outcome3 <- confint(model)['Age',2,3]
  # Estimate_Sex1_outcome3 <- summary(model)$coefficients['3','Sex1']
  # Std.Error_Sex1_outcome3 <- summary(model)$standard.errors[3,'Sex1']
  # CI_LL_logOR_Sex1_outcome3 <- confint(model)['Sex1',1,3]
  # CI_UL_logOR_Sex1_outcome3 <- confint(model)['Sex1',2,3]
  Estimate_Educ1_outcome3 <- summary(model)$coefficients['3','Educ1']
  Std.Error_Educ1_outcome3 <- summary(model)$standard.errors[3,'Educ1']
  CI_LL_logOR_Educ1_outcome3 <- confint(model)['Educ1',1,3]
  CI_UL_logOR_Educ1_outcome3 <- confint(model)['Educ1',2,3]
  Estimate_Educ2_outcome3 <- summary(model)$coefficients['3','Educ2']
  Std.Error_Educ2_outcome3 <- summary(model)$standard.errors['3','Educ2']
  CI_LL_logOR_Educ2_outcome3 <- confint(model)['Educ2',1,3]
  CI_UL_logOR_Educ2_outcome3 <- confint(model)['Educ2',2,3]
  # Gather all model information in dataframe
  Stats_model <- data.frame(vif_CM_PA,
                            vif_CM_EA,
                            vif_CM_SA,
                            N,
                            n_males,
                            mean_age,
                            median_age,
                            sd_age,
                            n_educ0,
                            n_educ1,
                            n_educ2,
                            n_CM_PA0_outcome0,
                            n_CM_PA1_outcome0,
                            n_CM_PAsum_outcome0,
                            n_CM_PA0_outcome1,
                            n_CM_PA1_outcome1,
                            n_CM_PAsum_outcome1,
                            n_CM_PA0_outcome2,
                            n_CM_PA1_outcome2,
                            n_CM_PAsum_outcome2,
                            n_CM_PA0_outcome3,
                            n_CM_PA1_outcome3,
                            n_CM_PAsum_outcome3,
                            n_CM_PA0_outcomesum,
                            n_CM_PA1_outcomesum,
                            n_CM_PAsum_outcomesum,
                            n_CM_EA0_outcome0,
                            n_CM_EA1_outcome0,
                            n_CM_EAsum_outcome0,
                            n_CM_EA0_outcome1,
                            n_CM_EA1_outcome1,
                            n_CM_EAsum_outcome1,
                            n_CM_EA0_outcome2,
                            n_CM_EA1_outcome2,
                            n_CM_EAsum_outcome2,
                            n_CM_EA0_outcome3,
                            n_CM_EA1_outcome3,
                            n_CM_EAsum_outcome3,
                            n_CM_EA0_outcomesum,
                            n_CM_EA1_outcomesum,
                            n_CM_EAsum_outcomesum,
                            n_CM_SA0_outcome0,
                            n_CM_SA1_outcome0,
                            n_CM_SAsum_outcome0,
                            n_CM_SA0_outcome1,
                            n_CM_SA1_outcome1,
                            n_CM_SAsum_outcome1,
                            n_CM_SA0_outcome2,
                            n_CM_SA1_outcome2,
                            n_CM_SAsum_outcome2,
                            n_CM_SA0_outcome3,
                            n_CM_SA1_outcome3,
                            n_CM_SAsum_outcome3,
                            n_CM_SA0_outcomesum,
                            n_CM_SA1_outcomesum,
                            n_CM_SAsum_outcomesum,
                            Estimate_CM_PA_outcome1,
                            Std.Error_CM_PA_outcome1,
                            CI_LL_logOR_CM_PA_outcome1,
                            CI_UL_logOR_CM_PA_outcome1,
                            Estimate_CM_PA_outcome2,
                            Std.Error_CM_PA_outcome2,
                            CI_LL_logOR_CM_PA_outcome2,
                            CI_UL_logOR_CM_PA_outcome2,
                            Estimate_CM_PA_outcome3,
                            Std.Errors_CM_PA_outcome3,
                            CI_LL_logOR_CM_PA_outcome3,
                            CI_UL_logOR_CM_PA_outcome3,
                            Estimate_CM_EA_outcome1,
                            Std.Error_CM_EA_outcome1,
                            CI_LL_logOR_CM_EA_outcome1,
                            CI_UL_logOR_CM_EA_outcome1,
                            Estimate_CM_EA_outcome2,
                            Std.Error_CM_EA_outcome2,
                            CI_LL_logOR_CM_EA_outcome2,
                            CI_UL_logOR_CM_EA_outcome2,
                            Estimate_CM_EA_outcome3,
                            Std.Errors_CM_EA_outcome3,
                            CI_LL_logOR_CM_EA_outcome3,
                            CI_UL_logOR_CM_EA_outcome3,
                            Estimate_CM_SA_outcome1,
                            Std.Error_CM_SA_outcome1,
                            CI_LL_logOR_CM_SA_outcome1,
                            CI_UL_logOR_CM_SA_outcome1,
                            Estimate_CM_SA_outcome2,
                            Std.Error_CM_SA_outcome2,
                            CI_LL_logOR_CM_SA_outcome2,
                            CI_UL_logOR_CM_SA_outcome2,
                            Estimate_CM_SA_outcome3,
                            Std.Errors_CM_SA_outcome3,
                            CI_LL_logOR_CM_SA_outcome3,
                            CI_UL_logOR_CM_SA_outcome3,
                            Estimate_Age_outcome1,
                            Std.Error_Age_outcome1,
                            CI_LL_logOR_Age_outcome1,
                            CI_UL_logOR_Age_outcome1,
                            # Estimate_Sex1_outcome1,
                            # Std.Error_Sex1_outcome1,
                            # CI_LL_logOR_Sex1_outcome1,
                            # CI_UL_logOR_Sex1_outcome1,
                            Estimate_Educ1_outcome1,
                            Std.Error_Educ1_outcome1,
                            CI_LL_logOR_Educ1_outcome1,
                            CI_UL_logOR_Educ1_outcome1,
                            Estimate_Educ2_outcome1,
                            Std.Error_Educ2_outcome1,
                            CI_LL_logOR_Educ2_outcome1,
                            CI_UL_logOR_Educ2_outcome1,
                            Estimate_Age_outcome2,
                            Std.Error_Age_outcome2,
                            CI_LL_logOR_Age_outcome2,
                            CI_UL_logOR_Age_outcome2,
                            # Estimate_Sex1_outcome2,
                            # Std.Error_Sex1_outcome2,
                            # CI_LL_logOR_Sex1_outcome2,
                            # CI_UL_logOR_Sex1_outcome2,
                            Estimate_Educ1_outcome2,
                            Std.Error_Educ1_outcome2,
                            CI_LL_logOR_Educ1_outcome2,
                            CI_UL_logOR_Educ1_outcome2,
                            Estimate_Educ2_outcome2,
                            Std.Error_Educ2_outcome2,
                            CI_LL_logOR_Educ2_outcome2,
                            CI_UL_logOR_Educ2_outcome2,
                            Estimate_Age_outcome3,
                            Std.Error_Age_outcome3,
                            CI_LL_logOR_Age_outcome3,
                            CI_UL_logOR_Age_outcome3,
                            # Estimate_Sex1_outcome3,
                            # Std.Error_Sex1_outcome3,
                            # CI_LL_logOR_Sex1_outcome3,
                            # CI_UL_logOR_Sex1_outcome3,
                            Estimate_Educ1_outcome3,
                            Std.Error_Educ1_outcome3,
                            CI_LL_logOR_Educ1_outcome3,
                            CI_UL_logOR_Educ1_outcome3,
                            Estimate_Educ2_outcome3,
                            Std.Error_Educ2_outcome3,
                            CI_LL_logOR_Educ2_outcome3,
                            CI_UL_logOR_Educ2_outcome3)
  return(Stats_model)
}

Stats_model12 <- model12_info(model12)

# 17.3 Export dataframe of statistics of model #12 into a csv file
write.csv(Stats_model12,"ALSPAC.partner_MODEL12_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 18. Statistical analysis Model #13 #################### 
SampleB$CM_PA <- relevel(SampleB$CM_PA, ref='0')
model13 <- multinom(PCM ~ CM_PA + Age  + Educ, data = SampleB)

# 18.1. Test assumptions for multinomial regression - Model #13
# Linearity assumption
probabilities13 <- predict(model13, type = "prob", na.rm=T)
probablities13_PCM1 <- probabilities13[,'1']
probablities13_PCM2 <- probabilities13[,'2']
probablities13_PCM3 <- probabilities13[,'3']
mydata13 <- SampleB[,c('PCM','CM_PA','Age','Educ')]
mydata13 <- mydata13[complete.cases(mydata13),]
mydata13_num <- mydata13 %>% dplyr::select_if(is.numeric)
predictors13 <- colnames(mydata13_num)

mydata13_PCM1Age_num <- data.frame(mydata13_num[,'Age']) %>%
  mutate(logit = log(probablities13_PCM1/(1-probablities13_PCM1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata13_PCM2Age_num <- data.frame(mydata13_num[,'Age']) %>%
  mutate(logit = log(probablities13_PCM2/(1-probablities13_PCM2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata13_PCM3Age_num <- data.frame(mydata13_num[,'Age']) %>%
  mutate(logit = log(probablities13_PCM3/(1-probablities13_PCM3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata13_PCM1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 1, Model #13")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #13"

ggplot(mydata13_PCM2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 2, Model #13")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #13"

ggplot(mydata13_PCM3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 3, Model #13")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #13"

# 18.2. Model #13 charactieristics
Stats_model13 <- model3to9and13to15_info(model13)

# 18.3. Export dataframe of summary statistics of model #13 into a csv file 
write.csv(Stats_model13,"ALSPAC.partner_MODEL13_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 19. Statistical analysis Model #14 #################### 
SampleB$CM_EA <- relevel(SampleB$CM_EA, ref='0')
model14 <- multinom(PCM ~ CM_EA + Age  + Educ, data = SampleB)

# 19.1. Test assumptions for multinomial regression - Model #14
# Linearity assumption
probabilities14 <- predict(model14, type = "prob", na.rm=T)
probablities14_PCM1 <- probabilities14[,'1']
probablities14_PCM2 <- probabilities14[,'2']
probablities14_PCM3 <- probabilities14[,'3']
mydata14 <- SampleB[,c('PCM','CM_EA','Age','Educ')]
mydata14 <- mydata14[complete.cases(mydata14),]
mydata14_num <- mydata14 %>% dplyr::select_if(is.numeric)
predictors14 <- colnames(mydata14_num)

mydata14_PCM1Age_num <- data.frame(mydata14_num[,'Age']) %>%
  mutate(logit = log(probablities14_PCM1/(1-probablities14_PCM1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata14_PCM2Age_num <- data.frame(mydata14_num[,'Age']) %>%
  mutate(logit = log(probablities14_PCM2/(1-probablities14_PCM2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata14_PCM3Age_num <- data.frame(mydata14_num[,'Age']) %>%
  mutate(logit = log(probablities14_PCM3/(1-probablities14_PCM3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata14_PCM1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 1, Model #14")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #14"

ggplot(mydata14_PCM2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 2, Model #14")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #14"

ggplot(mydata14_PCM3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 3, Model #14")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #14"

# 19.2. Model #14 charactieristics
Stats_model14 <- model3to9and13to15_info(model14)

# 19.3. Export dataframe of summary statistics of model #14 into a csv file 
write.csv(Stats_model14,"ALSPAC.partner_MODEL14_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 20. Statistical analysis Model #15 #################### 
SampleB$CM_SA <- relevel(SampleB$CM_SA, ref='0')
model15 <- multinom(PCM ~ CM_SA + Age  + Educ, data = SampleB)

# 20.1. Test assumptions for multinomial regression - Model #14
# Linearity assumption
probabilities15 <- predict(model15, type = "prob", na.rm=T)
probablities15_PCM1 <- probabilities15[,'1']
probablities15_PCM2 <- probabilities15[,'2']
probablities15_PCM3 <- probabilities15[,'3']
mydata15 <- SampleB[,c('PCM','CM_SA','Age','Educ')]
mydata15 <- mydata15[complete.cases(mydata15),]
mydata15_num <- mydata15 %>% dplyr::select_if(is.numeric)
predictors15 <- colnames(mydata15_num)

mydata15_PCM1Age_num <- data.frame(mydata15_num[,'Age']) %>%
  mutate(logit = log(probablities15_PCM1/(1-probablities15_PCM1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata15_PCM2Age_num <- data.frame(mydata15_num[,'Age']) %>%
  mutate(logit = log(probablities15_PCM2/(1-probablities15_PCM2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata15_PCM3Age_num <- data.frame(mydata15_num[,'Age']) %>%
  mutate(logit = log(probablities15_PCM3/(1-probablities15_PCM3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata15_PCM1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 1, Model #15")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #15"

ggplot(mydata15_PCM2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 2, Model #15")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #15"

ggplot(mydata15_PCM3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 3, Model #15")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #15"

# 20.2. Model #15 charactieristics
Stats_model15 <- model3to9and13to15_info(model15)

# 20.3. Export dataframe of summary statistics of model #15 into a csv file 
write.csv(Stats_model15,"ALSPAC.partner_MODEL15_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date



#################### 21. Statistical analysis Model #16 #################### 
SampleB$CM_sev <- relevel(SampleB$CM_sev, ref='0')
model16 <- multinom(PCM ~ CM_sev + Age  + Educ, data = SampleB)

# 21.1. Test assumptions for multinomial regression - Model #16
# Linearity assumption
probabilities16 <- predict(model16, type = "prob", na.rm=T)
probablities16_PCM1 <- probabilities16[,'1']
probablities16_PCM2 <- probabilities16[,'2']
probablities16_PCM3 <- probabilities16[,'3']
mydata16 <- SampleB[,c('PCM','CM_sev','Age','Educ')]
mydata16 <- mydata16[complete.cases(mydata16),]
mydata16_num <- mydata16 %>% dplyr::select_if(is.numeric)
predictors16 <- colnames(mydata16_num)

mydata16_PCM1Age_num <- data.frame(mydata16_num[,'Age']) %>%
  mutate(logit = log(probablities16_PCM1/(1-probablities16_PCM1))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata16_PCM2Age_num <- data.frame(mydata16_num[,'Age']) %>%
  mutate(logit = log(probablities16_PCM2/(1-probablities16_PCM2))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

mydata16_PCM3Age_num <- data.frame(mydata16_num[,'Age']) %>%
  mutate(logit = log(probablities16_PCM3/(1-probablities16_PCM3))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata16_PCM1Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 1, Model #16")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #16"

ggplot(mydata16_PCM2Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 2, Model #16")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #16"

ggplot(mydata16_PCM3Age_num, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y") +
  labs(title="Age per logit, at PCM = 3, Model #16")                        #Copy paste this graph in the Excel document "General_Info_Models.xlsx", under tab "model #16"

# 21.2. Model #16 characteristics
model16_info <- function(model){
  #Select only complete cases in the dataset
  mydata <- SampleB[,all.vars(formula(model))]
  mydata <- mydata[complete.cases(mydata),]
  # Create objects to store desired model characteristics
  vif <- car::vif(model)['CM_sev','GVIF']
  N <- nrow(mydata)
  n_males <- nrow(mydata)
  mean_age <- mean(mydata$Age,na.rm=T)
  median_age <- median(mydata$Age,na.rm = T)
  sd_age <- sd(mydata$Age,na.rm = T)
  n_educ0 <- table(mydata$Educ)[1]
  n_educ1 <- table(mydata$Educ)[2]
  n_educ2 <- table(mydata$Educ)[3]
  source("http://pcwww.liv.ac.uk/~william/R/crosstab.r")
  crosstab <- crosstab(mydata, row.vars = "CM_sev", col.vars = all.vars(formula(model))[1], type = "f")
  n_CM_sev0_outcome0 <- crosstab$crosstab['0','0']
  n_CM_sev1_outcome0 <- crosstab$crosstab['1','0']
  n_CM_sev2_outcome0 <- crosstab$crosstab['2','0']
  n_CM_sev3_outcome0 <- crosstab$crosstab['3','0']
  n_CM_sevsum_outcome0 <- crosstab$crosstab['Sum','0']
  n_CM_sev0_outcome1 <- crosstab$crosstab['0','1']
  n_CM_sev1_outcome1 <- crosstab$crosstab['1','1']
  n_CM_sev2_outcome1 <- crosstab$crosstab['2','1']
  n_CM_sev3_outcome1 <- crosstab$crosstab['3','1']
  n_CM_sevsum_outcome1 <- crosstab$crosstab['Sum','1']
  n_CM_sev0_outcome2 <- crosstab$crosstab['0','2']
  n_CM_sev1_outcome2 <- crosstab$crosstab['1','2']
  n_CM_sev2_outcome2 <- crosstab$crosstab['2','2']
  n_CM_sev3_outcome2 <- crosstab$crosstab['3','2']
  n_CM_sevsum_outcome2 <- crosstab$crosstab['Sum','2']
  n_CM_sev0_outcome3 <- crosstab$crosstab['0','3']
  n_CM_sev1_outcome3 <- crosstab$crosstab['1','3']
  n_CM_sev2_outcome3 <- crosstab$crosstab['2','3']
  n_CM_sev3_outcome3 <- crosstab$crosstab['3','3']
  n_CM_sevsum_outcomesum <- crosstab$crosstab['Sum','3']
  n_CM_sev0_outcomesum <- crosstab$crosstab['0','Sum']
  n_CM_sev1_outcomesum <- crosstab$crosstab['1','Sum']
  n_CM_sev2_outcomesum <- crosstab$crosstab['2','Sum']
  n_CM_sev3_outcomesum <- crosstab$crosstab['3','Sum']
  n_CM_sevsum_outcomesum <- crosstab$crosstab['Sum','Sum']
  # Record coefficient estimates log(OR), standard errors, and 95% CI of log(OR) of predictors' effects on the three levels of the outcome variable
  Estimate_CM_sev1_outcome1 <- summary(model)$coefficients[1,'CM_sev1']
  Std.Error_CM_sev1_outcome1 <- summary(model)$standard.errors[1,'CM_sev1']
  CI_LL_logOR_CM_sev1_outcome1 <- confint(model)['CM_sev1',1,1]
  CI_UL_logOR_CM_sev1_outcome1 <- confint(model)['CM_sev1',2,1]
  Estimate_CM_sev1_outcome2 <- summary(model)$coefficients[2,'CM_sev1']
  Std.Error_CM_sev1_outcome2 <- summary(model)$standard.errors[2,'CM_sev1']
  CI_LL_logOR_CM_sev1_outcome2 <- confint(model)['CM_sev1',1,2]
  CI_UL_logOR_CM_sev1_outcome2 <- confint(model)['CM_sev1',2,2]
  Estimate_CM_sev1_outcome3 <- summary(model)$coefficients[3,'CM_sev1']
  Std.Errors_CM_sev1_outcome3 <- summary(model)$standard.errors[3,'CM_sev1']
  CI_LL_logOR_CM_sev1_outcome3 <- confint(model)['CM_sev1',1,3]
  CI_UL_logOR_CM_sev1_outcome3 <- confint(model)['CM_sev1',2,3]
  Estimate_CM_sev2_outcome1 <- summary(model)$coefficients[1,'CM_sev2']
  Std.Error_CM_sev2_outcome1 <- summary(model)$standard.errors[1,'CM_sev2']
  CI_LL_logOR_CM_sev2_outcome1 <- confint(model)['CM_sev2',1,1]
  CI_UL_logOR_CM_sev2_outcome1 <- confint(model)['CM_sev2',2,1]
  Estimate_CM_sev2_outcome2 <- summary(model)$coefficients[2,'CM_sev2']
  Std.Error_CM_sev2_outcome2 <- summary(model)$standard.errors[2,'CM_sev2']
  CI_LL_logOR_CM_sev2_outcome2 <- confint(model)['CM_sev2',1,2]
  CI_UL_logOR_CM_sev2_outcome2 <- confint(model)['CM_sev2',2,2]
  Estimate_CM_sev2_outcome3 <- summary(model)$coefficients[3,'CM_sev2']
  Std.Errors_CM_sev2_outcome3 <- summary(model)$standard.errors[3,'CM_sev2']
  CI_LL_logOR_CM_sev2_outcome3 <- confint(model)['CM_sev2',1,3]
  CI_UL_logOR_CM_sev2_outcome3 <- confint(model)['CM_sev2',2,3]
  Estimate_CM_sev3_outcome1 <- summary(model)$coefficients[1,'CM_sev3']
  Std.Error_CM_sev3_outcome1 <- summary(model)$standard.errors[1,'CM_sev3']
  CI_LL_logOR_CM_sev3_outcome1 <- confint(model)['CM_sev3',1,1]
  CI_UL_logOR_CM_sev3_outcome1 <- confint(model)['CM_sev3',2,1]
  Estimate_CM_sev3_outcome2 <- summary(model)$coefficients[2,'CM_sev3']
  Std.Error_CM_sev3_outcome2 <- summary(model)$standard.errors[2,'CM_sev3']
  CI_LL_logOR_CM_sev3_outcome2 <- confint(model)['CM_sev3',1,2]
  CI_UL_logOR_CM_sev3_outcome2 <- confint(model)['CM_sev3',2,2]
  Estimate_CM_sev3_outcome3 <- summary(model)$coefficients[3,'CM_sev3']
  Std.Errors_CM_sev3_outcome3 <- summary(model)$standard.errors[3,'CM_sev3']
  CI_LL_logOR_CM_sev3_outcome3 <- confint(model)['CM_sev3',1,3]
  CI_UL_logOR_CM_sev3_outcome3 <- confint(model)['CM_sev3',2,3]
  Estimate_Age_outcome1 <- summary(model)$coefficients['1','Age']
  Std.Error_Age_outcome1 <- summary(model)$standard.errors[1,'Age']
  CI_LL_logOR_Age_outcome1 <- confint(model)['Age',1,1]
  CI_UL_logOR_Age_outcome1 <- confint(model)['Age',2,1]
  # Estimate_Sex1_outcome1 <- summary(model)$coefficients['1','Sex1']
  # Std.Error_Sex1_outcome1 <- summary(model)$standard.errors[1,'Sex1']
  # CI_LL_logOR_Sex1_outcome1 <- confint(model)['Sex1',1,1]
  # CI_UL_logOR_Sex1_outcome1 <- confint(model)['Sex1',2,1]
  Estimate_Educ1_outcome1 <- summary(model)$coefficients['1','Educ1']
  Std.Error_Educ1_outcome1 <- summary(model)$standard.errors[1,'Educ1']
  CI_LL_logOR_Educ1_outcome1 <- confint(model)['Educ1',1,1]
  CI_UL_logOR_Educ1_outcome1 <- confint(model)['Educ1',2,1]
  Estimate_Educ2_outcome1 <- summary(model)$coefficients['1','Educ2']
  Std.Error_Educ2_outcome1 <- summary(model)$standard.errors['1','Educ2']
  CI_LL_logOR_Educ2_outcome1 <- confint(model)['Educ2',1,1]
  CI_UL_logOR_Educ2_outcome1 <- confint(model)['Educ2',2,1]
  Estimate_Age_outcome2 <- summary(model)$coefficients['2','Age']
  Std.Error_Age_outcome2 <- summary(model)$standard.errors[2,'Age']
  CI_LL_logOR_Age_outcome2 <- confint(model)['Age',1,2]
  CI_UL_logOR_Age_outcome2 <- confint(model)['Age',2,2]
  # Estimate_Sex1_outcome2 <- summary(model)$coefficients['2','Sex1']
  # Std.Error_Sex1_outcome2 <- summary(model)$standard.errors[2,'Sex1']
  # CI_LL_logOR_Sex1_outcome2 <- confint(model)['Sex1',1,2]
  # CI_UL_logOR_Sex1_outcome2 <- confint(model)['Sex1',2,2]
  Estimate_Educ1_outcome2 <- summary(model)$coefficients['2','Educ1']
  Std.Error_Educ1_outcome2 <- summary(model)$standard.errors[2,'Educ1']
  CI_LL_logOR_Educ1_outcome2 <- confint(model)['Educ1',1,2]
  CI_UL_logOR_Educ1_outcome2 <- confint(model)['Educ1',2,2]
  Estimate_Educ2_outcome2 <- summary(model)$coefficients['2','Educ2']
  Std.Error_Educ2_outcome2 <- summary(model)$standard.errors['2','Educ2']
  CI_LL_logOR_Educ2_outcome2 <- confint(model)['Educ2',1,2]
  CI_UL_logOR_Educ2_outcome2 <- confint(model)['Educ2',2,2]
  Estimate_Age_outcome3 <- summary(model)$coefficients['3','Age']
  Std.Error_Age_outcome3 <- summary(model)$standard.errors[3,'Age']
  CI_LL_logOR_Age_outcome3 <- confint(model)['Age',1,3]
  CI_UL_logOR_Age_outcome3 <- confint(model)['Age',2,3]
  # Estimate_Sex1_outcome3 <- summary(model)$coefficients['3','Sex1']
  # Std.Error_Sex1_outcome3 <- summary(model)$standard.errors[3,'Sex1']
  # CI_LL_logOR_Sex1_outcome3 <- confint(model)['Sex1',1,3]
  # CI_UL_logOR_Sex1_outcome3 <- confint(model)['Sex1',2,3]
  Estimate_Educ1_outcome3 <- summary(model)$coefficients['3','Educ1']
  Std.Error_Educ1_outcome3 <- summary(model)$standard.errors[3,'Educ1']
  CI_LL_logOR_Educ1_outcome3 <- confint(model)['Educ1',1,3]
  CI_UL_logOR_Educ1_outcome3 <- confint(model)['Educ1',2,3]
  Estimate_Educ2_outcome3 <- summary(model)$coefficients['3','Educ2']
  Std.Error_Educ2_outcome3 <- summary(model)$standard.errors['3','Educ2']
  CI_LL_logOR_Educ2_outcome3 <- confint(model)['Educ2',1,3]
  CI_UL_logOR_Educ2_outcome3 <- confint(model)['Educ2',2,3]
  # Gather all model information in dataframe
  Stats_model <- data.frame(vif,
                            N,
                            n_males,
                            mean_age,
                            median_age,
                            sd_age,
                            n_educ0,
                            n_educ1,
                            n_educ2,
                            n_CM_sev0_outcome0,
                            n_CM_sev1_outcome0,
                            n_CM_sev2_outcome0,
                            n_CM_sev3_outcome0,
                            n_CM_sevsum_outcome0,
                            n_CM_sev0_outcome1,
                            n_CM_sev1_outcome1,
                            n_CM_sev2_outcome1,
                            n_CM_sev3_outcome1,
                            n_CM_sevsum_outcome1,
                            n_CM_sev0_outcome2,
                            n_CM_sev1_outcome2,
                            n_CM_sev2_outcome2,
                            n_CM_sev3_outcome2,
                            n_CM_sevsum_outcome2,
                            n_CM_sev0_outcome3,
                            n_CM_sev1_outcome3,
                            n_CM_sev2_outcome3,
                            n_CM_sev3_outcome3,
                            n_CM_sevsum_outcomesum,
                            n_CM_sev0_outcomesum,
                            n_CM_sev1_outcomesum,
                            n_CM_sev2_outcomesum,
                            n_CM_sev3_outcomesum,
                            n_CM_sevsum_outcomesum,
                            Estimate_CM_sev1_outcome1,
                            Std.Error_CM_sev1_outcome1,
                            CI_LL_logOR_CM_sev1_outcome1,
                            CI_UL_logOR_CM_sev1_outcome1,
                            Estimate_CM_sev1_outcome2,
                            Std.Error_CM_sev1_outcome2,
                            CI_LL_logOR_CM_sev1_outcome2,
                            CI_UL_logOR_CM_sev1_outcome2,
                            Estimate_CM_sev1_outcome3,
                            Std.Errors_CM_sev1_outcome3,
                            CI_LL_logOR_CM_sev1_outcome3,
                            CI_UL_logOR_CM_sev1_outcome3,
                            Estimate_CM_sev2_outcome1,
                            Std.Error_CM_sev2_outcome1,
                            CI_LL_logOR_CM_sev2_outcome1,
                            CI_UL_logOR_CM_sev2_outcome1,
                            Estimate_CM_sev2_outcome2,
                            Std.Error_CM_sev2_outcome2,
                            CI_LL_logOR_CM_sev2_outcome2,
                            CI_UL_logOR_CM_sev2_outcome2,
                            Estimate_CM_sev2_outcome3,
                            Std.Errors_CM_sev2_outcome3,
                            CI_LL_logOR_CM_sev2_outcome3,
                            CI_UL_logOR_CM_sev2_outcome3,
                            Estimate_CM_sev3_outcome1,
                            Std.Error_CM_sev3_outcome1,
                            CI_LL_logOR_CM_sev3_outcome1,
                            CI_UL_logOR_CM_sev3_outcome1,
                            Estimate_CM_sev3_outcome2,
                            Std.Error_CM_sev3_outcome2,
                            CI_LL_logOR_CM_sev3_outcome2,
                            CI_UL_logOR_CM_sev3_outcome2,
                            Estimate_CM_sev3_outcome3,
                            Std.Errors_CM_sev3_outcome3,
                            CI_LL_logOR_CM_sev3_outcome3,
                            CI_UL_logOR_CM_sev3_outcome3,
                            Estimate_Age_outcome1,
                            Std.Error_Age_outcome1,
                            CI_LL_logOR_Age_outcome1,
                            CI_UL_logOR_Age_outcome1,
                            # Estimate_Sex1_outcome1,
                            # Std.Error_Sex1_outcome1,
                            # CI_LL_logOR_Sex1_outcome1,
                            # CI_UL_logOR_Sex1_outcome1,
                            Estimate_Educ1_outcome1,
                            Std.Error_Educ1_outcome1,
                            CI_LL_logOR_Educ1_outcome1,
                            CI_UL_logOR_Educ1_outcome1,
                            Estimate_Educ2_outcome1,
                            Std.Error_Educ2_outcome1,
                            CI_LL_logOR_Educ2_outcome1,
                            CI_UL_logOR_Educ2_outcome1,
                            Estimate_Age_outcome2,
                            Std.Error_Age_outcome2,
                            CI_LL_logOR_Age_outcome2,
                            CI_UL_logOR_Age_outcome2,
                            # Estimate_Sex1_outcome2,
                            # Std.Error_Sex1_outcome2,
                            # CI_LL_logOR_Sex1_outcome2,
                            # CI_UL_logOR_Sex1_outcome2,
                            Estimate_Educ1_outcome2,
                            Std.Error_Educ1_outcome2,
                            CI_LL_logOR_Educ1_outcome2,
                            CI_UL_logOR_Educ1_outcome2,
                            Estimate_Educ2_outcome2,
                            Std.Error_Educ2_outcome2,
                            CI_LL_logOR_Educ2_outcome2,
                            CI_UL_logOR_Educ2_outcome2,
                            Estimate_Age_outcome3,
                            Std.Error_Age_outcome3,
                            CI_LL_logOR_Age_outcome3,
                            CI_UL_logOR_Age_outcome3,
                            # Estimate_Sex1_outcome3,
                            # Std.Error_Sex1_outcome3,
                            # CI_LL_logOR_Sex1_outcome3,
                            # CI_UL_logOR_Sex1_outcome3,
                            Estimate_Educ1_outcome3,
                            Std.Error_Educ1_outcome3,
                            CI_LL_logOR_Educ1_outcome3,
                            CI_UL_logOR_Educ1_outcome3,
                            Estimate_Educ2_outcome3,
                            Std.Error_Educ2_outcome3,
                            CI_LL_logOR_Educ2_outcome3,
                            CI_UL_logOR_Educ2_outcome3)
  return(Stats_model)
}

Stats_model16 <- model16_info(model16)

# 21.3 Export dataframe of statistics of model #16 into a csv file
write.csv(Stats_model16,"ALSPAC.partner_MODEL16_20210326.csv", row.names = TRUE)  #Change path to working directory on your computer and re-name file with cohort's name and date


dev.off()

###################### END ######################


# A couple of examples of the outputs created by the code above
setwd("~/Documents/VUmc/EarlyCause/MetaAnalyses/PCMmultimorbidity/OutputCohorts/NESDA")
read.table(file = "NESDA_model1_20210326.csv", header = T, sep = ",") # Model 1 - Example of the output of a binary logistic regression
read.table(file = "NESDA_model3_20210326.csv", header = T, sep = ",") # Model 3 - Example of the output of a multinomial logistic regression adjusted for all covariates
read.table(file = "NESDA_model4_20210326.csv", header = T, sep = ",") # Model 4 - Example of the output of a multinomial logistic regression adjusted for basic covariated only

