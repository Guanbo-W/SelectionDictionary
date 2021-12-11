# Load packages ######################################################################
devtools::install_github("YaohuiZeng/grpregOverlap")
library(grpregOverlap)
require(data.table)
library(haven)
library(dplyr)
library(glmnet)
library(tableone)

set.seed(2019)

Data <- read_sas("xxx")

##################################### available variables in HAS-BLED
names=c("all_major_bleed_fup",
        "bled_renal_3yr_pr_ac","liver","bled_stroke_3yr_pr_ac", "maj_bleed","bled_age_gt_65_ac","bled_drugs_ac")
data=Data[-which(Data$adherence_hypotens_yr_pr_index %in% NA),which(colnames(Data) %in% names)]
# crude OR
result=list();ci=list();est=NULL;covariate_names=NULL
for (i in c(1:6)) {
  covariate_names=colnames(data[,i])
  fomula=paste("all_major_bleed_fup",covariate_names,sep ="~")
  result[[i]]=exp(coef(glm(fomula,family=binomial(),data=data))[2])
  ci[[i]]=exp(confint(glm(fomula,family=binomial(),data=data))[2,])
  a=c(result[[i]],ci[[i]])
  est=rbind(est,a)
}
rownames(est)=colnames(data[,c(1:6)])
round(est,2)
# adjusted OR
covariate_names=paste(colnames(data[,-c(7)]),collapse ="+")
fomula=paste("all_major_bleed_fup",covariate_names,sep ="~")
round(cbind(exp(coef(glm(fomula,family=binomial(),data=data))),
            exp(confint(glm(fomula,family=binomial(),data=data)))),2)

##################################### available variables in "predictors for bleeding on oral anticoagulation" (2012, Lane and Lip, circulation) 
names=c("all_major_bleed_fup",
        "age_75_or_more","maj_bleed","bled_stroke_3yr_pr_ac","bled_renal_3yr_pr_ac", "anemia","female","liver","malign_cancer","adherence_hypotens_yr_pr_index", "antiplatel_2w_pr","nsaids_2w_pr","antidiab_2w_pr","antidepre_2w_pr","ppi_2w_pr","antibiotics_2w_pr","anti_arrhythmics_2w_pr","pgp_inhib_2w_pr")
data=Data[-which(Data$adherence_hypotens_yr_pr_index %in% NA),which(colnames(Data) %in% names)]
data$Adh=ifelse(data$adherence_hypotens_yr_pr_index<0.8,0,1)
data$othermedication=ifelse(data$antidiab_2w_pr==1|data$antidepre_2w_pr==1|data$ppi_2w_pr==1|data$antibiotics_2w_pr==1|data$anti_arrhythmics_2w_pr==1|data$pgp_inhib_2w_pr==1,1,0)
data=data[,-which(colnames(data) %in% c("antidiab_2w_pr","antidepre_2w_pr","ppi_2w_pr","antibiotics_2w_pr","anti_arrhythmics_2w_pr","pgp_inhib_2w_pr","adherence_hypotens_yr_pr_index"))]
# crude OR
result=list();ci=list();est=NULL;covariate_names=NULL
for (i in c(1:6,8:13)) {
  covariate_names=colnames(data[,i])
  fomula=paste("all_major_bleed_fup",covariate_names,sep ="~")
  result[[i]]=exp(coef(glm(fomula,family=binomial(),data=data))[2])
  ci[[i]]=exp(confint(glm(fomula,family=binomial(),data=data))[2,])
  a=c(result[[i]],ci[[i]])
  est=rbind(est,a)
}
rownames(est)=colnames(data[,c(1:6,8:13)])
round(est,2)
# adjusted OR
covariate_names=paste(colnames(data[,-c(7)]),collapse ="+")
fomula=paste("all_major_bleed_fup",covariate_names,sep ="~")
round(cbind(exp(coef(glm(fomula,family=binomial(),data=data))),
            exp(confint(glm(fomula,family=binomial(),data=data)))),2)

######################################### Our full model
names=c("all_major_bleed_fup",
        "age_75_or_more","female","bled_stroke_3yr_pr_ac","anemia","liver","malign_cancer","bled_renal_3yr_pr_ac","maj_bleed","antiplatel_2w_pr","nsaids_2w_pr",
        "cha2sds2_vasc_3yr_pr_ac_ge_3",
        "type_oac","dose_mg_doac_index","adherence_hypotens_yr_pr_index","dabigat_110_mg","dabigat_150_mg","apix_5_mg","apix_2_5_mg",
        "antidepre_2w_pr","ppi_2w_pr",
        "vhd","pvd","cardiomyopathy","chf","mi",
        "diabet","copd_asthma","dyslip"
)
data=Data[-which(Data$adherence_hypotens_yr_pr_index %in% NA),which(colnames(Data) %in% names)]
data$Type=ifelse(data$type_oac=="1) WARFARINE",0,1)
data$Dose=ifelse(data$dose_mg_doac_index %in% c(5,20,150),1,0)
data$Adh=ifelse(data$adherence_hypotens_yr_pr_index<0.8,0,1)
data$Api=ifelse(data$dabigat_110_mg==1|data$dabigat_150_mg==1,1,0)
data$Dab=ifelse(data$apix_5_mg==1|data$apix_2_5_mg==1,1,0)
data=data[,-which(colnames(data) %in% c("type_oac","dose_mg_doac_index","adherence_hypotens_yr_pr_index","dabigat_110_mg","dabigat_150_mg","apix_5_mg","apix_2_5_mg"))]
data$Type_Adh=data$Type*data$Adh
data$Dose_Adh=data$Dose*data$Adh
data$T_antiplatel=data$Type*data$antiplatel_2w_pr
data$T_nsaids=data$Type*data$nsaids_2w_pr
data$T_antidepre=data$Type*data$antidepre_2w_pr
data$T_ppi=data$Type*data$ppi_2w_pr
data$heart1=ifelse(data$vhd==1 | data$pvd==1 | data$cardiomyopathy==1
                   | data$chf==1 | data$mi==1,1,0)
data=data[,-which(colnames(data) %in% c("vhd","pvd","cardiomyopathy","chf", "mi"))]
data=data[,c(14, 13, 15, 
             11, 17, 16, 4, 3, 10, 29, 2, 5, 1,
             18, 21, 22, 19, 20, 23, 24, 
             6, 9, 8, 7, 
             25, 26, 27, 28, 
             12)]
# crude OR
result=list();ci=list();est=NULL;covariate_names=NULL
for (i in c(1:11,13:29)) {
  covariate_names=colnames(data[,i])
  fomula=paste("all_major_bleed_fup",covariate_names,sep ="~")
  result[[i]]=exp(coef(glm(fomula,family=binomial(),data=data))[2])
  ci[[i]]=exp(confint(glm(fomula,family=binomial(),data=data))[2,])
  a=c(result[[i]],ci[[i]])
  est=rbind(est,a)
}
rownames(est)=colnames(data[,c(1:11,13:29)])
round(est,2)
# adjusted OR
covariate_names=paste(colnames(data[,-c(12)]),collapse ="+")
fomula=paste("all_major_bleed_fup",covariate_names,sep ="~")
round(cbind(exp(coef(glm(fomula,family=binomial(),data=data))),
            exp(confint(glm(fomula,family=binomial(),data=data)))),2)

#############  TableOne
X=as.matrix(data[,-29],)
y=as.vector(data$all_major_bleed_fup)
tableone1=CreateTableOne(vars=colnames(X),strata="all_major_bleed_fup",data=data)
data$ddd=ifelse(data$Type==0,"war",ifelse(data$Dose==1,"high","low"))
tableone2=CreateTableOne(vars=colnames(data),strata="ddd",data=data)
data=data[,-30]

############# Lasso
lassofit=cv.glmnet(X,y,family = "binomial")
exp(coef(lassofit, s=lassofit$lambda.min))

############# AdaLasso
ridge1_cv=cv.glmnet(X,y,family = "binomial",type.measure = "mse",alpha=0)
best_ridge_coef=as.numeric(coef(ridge1_cv,s=ridge1_cv$lambda.min))[-1]
alasso_cv=cv.glmnet(X,y,family = "binomial",type.measure = "mse",alpha=1,penalty.factor = 1/abs(best_ridge_coef),keep=T)
exp(coef(alasso_cv,s=alasso_cv$lambda.min))

############# LOGL #############################
group=list(c(3),c(10),c(11),c(12),c(13),
           c(14),c(14, 17),c(14,15),c(14,16),c(18),c(14, 18, 19),c(14, 17, 18, 19, 20),
           c(14, 25),c(14, 26),
           c(23),c(14,23,27),c(24),c(14,24,28),
           c(1,8, 4, 5, 2, 9, 7, 6, 21, 22))

cvfit.grLasso <- cv.grpregOverlap(X, y, group, penalty = 'grLasso', family = 'binomial',trace = TRUE)
cvfit.grMCP <- cv.grpregOverlap(X, y, group, penalty = 'grMCP', family = 'binomial',trace = TRUE)
cvfit.grSCAD <- cv.grpregOverlap(X, y, group, penalty = 'grSCAD', family = 'binomial',trace = TRUE)

exp(coef(cvfit.grLasso))
which(coef(cvfit.grLasso)!=0)
summary(cvfit.grLasso)

exp(coef(cvfit.grMCP))
which(coef(cvfit.grMCP)!=0)
summary(cvfit.grMCP)

exp(coef(cvfit.grSCAD))
which(coef(cvfit.grSCAD)!=0)
summary(cvfit.grSCAD)

