---
title: "CCPEBaseline"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Cleaning HCS data
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/HealthCapitalScale")
HCS = read.csv("CIL_RCS_Variables_6142018.csv", header = TRUE)
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library("Hmisc")
head(HCS)
HCS = HCS[c("work_environment", "work_job", "fin_resources", "leg_comply", "rel_partner", "rel_family", "rel_friends", "rel_clean_users", "rel_groups_access", "rel_group_member", "rel_sponsor", "rel_online", "rel_sup_others", "liv_symbols", "men_profound", "rel_participate", "form_sup_rec_prog", "form_sup_pcp", "rel_program", "drug_mat", "form_sup_insurance", "med_health", "health_plan", "men_rituals", "prob_skills", "men_self", "men_purpose", "rel_service", "men_hopes", "rel_values", "liv_safe", "liv_nut_meals", "liv_transport", "liv_clothes", "drug_environment", "men_activity", "drug_abstain", "form_sup_bhealth_prog", "men_manage")]
HCS = HCS[c("work_environment", "work_job", "fin_resources", "rel_clean_users", "rel_group_member", "rel_sponsor", "rel_sup_others", "drug_environment", "men_activity", "drug_abstain", "men_manage", "men_hopes")]
head(HCS)
```
Get descriptives
```{r}
describe(HCS)
```


Build model for initial testing.  Get rid of missing values for cronbach alpha

Now get the reliability for the three measures on their own. 
model1 = "Work =~ work_environment+ work_job + fin_resources
          Rel =~ rel_clean_users + rel_group_member + rel_sponsor + rel_sup_others
          Live =~ drug_environment + men_activity + drug_abstain + men_manage + men_hopes"
```{r}

write.csv(HCS, "HCS.csv", row.names = FALSE)
HCS = read.csv("HCS.csv", header = TRUE, na.strings = c("NULL"))
Work = HCS[,1:3]
Rel = HCS[,4:7]
Live = HCS[,8:12]
HCSAlpha = list(Work, Rel, Live)
lapply(HCSAlpha, function(x) alpha(x))
alpha(HCS)
```
Ok try this based on rotated EFA:
Factor 1 = work_environment, work_job, fin_resources
Factor 2 = rel_clean_users, rel_group_member, rel_sponsor, rel_sup_others  
Factor 3 = liv_safe, liv_nut_meals, drug_environment, men_activity, drug_abstain, men_manage
```{r}
model1 = "Work =~ work_environment+ work_job + fin_resources
          Rel =~ rel_clean_users + rel_group_member + rel_sponsor + rel_sup_others
          Live =~ drug_environment + men_activity + drug_abstain + men_manage + men_hopes"

fit1 = cfa(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = HCS)
summary(fit1, fit.measures = TRUE, standardized = TRUE)
```
To get predictive validity, just use the totatl score for each construct and correlation that with other measures
```{r}
#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/HealthCapitalScale")
#HCS = read.csv("CIL_RCS_Variables_6142018.csv", header = TRUE)
#HCSPred = read.csv("CIL_RCS_Variables_6142018.csv", header = TRUE)
datPred = list(Work, Rel, Live)

datPredloop = NULL
for(i in 1:3){
  datPredloop[[i]]= apply(datPred[[i]], 1, sum)
}
datPred = data.frame(datPredloop)
colnames(datPred) = c("Work", "Rel", "Live")
datPred




HCSPred = HCSPred[,83:91]
write.csv(HCSPred, "HCSPred.csv", row.names = FALSE)
HCSPred = read.csv("HCSPred.csv", header = TRUE, na.strings = c("NULL"))
HCSPred = cbind(HCSPred, datPred)
dim(HCSPred)
HCSPred = na.omit(HCSPred)
corsPred =  rcorr(as.matrix(HCSPred), type = "spearman")
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
    )
}

corsPred = flattenCorrMatrix(corsPred$r, corsPred$P)
corsPred = subset(corsPred, cor >= .3 | cor <= -.3)
corsPred
```



