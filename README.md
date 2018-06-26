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
head(HCS)
HCS = HCS[c("work_environment", "work_job", "fin_resources", "leg_comply", "rel_partner", "rel_family", "rel_friends", "rel_clean_users", "rel_groups_access", "rel_group_member", "rel_sponsor", "rel_online", "rel_sup_others", "liv_symbols", "men_profound", "rel_participate", "form_sup_rec_prog", "form_sup_pcp", "rel_program", "drug_mat", "form_sup_insurance", "med_health", "health_plan", "men_rituals", "prob_skills", "men_self", "men_purpose", "rel_service", "men_hopes", "rel_values", "liv_safe", "liv_nut_meals", "liv_transport", "liv_clothes", "drug_environment", "men_activity", "drug_abstain", "form_sup_bhealth_prog", "men_manage")]
head(HCS)
```
Build model for initial testing.  Get rid of missing values for cronbach alpha
```{r}
dim(HCS)
typeof(HCS)
write.csv(HCS, "HCS.csv", row.names = FALSE)
HCS = read.csv("HCS.csv", header = TRUE, na.strings = c("NULL"))
alpha(HCS)
```
Now put together CFA model
```{r}
model1 = "HCS =~ work_environment+ work_job+ fin_resources+ leg_comply+ rel_partner+ rel_family+ rel_friends+ rel_clean_users+ rel_groups_access+ rel_group_member+ rel_sponsor+ rel_online+ rel_sup_others+ liv_symbols+ men_profound+ rel_participate+ form_sup_rec_prog+ form_sup_pcp+ rel_program+ drug_mat+ form_sup_insurance+ med_health+ health_plan+ men_rituals+ prob_skills+ men_self+ men_purpose+ rel_service+ men_hopes+ rel_values+ liv_safe+ liv_nut_meals+ liv_transport+ liv_clothes+ drug_environment+ men_activity+ drug_abstain+ form_sup_bhealth_prog+ men_manage"

fit1 = cfa(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = HCS)
summary(fit1, fit.measures = TRUE)

```
Ok drop leg_comply and  form_sup_nsrnc (form_sup_insurance) were dropped.
```{r}
model1 = "HCS =~ work_environment+ work_job+ fin_resources+ rel_partner+ rel_family+ rel_friends+ rel_clean_users+ rel_groups_access+ rel_group_member+ rel_sponsor+ rel_online+ rel_sup_others+ liv_symbols+ men_profound+ rel_participate+ form_sup_rec_prog+ form_sup_pcp+ rel_program+ drug_mat+ med_health+ health_plan+ men_rituals+ prob_skills+ men_self+ men_purpose+ rel_service+ men_hopes+ rel_values+ liv_safe+ liv_nut_meals+ liv_transport+ liv_clothes+ drug_environment+ men_activity+ drug_abstain+ form_sup_bhealth_prog+ men_manage"

fit1 = cfa(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = HCS)
summary(fit1, fit.measures = TRUE)

```
rel_clean_usrs form_sup_pcp 
```{r}
model1 = "HCS =~ work_environment+ work_job+ fin_resources+ rel_partner+ rel_family+ rel_friends+ rel_groups_access+ rel_group_member+ rel_sponsor+ rel_online+ rel_sup_others+ liv_symbols+ men_profound+ rel_participate+ form_sup_rec_prog+ rel_program+ drug_mat+ med_health+ health_plan+ men_rituals+ prob_skills+ men_self+ men_purpose+ rel_service+ men_hopes+ rel_values+ liv_safe+ liv_nut_meals+ liv_transport+ liv_clothes+ drug_environment+ men_activity+ drug_abstain+ form_sup_bhealth_prog+ men_manage"

fit1 = cfa(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = HCS)
summary(fit1, fit.measures = TRUE)
```
rel_partner, rel_family, rel_grops_ccss, rel_sup_others, men_profound, form_sp_rc_prg, rel_program, drug_mat, prob_skills,liv_safe, liv_nut_meals,liv_clothes
```{r}
model1 = "HCS =~ work_environment+ work_job+ fin_resources+ rel_friends+ rel_group_member+ rel_sponsor+ rel_online+ liv_symbols+ rel_participate+ med_health+ health_plan+ men_rituals+ men_self+ men_purpose+ rel_service+ men_hopes+ rel_values+ liv_transport+ drug_environment+ men_activity+ drug_abstain+ form_sup_bhealth_prog+ men_manage"

fit1 = cfa(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = HCS)
summary(fit1, fit.measures = TRUE)
```
frm_sp_bhlth_p, rel_online, liv_transport 
```{r}
model1 = "HCS =~ work_environment+ work_job+ fin_resources+ rel_friends+ rel_group_member+ rel_sponsor+ liv_symbols+ rel_participate+ med_health+ health_plan+ men_rituals+ men_self+ men_purpose+ rel_service+ men_hopes+ rel_values+ drug_environment+ men_activity+ drug_abstain+ men_manage"

fit1 = cfa(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = HCS)
summary(fit1, fit.measures = TRUE)
```
med_health, health_plan, rel_values, rel_friends
```{r}
model1 = "HCS =~ work_environment+ work_job+ fin_resources+ rel_group_member+ rel_sponsor+ liv_symbols+ rel_participate+ men_rituals+ men_self+ men_purpose+ rel_service+ men_hopes+ drug_environment+ men_activity+ drug_abstain+ men_manage"

fit1 = cfa(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = HCS)
summary(fit1, fit.measures = TRUE)

```
men_purpose, drug_envirnmnt
```{r}
model1 = "HCS =~ work_environment+ work_job+ fin_resources+ rel_group_member+ rel_sponsor+ liv_symbols+ rel_participate+ men_rituals+ men_self+ rel_service+ men_hopes+ men_activity+ drug_abstain+ men_manage"

fit1 = cfa(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = HCS)
summary(fit1, fit.measures = TRUE)
```
men_hopes, rel_service, men_self, fin_resources
```{r}
model1 = "HCS =~ work_environment+ work_job+ rel_group_member+ rel_sponsor+ liv_symbols+ rel_participate+ men_rituals+ men_activity+ drug_abstain+ men_manage"

fit1 = cfa(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = HCS)
summary(fit1, fit.measures = TRUE)
```
Now try exploratory data analysis
```{r}
library(lavaan)
library(semTools)
library(GPArotation)



parallel = fa.parallel(HCS, fa= "fa")
parallel$fa.values

prinAnalysis = principal(HCS, nfactors = 5, rotate="varimax", missing = TRUE)

prinAnalysis$loadings


unrotated <- efaUnrotate(HCS, nf=3, estimator="mlr", missing = "ML")
summary(unrotated, std=TRUE)
inspect(unrotated, "std")

# Rotated by Quartimin
rotated <- oblqRotate(unrotated, method="quartimin")
summary(rotated, sort=FALSE, suppress=0.4)
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
summary(fit1, fit.measures = TRUE)
```



