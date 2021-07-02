#Name: Karl Michael Jacinto
#Date: 24/06/2020

#Sets the working directory
setwd("~/Third Year/T2/ACTL3142/Assignment/Part B")

#Loads libraries
library(dplyr)
library(tidyr)
library(caret)

#Imports datasets
patient_beneficiary = read.csv("Medicare_Outpatient_Inpatient_Beneficiary_PartB.csv")
patient_beneficiary_eval = read.csv("Medicare_Outpatient_Inpatient_Beneficiary_Eval_PartB.csv")
provider = read.csv("Medicare_Provider_PartB.csv")
provider_eval = read.csv("Medicare_Provider_Eval_PartB.csv")

View(patient_beneficiary)
View(patient_beneficiary_eval)

#check for duplicate values
sum(duplicated(patient_beneficiary))
sum(duplicated(patient_beneficiary_eval))

#Converting Claim Dates to date formats to calculate the claim process time
patient_beneficiary$ClaimEndDt = as.Date(patient_beneficiary$ClaimEndDt)
patient_beneficiary$ClaimStartDt = as.Date(patient_beneficiary$ClaimStartDt)
patient_beneficiary$ClaimProcessTime = as.numeric(patient_beneficiary$ClaimEndDt 
                                        - patient_beneficiary$ClaimStartDt)

patient_beneficiary_eval$ClaimEndDt = as.Date(patient_beneficiary_eval$ClaimEndDt)
patient_beneficiary_eval$ClaimStartDt = as.Date(patient_beneficiary_eval$ClaimStartDt)
patient_beneficiary_eval$ClaimProcessTime = as.numeric(patient_beneficiary_eval$ClaimEndDt 
                                                  - patient_beneficiary_eval$ClaimStartDt)

#Get the age value
patient_beneficiary$DOB = as.Date(patient_beneficiary$DOB)
patient_beneficiary$Dead = is.na(patient_beneficiary$DOD) == FALSE
patient_beneficiary$DOD = patient_beneficiary$DOD %>% replace_na('2010-01-01')
patient_beneficiary$DOD = as.Date(patient_beneficiary$DOD)
patient_beneficiary$Age = as.numeric((patient_beneficiary$DOD - patient_beneficiary$DOB) / 365)
patient_beneficiary$Dead[patient_beneficiary$Dead == TRUE] = 1
patient_beneficiary$Dead[patient_beneficiary$Dead == FALSE] = 0

patient_beneficiary_eval$DOB = as.Date(patient_beneficiary_eval$DOB)
patient_beneficiary_eval$Dead = is.na(patient_beneficiary_eval$DOD) == FALSE
patient_beneficiary_eval$DOD = patient_beneficiary_eval$DOD %>% replace_na('2010-01-01')
patient_beneficiary_eval$DOD = as.Date(patient_beneficiary_eval$DOD)
patient_beneficiary_eval$Age = as.numeric((patient_beneficiary_eval$DOD - patient_beneficiary_eval$DOB) / 365)
patient_beneficiary_eval$Dead[patient_beneficiary_eval$Dead == TRUE] = 1
patient_beneficiary_eval$Dead[patient_beneficiary_eval$Dead == FALSE] = 0

pb = patient_beneficiary
pbe = patient_beneficiary_eval

pb_merged = merge(pb, provider, by = "ProviderID")

write.csv(x = pb_merged, file = 'pb_merged.csv')

pbe_merged = merge(pbe, provider_eval, by = "ProviderID")

write.csv(x = pbe_merged, file = 'pbe_merged.csv')

#pb_merged was processed and aggregated using a combination of Excel and Python.
#The resulting dataframe is "aggregated_upsampled.csv".
aggregated = read.csv("aggregated_upsampled.csv")

library(leaps)
library(glmnet)

aggregated = subset(aggregated, select = -c(ProviderID))

#best subset regression
best_subset_regression = regsubsets(Fraud ~. , aggregated, nvmax = length(colnames(aggregated)))
best_subset_regression_summary = summary(best_subset_regression)
best_subset_regression_summary$rss
best_subset_regression_summary$which

plot(seq(1,length(colnames(aggregated)) - 1), best_subset_regression_summary$cp, type="l")
plot(seq(1,length(colnames(aggregated)) - 1), best_subset_regression_summary$bic, type="l")
plot(seq(1,length(colnames(aggregated)) - 1), best_subset_regression_summary$adjr2, type="l")

best_subset_regression_summary$cp
best_subset_regression_summary$bic
best_subset_regression_summary$adjr2

best_14 = c('DeductibleAmtPaid', 'NoOfMonths_PartACov', 'NoOfMonths_PartBCov', 'IPAnnualReimbursementAmt', 'IPAnnualDeductibleAmt', 'OPAnnualReimbursementAmt', 'ClaimProcessTime', 'ChronicCond_Heartfailure_1', 'ChronicCond_KidneyDisease_1', 'ChronicCond_Cancer_1', 'ChronicCond_Depression_1', 'RenalDiseaseIndicator_1', 'Race_3', 'Counts')
aggregated_best_subset = subset(aggregated, select = c(best_14, 'Fraud'))

library(boot)

k = 10
glm_fit = glm(Fraud ~., aggregated_best_subset, family = "binomial")
cv_error_k = cv.glm(aggregated_best_subset, glm_fit, K = k)$delta[1]

accuracy = 1 - cv_error_k

