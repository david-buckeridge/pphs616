## ------------- Set Directory -------------

# load libraries
options(gsubfn.engine = "R")
library(sqldf)


## ------------- Read Files -------------
setwd("/Users/davidbuckeridge/GitHub/pphs616")
hospital_discharges = read.csv('data/hospital_discharges.csv')
physician_services = read.csv('data/physician_services.csv')
sampled_patients = read.csv('data/sampled_patients.csv')

hospital_discharges$admit = as.Date(hospital_discharges$admit)
hospital_discharges$discharge = as.Date(hospital_discharges$discharge)
physician_services$date = as.Date(physician_services$date)
sampled_patients$dob = as.Date(paste(as.character(sampled_patients$dob), "-01", sep=""), format="%Y-%m-%d")


# Popular algorithm (Hux et al, 2002):
# * Two physician diabetes diagnosis codes separated by 730 days or less OR 
# * One hospitalization diabetes diagnosis code.
# Sensitivity: 86% 
# Specificity: 97%

# Codes:
#  ICD-9:250.*
#  ICD-10: E10*-E14*


# Step 1 - Identify people with at least one hospital admission for diabetes
#        - We take the date of first admission only (i.e., "min(admit)"), so there will be only one row for each person
#        - When an aggregtion operator is used in the SELECT clause, there must be a GROUP BY 
#        - Ranges of ICD9 and ICD10 codes are given to account for the change in coding
hospital_diag = 
  sqldf("SELECT anon_id, min(admit) AS diab_date 
         FROM hospital_discharges 
         WHERE (icd_type='ICD-9'  AND icd LIKE '250%')
            OR (icd_type='ICD-10' AND (icd LIKE 'E10%' OR
                                       icd LIKE 'E11%' OR
                                       icd LIKE 'E12%' OR
                                       icd LIKE 'E13%' OR
                                       icd LIKE 'E14%'))
         GROUP BY anon_id")


#        - Identify all hospitalization events for diabetes
#        - We take all admissions, so there will be multiple rows for a person if they had multiple admissions
hospital_diag_events = 
  sqldf("SELECT anon_id, admit AS diab_date 
        FROM hospital_discharges 
        WHERE (icd_type='ICD-9'  AND icd LIKE '250%')
        OR (icd_type='ICD-10' AND (icd LIKE 'E10%' OR
        icd LIKE 'E11%' OR
        icd LIKE 'E12%' OR
        icd LIKE 'E13%' OR
        icd LIKE 'E14%'))")


# Question 1a - What proportion of all subjects had at least one admission for diabetes (R)?


# Question 1b - Plot the frequency distribution of the number of hospitalizations per person for diabetes (SQL + R). 
# Hint: This is easy if you perform another SQL query very similar to the one for hospital_diag...


# Question 1c - What proportion of all hospital admissions were for diabetes (R)?


# Question 1d - When was ICD-10 first used for coding hospital discharges (SQL)?



# Question 1e - Compare the proportion of hospital admissions that were for diabetes before and after the switch to ICD-10 (SQL + R).
# Rates before...
hospital_diag_preICD10 = 
  sqldf("SELECT anon_id, discharge
          FROM hospital_discharges
          WHERE icd_type='ICD-9' 
            AND icd LIKE '250%'
        ")

hospital_discharge_count_preICD10 =
  sqldf("SELECT COUNT(*)
          FROM hospital_discharges
          WHERE icd_type='ICD-9' 
        ")[1,1]  

nrow(hospital_diag_preICD10) / hospital_discharge_count_preICD10


# Rates after...




# Step 2 - Identify physician billing events for diabetes
phys_diab = 
  sqldf("SELECT anon_id, date 
         FROM physician_services 
         WHERE icd LIKE '250%'")

# Physician consulation rate
phys_diab_unique = 
  sqldf("SELECT DISTINCT anon_id
         FROM physician_services
         WHERE icd LIKE '250%'")

# Question 2a - What proportion of all subjects had at least one physician visit for diabetes (R)?


# Question 2b - Plot the frequency distribution of physician visits per person for diabetes (SQL + R).



# Question 2c - What proportion of all physician visits were for diabetes (R)?



# Step 3 - Identify people with two or more billings for diabetes within 730 days
phys_diag = 
  sqldf("SELECT x.anon_id, (x.date - y.date) as interval, min(x.date) as diab_date
         FROM phys_diab x JOIN phys_diab y 
         ON x.anon_id=y.anon_id 
          AND x.date > y.date 
          AND (x.date - y.date <=730)
         GROUP BY x.anon_id") 

# Question 3a - What proportion of subjects had two visits for diabetes within 730 days?


# Question 3b - Plot the proportion of subjects that would be identified in 3a as a function of the cutoff ranging from 0 to 730 days. 
#               Do you think 730 days is a reasonable cut-off? Explain.



# Step 4 - Join cases detected through physician billing with those detected from hospital discharges.
both_diag =
  sqldf("SELECT anon_id, diab_date 
         FROM phys_diag 
          UNION
         SELECT anon_id, diab_date 
         FROM hospital_diag")
                   
diab_dates = 
  sqldf("SELECT anon_id, min(diab_date) as diab_date 
         FROM both_diag 
         GROUP BY anon_id")



# Question 4a - What proportion of subjects met the case definition for diabetes of either one hospital admission
#               or two physician visits within 730 days (R)?




# Question 4b - Create a Venn diagram (the areas does not need to perfectly reflect the proportions) to 
#               illustrate the following: total number of patients, number that are cases due to physician billing, hospital discharges, or both (R). 

#library(VennDiagram)



# Question 4c - Calculate the proportion of subjects that meet the cased definition stratified by age
#               (44 and under, 45 to 64, 65 and over) and sex (male, female) stratified rates. (Hint - you will need to join to the patients table) (SQL + R).





