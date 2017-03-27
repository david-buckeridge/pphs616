## ------------- Set Directory -------------

# load libraries
options(gsubfn.engine = "R")
library(sqldf)


## ------------- Read Files -------------
setwd("/Users/david/GitHub/pphs616")
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


# Question 1a - What proprtion of all subjects had at least one admission for diabetes (R)?
nrow(hospital_diag) / nrow(sampled_patients) # people

# Question 1b - Plot the frequency distribution of the number of hospitalizations per person for diabetes (SQL + R). 
# Hint: This is easy if you perform another SQL query very similar to the one for hospital_diag...
hospital_diag_count = 
  sqldf("SELECT anon_id, count(admit) AS diab_count 
        FROM hospital_discharges 
        WHERE (icd_type='ICD-9'  AND icd LIKE '250%')
        OR (icd_type='ICD-10' AND (icd LIKE 'E10%' OR
        icd LIKE 'E11%' OR
        icd LIKE 'E12%' OR
        icd LIKE 'E13%' OR
        icd LIKE 'E14%'))
        GROUP BY anon_id")

hist(hospital_diag_count$diab_count)


# Question 1c - What proportion of all hospital admissions were for diabetes (R)?
nrow(hospital_diag_events) / nrow(hospital_discharges) # events

# Question 1d - When was ICD-10 first used for coding hospital discharges?

first.admit = as.Date(
  sqldf("SELECT min(admit)
         FROM hospital_discharges
         WHERE (icd_type='ICD-10' AND (icd LIKE 'E10%' OR
                                       icd LIKE 'E11%' OR
                                       icd LIKE 'E12%' OR
                                       icd LIKE 'E13%' OR
                                       icd LIKE 'E14%'))")[1,1],
              origin="1970-01-01")

first.discharge = as.Date(
  sqldf("SELECT min(discharge)
         FROM hospital_discharges
         WHERE (icd_type='ICD-10' AND (icd LIKE 'E10%' OR
                                       icd LIKE 'E11%' OR
                                       icd LIKE 'E12%' OR
                                       icd LIKE 'E13%' OR
                                       icd LIKE 'E14%'))")[1,1],  
              origin="1970-01-01")



# Compare rates before and after ICD-10
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
hospital_diag_postICD10 = 
  sqldf("SELECT anon_id, discharge
        FROM hospital_discharges
        WHERE icd_type='ICD-10' 
        AND (icd LIKE 'E10%' OR
             icd LIKE 'E11%' OR
             icd LIKE 'E12%' OR
             icd LIKE 'E13%' OR
             icd LIKE 'E14%')
        ")

hospital_discharge_count_postICD10 =
  sqldf("SELECT COUNT(*)
        FROM hospital_discharges
        WHERE icd_type='ICD-10' 
        ")[1,1]  
                            
nrow(hospital_diag_postICD10) / hospital_discharge_count_postICD10



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

# period prevalence
nrow(phys_diab_unique) / nrow(sampled_patients)

# event rate
nrow(phys_diab) / nrow(physician_services)



# Step 3 - Identify people with two or more billings for diabetes within 730 days
phys_diag = 
  sqldf("SELECT x.anon_id, min(x.date) as diab_date
         FROM phys_diab x JOIN phys_diab y 
         ON x.anon_id=y.anon_id 
          AND x.date > y.date 
          AND (x.date - y.date <=730)
         GROUP BY x.anon_id") 

nrow(phys_diag) / nrow(sampled_patients)



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


nrow(diab_dates) / nrow(sampled_patients)

# determine rates by age...
diab_dates_age =
  sqldf("SELECT x.anon_id, diab_date, dob, sex
         FROM diab_dates x, sampled_patients y
         WHERE x.anon_id = y.anon_id")




