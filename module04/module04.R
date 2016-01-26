## ------------- Set Directory -------------

# load libraries
library(sqldf)


## ------------- Read Files -------------
hospital_discharges <- read.csv('data/hospital_discharges.csv')
physician_services <- read.csv('data/physician_services.csv')
sampled_patients <- read.csv('data/sampled_patients.csv')


# Popular algorithm (Hux et al, 2002):
# * Two physician diabetes diagnosis codes separated by 730 days or less OR 
# * One hospitalization diabetes diagnosis code.
# Sensitivity: 86% 
# Specificity: 97%

# Codes:
#  ICD-9:250.*
#  ICD-10: E10*-E14*


# Step 1 - Identify people with hospital admission for diabetes
hospital_diag <- 
  sqldf("SELECT anon_id, admit as diab_date 
         FROM hospital_discharges 
         WHERE (icd_type='ICD-9'  AND icd LIKE '250%')
            OR (icd_type='ICD-10' AND (icd LIKE 'E10%' OR
                                       icd LIKE 'E11%' OR
                                       icd LIKE 'E12%' OR
                                       icd LIKE 'E13%' OR
                                       icd LIKE 'E14%'))")


# When was ICD-10 first used for coding hospital discharges?



# Step 2 - Identify people with physician billing for diabetes
# phys_diab = ?



# Step 3 - Identify people with two or more billings for diabetes within 730 days
phys_diag <- sqldf("SELECT x.anon_id, min(x.date) as diab_date
                    FROM phys_diab x JOIN phys_diab y 
                    ON x.anon_id=y.anon_id AND 
                       x.date > y.date AND (x.date - y.date <=730)
                    GROUP BY x.anon_id") 

# Determine and plot the distribution of time between billings for diabetes within individuals



# Step 4 - Join cases detected through physician billing with those detected from hospital discharges.
both_diag <- sqldf("SELECT anon_id, diab_date FROM phys_diag 
                   UNION
                   SELECT anon_id, diab_date FROM hospital_diag")
                   
diab_dates <- sqldf("SELECT anon_id, min(diab_date) as diab_date 
                    FROM both_diag GROUP BY anon_id")




# Draw a VENN diagram depicting how many patients were detected from physician billing, hospital discharge, or both
# Determine diabetes prevalence by sex (M, F) and age-group (0-65, 65+)
