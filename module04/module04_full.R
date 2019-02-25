## ------------- Set Directory -------------

# load libraries
options(gsubfn.engine = "R")
library(sqldf)

## ------------- Read Files -------------
setwd("/Users/davidbuckeridge/GitHub/pphs616")
# Read in data
# (Do not use '.' in the variable name or it will be hard to work with sqldf)
hospital_discharges = read.csv('data/hospital_discharges.csv')
physician_services = read.csv('data/physician_services.csv')
sampled_patients = read.csv('data/sampled_patients.csv')

hospital_discharges$admit = as.Date(hospital_discharges$admit)
hospital_discharges$discharge = as.Date(hospital_discharges$discharge)
physician_services$date = as.Date(physician_services$date)


# Popular algorithm (Hux et al, 2002):
# * Two physician diabetes diagnosis codes separated by 730 days or less OR 
# * One hospitalization diabetes diagnosis code.
# Sensitivity: 86% 
# Specificity: 97%

# Codes:
#  ICD-9:250.*
#  ICD-10: E10*-E14*

# Unfortunately the SQLite backend doesn't support regex operators.
# Using the admission date only because I have to pick one. Discharge would also work.
hospital_diag = 
  sqldf("SELECT anon_id, admit as diab_date 
         FROM hospital_discharges 
         WHERE (icd_type='ICD-9'  AND icd LIKE '250%')
            OR (icd_type='ICD-10' AND (icd LIKE 'E10%' OR
                                       icd LIKE 'E11%' OR
                                       icd LIKE 'E12%' OR
                                       icd LIKE 'E13%' OR
                                       icd LIKE 'E14%'))")

phys_diab <- sqldf("SELECT anon_id, date 
                   FROM physician_services 
                   WHERE icd LIKE '250%'")
# Self-join to get the 730 days of difference bit.
phys_diag <- sqldf("SELECT x.anon_id, min(x.date) as diab_date
                    FROM phys_diab x JOIN phys_diab y 
                    ON x.anon_id=y.anon_id AND 
                       x.date > y.date AND (x.date - y.date <=730)
                    GROUP BY x.anon_id")     

phys_diag_time <- sqldf("SELECT x.anon_id, x.date as d1, y.date as d2
                    FROM phys_diab x JOIN phys_diab y 
                   ON x.anon_id=y.anon_id AND 
                   x.date > y.date AND (x.date - y.date <=730)")



# Stick them both together and find the minimum date.
both_diag <- sqldf("SELECT anon_id, diab_date FROM phys_diag 
                   UNION
                   SELECT anon_id, diab_date FROM hospital_diag")

# Take the minimum date
diab_dates <- sqldf("SELECT anon_id, min(diab_date) as diab_date 
                    FROM both_diag GROUP BY anon_id")

nrow(diab_dates) / nrow(sampled_patients) # 5.4% Prevalence

# So the prevalence in this cohort is 5.4%. Typical finidngs for Canada are 6.8%
# Age and Sex based prevalence would require a JOIN to the sampled_patients table..



