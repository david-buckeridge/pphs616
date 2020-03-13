library(coarseDataTools)
library(tidyverse)

## ------------- Load Data ---------------------------
# Read data files as downloaded from GitHub
# https://github.com/CSSEGISandData/COVID-19
# As described in Dong, E., Du, H., & Gardner, L. (2020).
#  An interactive web-based dashboard to track COVID-19 in
#  real time. The Lancet. Infectious Diseases.
#  http://doi.org/10.1016/S1473-3099(20)30120-1
source("./load.covid.data.R")

ref.country <- "Korea, South"
cmp.country <- "Italy"

## ----------- Naïve Estimators ---------------
# Naïve CFR Estimator
# Naïve estimator for referent country.
naive.ref.cfr <- jhu.data %>% filter(country == ref.country) %>% top_n(1, date) %>% mutate(naive.cfr = death / case) %>% .$naive.cfr
# Naïve estimator for comparator country.
naive.cmp.cfr <- jhu.data %>% filter(country == cmp.country) %>% top_n(1, date) %>% mutate(naive.cfr = death / case) %>% .$naive.cfr

# Naïve relative CFR
naive.rel.cfr <- naive.cmp.cfr / naive.ref.cfr

## ----------- Adjusted Estimators ---------------

# First, we need a probability density for the time from symptom unset to death.
# from "Updated understanding of the outbreak of 2019 novel coronavirus (2019‐nCoV) in Wuhan, China"
# https://onlinelibrary.wiley.com/doi/full/10.1002/jmv.25689?af=R
times2death <- c(20, 16, 10, 6, 10, 14, 41, 12, 30, 7, 19, 13, 11, 20, 19, 8, 16) # Of individual patient in the case study
nb <- MASS::fitdistr(times2death,"negative binomial") # Fit observed data to a neg. bin.
dens <- dnbinom(seq_along(unique(jhu.data$date)), nb$estimate["size"], mu = nb$estimate["mu"]) # Get density based on neg. binom.
dens <- dens[1:30] / sum(dens[1:30]) # Normalize vector because we use finite range (bound to 30 because error in library code)

# Second, use this data to compute relative CFR.

# Preping data
df <- jhu.data %>%
  select(-c(state, lat, long)) %>% # Removing add. details
  group_by(country, date) %>% # Desired End stratification
  summarise( # Aggregate data for each sub-country region
    case = sum(case, na.rm = T),
    death = sum(death, na.rm = T),
    recovered = sum(recovered, na.rm = T)
  ) %>%
  ungroup() %>%
  group_by(country) %>%
  mutate( # Compute # of incident case/death/recovered by diff. with last day
    inc.case = case - lag(case),
    inc.death = death - lag(death),
    inc.recovered = recovered - lag(recovered)
  ) %>%
  mutate_at(vars(matches("inc.")), function(numbers) sapply(numbers, max, 0)) %>% # Force incidences to be [0, )
  ungroup()

# Creating full data (per the docs), using 2 as comparator, 1 as reference.
min.cases <- 5 # Defining a section of outbreak to analyse (Days with more than this many incident cases)
full.data <- df %>%
  filter(country %in% c(cmp.country, ref.country)) %>%
  arrange(date) %>%
  group_by(country) %>%
  mutate(
    time = row_number(),
    grp = ifelse(country == ref.country, 1, 2)
  ) %>% ungroup() %>%
  rename(
    R = inc.recovered,
    D = inc.death,
    N = inc.case
  ) %>% select(grp, time, N, D, R)

# Add the counts of day in "outbreak" to analyse
outbreak.start.time <- full.data %>%
  select(-c(R, D)) %>%
  spread(key = grp, value = N, sep = '.') %>%
  filter(grp.1 > min.cases & grp.2 > min.cases) %>%
  top_n(1, desc(time)) %>% .$time

# Adding day count during outbreak
full.data <- full.data %>%
  mutate(new.times = ifelse(time < outbreak.start.time, NA, time - outbreak.start.time + 1))

# Running the thing
cfr.ests <- EMforCFR(
  assumed.nu = dens,
  alpha.start.values = rep(0, length(unique(full.data$new.times)) - 2),
  full.data = as.data.frame(full.data),
  verb = T,
  SEM.var = TRUE,
  max.iter = 500,
  tol = 1e-05
)




