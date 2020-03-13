## ------------- Module COVID-19 -------------

## ------------- Load Libraries ----------------------
library(dplyr)
library(RColorBrewer)
library(coarseDataTools)
library(tidyverse)
library(ggplot2)
library(maps)


## ------------- Load Data ---------------------------
# Read data files as downloaded from GitHub
# https://github.com/CSSEGISandData/COVID-19
# As described in Dong, E., Du, H., & Gardner, L. (2020). 
#  An interactive web-based dashboard to track COVID-19 in 
#  real time. The Lancet. Infectious Diseases.
#  http://doi.org/10.1016/S1473-3099(20)30120-1
source("./load.covid.data.R")


# Remove unused details, and aggregate states by country
df <- jhu.data %>%
  select(-state) %>%
  group_by(country, date) %>%
  summarise(
    case = sum(case, na.rm = T),
    death = sum(death, na.rm = T),
    recovered = sum(recovered, na.rm = T),
    lat = mean(lat), long = mean(long) # Centroid? this feel wrong
  ) %>% ungroup()


# TODO: Calculate incident events from cumulative series, then add the incident series to dataset
df <- df %>%
  group_by(country) %>%
  mutate(
    inc.case = case - lag(case),
    inc.death = death - lag(death),
    inc.recovered = recovered - lag(recovered),
    cCFR = ifelse(death > 10, death / case, NA)
  ) %>%
  ungroup()


# TODO: Calculate metadata per countries (date of first event, and totals)
df <- df %>% group_by(country)
df.meta <- df %>%
  summarise(cases = last(case), deaths = last(death), recoveries = last(recovered)) %>%
  inner_join(df %>% filter(case >= 1) %>% summarise(index.case.date = min(date))) %>%
  left_join(df %>% filter(death >= 1) %>% summarise(index.death.date = min(date))) %>%
  left_join(df %>% filter(recovered >= 1) %>% summarise(index.recovery.date = min(date)))
df <- df %>% ungroup()



# TODO: Merge with WHO data to obtain WHO regions (WHO estimates are <= then JHU)
# NOTE(malavv): WHO country names do not match at all with JHU, and there are much fewer country.

## ------------------ Plot time-series --------------------------
ggplot(df %>% group_by(date) %>% summarise_if(is.numeric, sum, na.rm=T), aes(x=date)) +
  geom_line(aes(y=case, color="cases")) +
  geom_line(aes(y=death, color="deaths")) +
  geom_line(aes(y=recovered, color="recoveries")) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  labs(title = "Timeseries of COVID 2019, Globally", x = "Week (Month/Day)", y = "Number of confirmed and suspected cases")



## ----------------- Plot distribution by geography -------------
# TODO: plot epi curve (i.e., incident cases - hint, difference of cumulative cases will give you this...) by WHO region
top10.countries <- head(df.meta %>% arrange(desc(deaths)), n = 8)$country
# Removing first day because incident case makes no sense when no past exist.
ggplot(df %>% filter(country %in% top10.countries & date != "2020-01-22"), aes(x=date)) +
  geom_line(aes(y=cCFR, color=country))+
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  scale_y_continuous(limits = c(0, 0.125)) +
  labs(title = "Timeseries of COVID 2019 crude CFR, Countries with Top 10 Case load",
       x = "Week (Month/Day)", y = "Crude Case Fatality Risk")




# TODO: extra! - create global maps (bubble) of total cases by country
countries.location <- df %>% select(country, lat, long) %>% unique()
ggplot(df.meta %>% inner_join(countries.location)) +
  borders("world", colour="gray50", fill="gray50") +
  geom_point(aes(x=long, y=lat, size=cases), color="red") +
  scale_size(breaks = seq(1, 5000, by=1000), range = c(0, 10)) +
  labs(x = "", y = "", title = "Number of COVID 2019 cases by countries")

# Now Layer the cities on top

# how do epi curves compare to your expecation based on incubation period and Ro

## ------------------ Estimate Case Fatility Risk ---------------
# estimate case fatility risk by country using package by Reich and see article by Lipsitch

# create data set for input to EMforCFR function with the following columns:
# grp a 1 or a 2 indicating which of the two groups, j, the observation is for.
# new.times an integer value representing the time, t, of observation.
# R the count of recovered cases with onset at time t in group j.
# D the count of deaths which occurred at time t in groupo j (note that these deaths did not have disease onset at time t but rather died at time t).
# N the total cases at t, j, or the sum of R and D columns.


# start with second day

# for all days
  # subtract value for previous day from value for today to obtain new events today 

# NOTE(malavv) CFR in this function seem to want to compare two groups. Haven't read the paper yet. I'll make it work
#              for China vs South Korea
full.data <- df %>%
  filter(country %in% c("Mainland China", "Republic of Korea")) %>%
  filter(date != "2020-01-22" & date <= "2020-03-07") %>% # Removing first date, because incidence is NA
  arrange(date) %>% group_by(country) %>%
  mutate(
    time = row_number(),
    new.times = time,
    grp = ifelse(country == "Mainland China", 1, 2)
  ) %>% ungroup() %>%
  rename(
    R = inc.recovered,
    D = inc.death,
    N = case
  ) %>% select(grp, time, new.times, N, D, R)

# Creating an assumed.nu survival probability vector
# from "Updated understanding of the outbreak of 2019 novel coronavirus (2019‐nCoV) in Wuhan, China"
# https://onlinelibrary.wiley.com/doi/full/10.1002/jmv.25689?af=R
times2death <- c(20, 16, 10, 6, 10, 14, 41, 12, 30, 7, 19, 13, 11, 20, 19, 8, 16)
nb <- MASS::fitdistr(times2death,"negative binomial") # Fit observed data to a neg. bin.
dens <- dnbinom(unique(full.data$time), nb$estimate["size"], mu = nb$estimate["mu"]) # Get density based on neg. binom.
dens <- dens / sum(dens) # Normalize vector because we use finite range

# Running the thing
cfr.ests <- EMforCFR(
  assumed.nu = dens,
  alpha.start.values = rep(0, 45 - 1),
  full.data = as.data.frame(a),
  verb = T,
  SEM.var = TRUE,
  max.iter = 500,
  tol = 1e-05)

## ------------------ Is COVID-19 Controllable ------------------
# estimate and plot parameters for COVID-19 on Figure in article from Fraser


## ------------------ Something else! ---------------------------


## ------------------ Functions ---------------------
# Function to aggregate case counts across regions in a country, if necessary
aggregateJHUCounts <- function(x) {
  if (nrow(x) > 1) {
    # if multiple rows for a country, aggregate case counts to form single row
    counts <- rowsum(x[,5:ncol(x)], group=rep(1, nrow(x)))
  } else {
    # otherwise use single set of counts
    counts <- x[,5:ncol(x)]
  }   
  return(counts)
} # aggregateJHUCounts


# Function to be used in lapply to extract maximum case count (should be last value..)
# We create the index knowing first entry is country name and second entry is list, containing
#  country name (to ensure integrity) in first slot, and vectors of integer counts for cases,
#  deaths, and recovered in subsequent slots in that order.
current.count <- function(x, count.type) {
  valid.types <- c("cases", "deaths", "recovered")
  index.type <- which(valid.types == count.type)
  
  return(max(x[[(index.type + 1)]]))
}

# Function to return the day of the first event of the type requested
#  (as 1-indexed integer days from jhu.start)
first.event <- function(x, event.type) {
  valid.types <- c("cases", "deaths", "recovered")
  index.type <- which(valid.types == event.type)
  
  index.events <- which(x[[index.type + 1]] > 0)
  
  return(ifelse(length(index.events) > 0, min(index.events), NA))
}

