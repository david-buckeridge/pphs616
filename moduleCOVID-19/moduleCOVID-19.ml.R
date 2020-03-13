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
    inc.recovered = recovered - lag(recovered)
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
top10.countries <- head(df.meta %>% arrange(desc(cases)), n = 10)$country
# Removing first day because incident case makes no sense when no past exist.
ggplot(df %>% filter(country %in% top10.countries & date != "2020-01-22"), aes(x=date)) +
  geom_line(aes(y=inc.case, color=country))+
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  labs(title = "Timeseries of COVID 2019 incidence, Countries with Top 10 Case load",
       x = "Week (Month/Day)", y = "Number of incidence cases")


# TODO: extra! - create global maps (bubble) of total cases by country
countries.location <- df %>% select(country, lat, long) %>% unique()
ggplot(df.meta %>% inner_join(countries.location)) +
  borders("world", colour="gray50", fill="gray50") +
  geom_point(aes(x=long, y=lat, size=cases), color="red") +
  scale_size(breaks = seq(1, 150000, by=10000), range = c(0, 10)) +
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

## ------------------ Is COVID-19 Controllable ------------------
# estimate and plot parameters for COVID-19 on Figure in article from Fraser


## ------------------ Something else! ---------------------------

## ------------------ Aligned case series -----------------------
num.cases.for.date.align <- 50

df <- df %>%
  # Removed China for readability, and Cruise Ship for relevance
  filter(case > num.cases.for.date.align & !(country %in% c("China", "Cruise Ship"))) %>%
  arrange(date) %>%
  group_by(country) %>%
  mutate(time = row_number())

ggplot(df, aes(x=time)) +
  geom_line(aes(y=case, color=country)) +
  labs(title = sprintf("Country's case evolution starting at %d cases", num.cases.for.date.align),
       x = "Days since threshold", y = "Number of cases")