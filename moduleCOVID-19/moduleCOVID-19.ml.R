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

# TODO: Calculate incident events from cumulative series, then add the incident series to dataset
df <- jhu.country.data %>%
  group_by(country) %>%
  mutate(
    inc.case = case - lag(case),
    inc.death = death - lag(death),
    inc.recovered = recovered - lag(recovered),
    cCFR = ifelse(death > 10, death / case, NA)
  ) %>%
  ungroup()


# TODO: Calculate metadata per countries (date of first event, and totals)
df.meta <- jhu.country.data %>% group_by(country) %>%
  summarise(cases = last(case), deaths = last(death), recoveries = last(recovered), cCFR = last(death) / last(case)) %>%
  inner_join(jhu.country.data %>% group_by(country) %>% filter(case >= 1) %>% summarise(index.case.date = min(date))) %>%
  left_join(jhu.country.data %>% group_by(country) %>% filter(death >= 1) %>% summarise(index.death.date = min(date))) %>%
  left_join(jhu.country.data %>% group_by(country) %>% filter(recovered >= 1) %>% summarise(index.recovery.date = min(date)))


## ------------------ Plot time-series --------------------------
ggplot(df %>% group_by(date) %>% summarise_if(is.numeric, sum, na.rm=T), aes(x=date)) +
  geom_line(aes(y=case, color="cases")) +
  geom_line(aes(y=death, color="deaths")) +
  geom_line(aes(y=recovered, color="recoveries")) +
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  labs(title = "Timeseries of COVID 2019, Globally", x = "Week (Month/Day)", y = "Number of confirmed and suspected cases")



## ----------------- Plot distribution by geography -------------

top10.countries <- head(df.meta %>% arrange(desc(deaths)), n = 8)$country
# Removing first day because incident case makes no sense when no past exist.
ggplot(df %>% filter(country %in% top10.countries & date != "2020-01-22"), aes(x=date)) +
  geom_line(aes(y=cCFR, color=country))+
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  scale_y_continuous(limits = c(0, 0.125)) +
  labs(title = "Timeseries of COVID 2019 crude CFR, Countries with More than 10 Deaths",
       x = "Week (Month/Day)", y = "Crude Case Fatality Risk")


# Plot incident cases by WHO region
df.who <- df %>%
  group_by(who.region, date) %>%
  filter(date != "2020-01-22") %>%
  summarise(inc.case = sum(inc.case, na.rm = T)) %>%
  ungroup()
ggplot(df.who, aes(x=date)) +
  geom_line(aes(y=inc.case, color=who.region))+
  scale_x_date(date_breaks = "1 week", date_labels = "%m-%d") +
  labs(title = "Timeseries of COVID 2019 incidence, WHO regions",
       x = "Week (Month/Day)", y = "Number of incidence cases")


# Create global map (bubble) of total cases by country
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

## ------------------ Is COVID-19 Controllable ------------------
# estimate and plot parameters for COVID-19 on Figure in article from Fraser


## ------------------ Something else! ---------------------------

## ------------------ Aligned case series -----------------------
num.cases.for.date.align <- 50

df.evo <- df %>%
  # Removed China for readability, and Cruise Ship for relevance
  filter(case > num.cases.for.date.align & !(country %in% c("China", "Cruise Ship"))) %>%
  arrange(date) %>%
  group_by(country) %>%
  mutate(time = row_number()) %>%
  ungroup()

ggplot(df.evo, aes(x=time)) +
  geom_line(aes(y=case, color=country)) +
  labs(title = sprintf("Country's case evolution starting at %d cases", num.cases.for.date.align),
       x = "Days since threshold", y = "Number of cases")

## ---------------- Daily naive CFR --------------------------
daily.cfr <- df %>% mutate(cfr = death / case) %>%
  group_by(country) %>%
  filter(sum(case) > 1000) %>% # Limit output to large Ns
  ungroup()

ggplot(daily.cfr %>% mutate(cfr = death / case) %>% filter(sum(case) > 1000), aes(x=date)) +
  geom_line(aes(y=cfr, color=country), size=1) +
  scale_x_date(limits = c(ymd("2020-02-20", today() - 1)), date_breaks = "3 day", date_labels = "%m-%d") +
  scale_y_continuous(limits = c(0, 0.08)) + labs(title = "Case-Fatality Risk per Country", x="Date", y="Case-Fatality Risk")