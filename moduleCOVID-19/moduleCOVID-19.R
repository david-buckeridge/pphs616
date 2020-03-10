## ------------- Module COVID-19 -------------

## ------------- Load Libraries ----------------------
library(RColorBrewer)
library(coarseDataTools)

## ------------- Load Data ---------------------------

# Read data files as downloaded from GitHub
# https://github.com/CSSEGISandData/COVID-19
# As described in Dong, E., Du, H., & Gardner, L. (2020). 
#  An interactive web-based dashboard to track COVID-19 in 
#  real time. The Lancet. Infectious Diseases.
#  http://doi.org/10.1016/S1473-3099(20)30120-1

who.cases.urlfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/who_covid_19_situation_reports/who_covid_19_sit_rep_time_series/who_covid_19_sit_rep_time_series.csv"
WHO.cases = read.csv(who.cases.urlfile)

jhu.cases.urlfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
jhu.cases = read.csv(jhu.cases.urlfile)

jhu.deaths.urlfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
jhu.deaths = read.csv(jhu.deaths.urlfile)

jhu.recovered.urlfile = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"
jhu.recovered = read.csv(jhu.recovered.urlfile)


# These values are the same for files with counts of confirmed cases, deaths, and recovered
jhu.first.date.col = 5
jhu.last.date.col = ncol(jhu.cases)
jhu.start = as.Date("2020-01-22")
jhu.end = jhu.start + (ncol(jhu.cases) - jhu.first.date.col) # remove the col for geog and first date
jhu.days = as.integer(jhu.end - jhu.start)
jhu.dates = seq(jhu.start, jhu.end, by="day") # sequence of dates for plotting time series
jhu.country.region = unique(jhu.cases[,"Country.Region"])
jhu.n.country = length(jhu.country.region)


## ------------ Create time series by Country ------------------------------

# Create empty list indexed by country to hold cumalitve counts of cases, deaths,
#  and recovered for each country.
country.counts = vector(mode="list", length=jhu.n.country)
names(country.counts) = jhu.country.region


# extract from jhu.cases, vector of cases by day from 2020-1-22 to most recent update
# for country in country.region
for (country in jhu.country.region) {
  country.cases = aggregateJHUCounts(jhu.cases[jhu.cases$Country.Region == country,])
  country.deaths = aggregateJHUCounts(jhu.deaths[jhu.deaths$Country.Region == country,])
  country.recovered = aggregateJHUCounts(jhu.recovered[jhu.deaths$Country.Region == country,])
  
  # place daily counts in list, indexed by country
  country.counts[[country.region]] = list("country" = country, 
                                          "cases"= country.cases, 
                                          "deaths" = country.deaths,
                                          "recovered" = country.recovered)
} # for country


# TODO: create function for calculating incident events from cumulative series, then add the
#       incident series to country.counts as new indicators.


###### NOT WORKING
# Create long data structure for plotting time series 
# country, date, indicator, value
indicator.types = c("cases", "deaths", "recovered") # order must reflect order in country.counts objects
col.dim = 4 # the columns above
row.dim = jhu.n.country * jhu.days * length(indicator.types)
country.counts.long = data.frame(country=rep(jhu.country.region,length.out=row.dim), 
                                date=rep(jhu.dates,length.out=row.dim),
                                indicator=rep(indicator.types, length.out=row.dim), 
                                value=rep(NA,row.dim))

for (country.region in jhu.country.region) {
  country.object = country.counts[[country.region]]
  for (indicator in indicator.types) {
    index.type = which(indicator.types == indicator)
    country.series = country.object[[index.type+1]]
    
    country.counts.long[country.counts.long$country == country.region &
                        country.counts.long$indicator == indicator,4] = as.vector(country.series)
    
  } # indicator
} # country




## ------------ Extract information about each Country from time series ------

# Apply functions to extract summary metrics
country.totalCases = lapply(X=country.counts, FUN=current.count, "cases")
country.totalDeaths = lapply(X=country.counts, FUN=current.count, "deaths")
country.totalRecovered = lapply(X=country.counts, FUN=current.count, "recovered")

country.firstCase = lapply(X=country.counts, FUN=first.event, "cases")
country.firstDeath = lapply(X=country.counts, FUN=first.event, "deaths")

# Place summary metrics in a data frame indexed by country
country.indicators = data.frame(country=jhu.country.region, totalCases=rep(NA,jhu.n.country),
                                totalDeaths=rep(NA,jhu.n.country), totalRecovered=rep(NA,jhu.n.country),
                                firstCase=rep(NA,jhu.n.country), firstDeath=rep(NA, jhu.n.country))

for (country.region in jhu.country.region) {
  country.indicators$totalCases[country.indicators$country == country.region] = country.totalCases[[country.region]]
  country.indicators$totalDeaths[country.indicators$country == country.region] = country.totalDeaths[[country.region]]
  country.indicators$totalRecovered[country.indicators$country == country.region] = country.totalRecovered[[country.region]]
  country.indicators$firstCase[country.indicators$country == country.region] = country.firstCase[[country.region]]
  country.indicators$firstDeath[country.indicators$country == country.region] = country.firstDeath[[country.region]]
}

# Create long data structure for visualizing indicator data






## ------------------ Plot time-series --------------------------

# create plotting-friendly dataset in long format with var country, date, count
country.cases.long = data.frame(country=NULL, date=NULL, count=NULL)

# merge with WHO data and plot cumulative time series by WHO regions

## ----------------- Plot distribution by geography -------------

# plot epi curve (i.e., incident cases - hint, difference of cumulative cases will give you this...) by WHO region
# extra! - create global maps (bubble) of total cases by country 
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


## ------------------ Functions ---------------------
# Function to aggregate case counts across regions in a country, if necessary
aggregateJHUCounts <- function(x) {
  if (nrow(x) > 1) {
    # if multiple rows for a country, aggregate case counts to form single row
    counts = rowsum(x[,5:ncol(x)], group=rep(1, nrow(x)))
  } else {
    # otherwise use single set of counts
    counts = x[,5:ncol(x)]
  }   
  return(counts)
} # aggregateJHUCounts


# Function to be used in lapply to extract maximum case count (should be last value..)
# We create the index knowing first entry is country name and second entry is list, containing
#  country name (to ensure integrity) in first slot, and vectors of integer counts for cases,
#  deaths, and recovered in subsequent slots in that order.
current.count <- function(x, count.type) {
  valid.types = c("cases","deaths","recovered")
  index.type = which(valid.types == count.type)
  
  return(max(x[[(index.type + 1)]]))
}

# Function to return the day of the first event of the type requested
#  (as 1-indexed integer days from jhu.start)
first.event <- function(x, event.type) {
  valid.types = c("cases","deaths","recovered")
  index.type = which(valid.types == event.type)
  
  index.events = which(x[[index.type + 1]] > 0)
  
  return(ifelse(length(index.events) > 0, min(index.events), NA))
}

