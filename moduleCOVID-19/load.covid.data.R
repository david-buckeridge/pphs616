library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

# Novel Coronavirus (COVID-19) Cases, provided by JHU CSSE
JHU.CSSE.COVID19.github.raw <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/"
# All timeseries loaded uses a *wide* format. Where a few initial columns are constant, and each day is added
# as a column to the end of the data. This means the number of columns of this data is variable (ie. *not* constant).

# Yesterday's date is used to limit data. Data on the same day, or the previous day is highly unstable.
yesterday <- today() - 1

# WHO report
who.cases <- read_csv(
  paste0(JHU.CSSE.COVID19.github.raw, "who_covid_19_situation_reports/who_covid_19_sit_rep_time_series/who_covid_19_sit_rep_time_series.csv"),
  col_types = cols(.default = col_integer(), `Province/States` = col_factor(), `Country/Region` = col_factor(), `WHO region` = col_factor())) %>%
  rename(state = `Province/States`, country = `Country/Region`, who.region = `WHO region`) %>% # easier to work with
  gather(date, count, -c(state, country, who.region)) %>% # Wide to Long
  mutate(date = mdy(date)) %>% # Parsing Month/Day/Year as dates
<<<<<<< HEAD
  filter(date <= ymd("2020-03-12")) # Today will have incomplete date
=======
  filter(date < yesterday)
>>>>>>> origin/master

# CSSE's time series (note. be careful with Lat & Long arithmetic, since they're double, might have prec. issue)
cols_cfg <- cols(.default = col_integer(), `Province/State` = col_factor(), `Country/Region` = col_factor(), Lat = col_double(), Long = col_double())
jhu.data <- read_csv(paste0(JHU.CSSE.COVID19.github.raw, "csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"), col_types = cols_cfg) %>% gather(date, case, -(1:4)) %>%
  inner_join(read_csv(paste0(JHU.CSSE.COVID19.github.raw, "csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"), col_types = cols_cfg) %>% gather(date, death, -(1:4))) %>%
  inner_join(read_csv(paste0(JHU.CSSE.COVID19.github.raw, "csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"), col_types = cols_cfg) %>% gather(date, recovered, -(1:4))) %>%
  rename(state = `Province/State`, country = `Country/Region`, lat = Lat, long = Long) %>% # easier to work with
  mutate(date = mdy(date)) %>% # Parsing Month/Day/Year as dates
<<<<<<< HEAD
  filter(date <= ymd("2020-03-12")) # Today will have incomplete date
=======
  filter(date < yesterday)

# Create a simplified dataframe, aggregated by country
jhu.country.data <- jhu.data %>%
  select(-state) %>%
  group_by(country, date) %>%
  summarise(
    case = sum(case, na.rm = T),
    death = sum(death, na.rm = T),
    recovered = sum(recovered, na.rm = T),
    lat = mean(lat), long = mean(long) # Centroid? this feel wrong
  ) %>%
  ungroup()

# TODO: Merge with WHO data to obtain WHO regions (WHO estimates are <= then JHU)
country2who.region <- read_csv('country2who.region.csv', col_types = 'cc')
jhu.country.data <- jhu.country.data %>% inner_join(country2who.region, by="country")
>>>>>>> origin/master

# Global variables
jhu.dates <- sort(unique(jhu.data$date))
jhu.countries <- levels(jhu.data$country)
jhu.states <- levels(jhu.data$state)

# -- Notes --
# Confirmed cases include presumptive positive cases, so their numbers go down without death or recovered going up

# -- Suspicious data --
# * Japan (2020-01-22, 23, 24) cases jump from 2 -> 1 -> 2, with no indicator in death or recovered.
# * Japan (2020-02-05, 06, 07) cases jump from 22 -> 45 -> 25, with no indicator in death or recovered.
# * Saint Barthelemy (2020-03-08, 09, 10) cases drop from 3 -> 1 -> 1, with no indicator in death or recovered.
