## 1. Load Data
data.url = "https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv"
gapminder = read.csv(data.url)

### ---- Functions -----

countries = unique(gapminder$country)
increase = data.frame(country = countries, years = rep(NA, length(countries)), year = rep(NA, length(countries)))

for (country in countries) {
  
  years = gapminder$year[gapminder$country == country]
  le = gapminder$lifeExp[gapminder$country == country]
  
  largest = largest.increase(years, le)
  
  increase$years[increase$country == country] = largest$years
  increase$year[increase$country == country] = largest$year
  
} # for - country



## 2. Find the year with the greatest increase in LE for a given country
# Purpose: Take 
# Inputs:
# Internal variables and algorithm
# Return Value: