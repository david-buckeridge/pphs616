## 1. Load Data
data.url = "https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv"
gapminder = read.csv(data.url)

### ---- Functions -----
# Obtain list of countries
countries = unique(gapminder$country)
# Create data frame to hold results
increase = data.frame(country = countries, years = rep(NA, length(countries)), year = rep(NA, length(countries)))

for (country in countries) {
  # Extract from data frame years and life expectancy values for current country
  years = gapminder$year[gapminder$country == country]
  le = gapminder$lifeExp[gapminder$country == country]
  
  # Call function to find largest increase for this country
  largest = largest.increase(years, le)
  
  # Place maximum increase and year of increase in data frame for results
  increase$years[increase$country == country] = largest$value
  increase$year[increase$country == country] = largest$date
  
} # for - country

# Identify largest increase across all countries
max.increase = max(increase$years)
# Print the row with the largest increase
increase[which(increase$years == max.increase),]



## 2. Find the year with the greatest increase in LE for a given country
# Purpose: Determine the largest increase in values between two adjacent dates 
# Inputs: A vector of dates and a vector of corresponding values
# Return Value: A list with the maximum change in value and the corresponding date
#  from the end of the interval.

largest.increase <- function(dates, values) {
  # check that inputs are same length and nonzero
  stopifnot(length(dates) == length(values), length(dates) > 1)
  
  # calculate differences in values
  differences = diff(values)
  
  # find maximum difference
  max.difference = max(differences)
  # find index of first maximum
  max.difference.index = which(differences == max.difference)
  # if the maximum occurs more than once get the index of the first occurence
  if (length(max.difference.index) > 1) {
    max.difference.index = max.difference.index[1]
  } # if - length max.index
  
  # retrieve year corresponding to maximum
  date.largest = dates[max.difference.index + 1]
  
  return(list(value=max.difference, date=date.largest))
} # largest.increase



