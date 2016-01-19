# A program to illustrate some basic features in R for pphs 616
# By David Buckeridge

# Load libraries
library(ggplot2)

# Load functions
source("functions/programming.functions.R")

# Load the gapminder date set
data.url = "https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv"
gapminder = read.csv(data.url)

# Compute mean Life Expectancy and GDP by Country
## First, create data frame with country names and continent
continents = unique(gapminder[,c("country","continent")])
n.rows = nrow(continents)

## Using aggregate with formula notation
mean.values.aggregate = aggregate(cbind(lifeExp, gdpPercap, pop) ~ country, data = gapminder, mean)


## Using a for loop
## Create a data frame with country names, continent names, and blank columns for values to be computed
mean.values.loop = data.frame(country=continents$country, continent=continents$country, 
                              lifeExp=numeric(n.rows), gdpPercap=numeric(n.rows), 
                              pop=numeric(n.rows))


## Calculate average values for each country and then place values in data frame
for (country in continents$country) {
  # Extract values from gapminder data frame
  lifeExp = mean(gapminder$lifeExp[gapminder$country == country])
  gdpPercap = mean(gapminder$gdpPercap[gapminder$country == country])ex
  pop = mean(gapminder$pop[gapminder$country == country])
  
  # Place calculated values into results data frame 
  mean.values.loop$lifeExp[mean.values.loop$country == country] = lifeExp
  mean.values.loop$gdpPercap[mean.values.loop$country == country] = gdpPercap
  mean.values.loop$pop[mean.values.loop$country == country] = pop
} # for - country



## Using a for loop and functions
## Create a data frame with country names, continent names, and blank columns for values to be computed
mean.values.loop.function = data.frame(country=continents$country, continent=continents$country, 
                              lifeExp=numeric(n.rows), gdpPercap=numeric(n.rows), 
                              pop=numeric(n.rows))


## Define functions for extracting mean values and updating results frames
for (country in continents$country) {
  for (value.col in c("lifeExp", "gdpPercap", "pop")) {
    mean.values.loop.function = update.results.frame(mean.values.loop.function, gapminder, country, "country", value.col)
  } # for - value.col
} # for - country






# Create data frame with values to be plotted
## Select one results data frame to use
results.to.use = mean.values
#results.to.use = mean.values.loop
#results.to.use = mean.values.loop.function
## Create data frame
plot.data = merge(results.to.use, continents, by="country")


# Plot life expectancy by gdp for each country using ggplot2
ggplot(data = plot.data, aes(x=lifeExp, y=gdpPercap, size=pop, label=country)) +
  geom_point(aes(colour=factor(plot.data$continent), alpha=0.1)) +
  scale_size_continuous(range=c(2,15)) +
  guides(alpha=FALSE) +
  labs(color = "Continent", size="Population")