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


#1. Using aggregate with formula notation
mean.values.aggregate = aggregate(cbind(lifeExp, gdpPercap, pop) ~ country, data = gapminder, mean)


#2. Using a for loop
## Create a data frame with country names, continent names, and blank columns for values to be computed
mean.values.loop = data.frame(country=continents$country, continent=continents$continent, 
                              lifeExp=numeric(n.rows), gdpPercap=numeric(n.rows), 
                              pop=numeric(n.rows))


## Calculate average values for each country and then place values in data frame

#2a. Example control flow using an integer counter
for (i in 1:length(continents$country)){
  country = continents$country[i]
  # ...
  
} # for

#2b. Example control flow iterating over items in a collection
for (country in continents$country) {
  # Extract values from gapminder data frame
  lifeExp = mean(gapminder$lifeExp[gapminder$country == country])
  gdpPercap = mean(gapminder$gdpPercap[gapminder$country == country])
  pop = mean(gapminder$pop[gapminder$country == country])
  
  # Place calculated values into results data frame 
  mean.values.loop$lifeExp[mean.values.loop$country == country] = lifeExp
  mean.values.loop$gdpPercap[mean.values.loop$country == country] = gdpPercap
  mean.values.loop$pop[mean.values.loop$country == country] = pop
} # for - country



#2c. Example using a for loop and very general functions
## Create a data frame with country names, continent names, and blank columns for values to be computed
mean.values.loop.function = data.frame(country=continents$country, continent=continents$continent, 
                              lifeExp=numeric(n.rows), gdpPercap=numeric(n.rows), 
                              pop=numeric(n.rows))


## Call funcion to estimate mean values for each country and each value.
for (country in continents$country) {
  for (value.col in c("lifeExp", "gdpPercap", "pop")) {
    mean.values.loop.function = update.results.frame(mean.values.loop.function, gapminder, country, "country", value.col)
  } # for - value.col
} # for - country



#2d. Example using a for loop and some functions that are easier to understand...
## Create a data frame with country names, continent names, and blank columns for values to be computed
mean.values.loop.function.simple = data.frame(country=continents$country, continent=continents$continent, 
                                       lifeExp=numeric(n.rows), gdpPercap=numeric(n.rows), 
                                       pop=numeric(n.rows))


for (country in continents$country) {
  # Retrieve rows in data frame for current country and all three measures of lifeExp, gdpPercap, and pop
  values = get.values(data = gapminder, category.col = "country", category = country, value.col = c("lifeExp", "gdpPercap", "pop"))
  
  # Calculate mean for each column of interest
  values.mean = apply(values, 2, mean)
  
  # Add calculated values for each column to summary data frame
  mean.values.loop.function.simple[mean.values.loop.function.simple$country == country, c("lifeExp", "gdpPercap", "pop")] = values.mean
} # for - country


#3. Using vectorization 
## aggregate is actualy a wrapper for tapply, so we have already solved the problem using vectorization!
## another example would be using by()...





# Create data frame with values to be plotted
## Select one results data frame to use
results.to.use = mean.values.aggregate
#results.to.use = mean.values.loop
#results.to.use = mean.values.loop.function
#results.to.use = mean.values.loop.function.simple

## Create data frame for plotting
plot.data = merge(results.to.use, continents, by="country")


# Plot life expectancy by gdp for each country using ggplot2
ggplot(data = plot.data, aes(x=lifeExp, y=gdpPercap, size=pop, label=country)) +
  geom_point(aes(colour=factor(plot.data$continent), alpha=0.1)) +
  scale_size_continuous(range=c(2,15)) +
  guides(alpha=FALSE) +
  labs(color = "Continent", size="Population")