# A program to illustrate some basic features in R for pphs 616
# By David Buckeridge

# Declare and load libraries
library(ggplot2)
library(gapminder)

# Compute mean Life Expectancy and GDP by Country
## Using aggregate with formula notation
mean.values = aggregate(cbind(lifeExp, gdpPercap, pop) ~ country, data = gapminder, mean)

# Extract data frame with continent for each country
#  then join to mean.values so results can be plotted 
#  by country and continent
continents = unique(gapminder[,c("country","continent")])
plot.data = merge(mean.values, continents, by="country")

# Plot life expectancy by gdp for each country, by continent using ggplot2
ggplot(data = plot.data, aes(x=lifeExp, y=gdpPercap, size=pop, label=country)) +
  geom_point(aes(colour=factor(plot.data$continent), alpha=0.1)) +
  scale_size_continuous(range=c(2,15)) +
  guides(alpha=FALSE) +
  labs(color = "Continent", size="Population")


# Same plot for country values using regular graphics
plot(plot.data$lifeExp, plot.data$gdp, type='p')
