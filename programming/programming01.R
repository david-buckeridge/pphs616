# A program to illustrate some basic features in R for pphs 616
# By David Buckeridge

# Declare and load libraries
library(ggplot2)

# Load the gapminder date set from https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv
data.url = "https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv"
gapminder = read.csv(data.url)

# Compute mean Life Expectancy and GDP by Country
## Using aggregate with formula notation
mean.values = aggregate(cbind(lifeExp, gdpPercap, pop) ~ country, data = gapminder, mean)
continents = unique(gapminder[,c("country","continent")])

plot.data = merge(mean.values, continents, by="country")

# Plot life expectancy by gdp for each country using ggplot2
ggplot(data = plot.data, aes(x=lifeExp, y=gdpPercap, size=pop, label=country)) +
  geom_point(aes(colour=factor(plot.data$continent), alpha=0.1)) +
  scale_size_continuous(range=c(2,15)) +
  guides(alpha=FALSE) +
  labs(color = "Continent", size="Population")


# Same plot using regular graphics
plot(plot.data$lifeExp, plot.data$gdp, type='p')