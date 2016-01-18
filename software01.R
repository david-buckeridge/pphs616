# A program to illustrate some basic features in R for pphs 616
# By David Buckeridge

# Declare and load libraries
library(ggplot2)

# Load the gapminder date set from https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv
data.url = "https://raw.githubusercontent.com/resbaz/r-novice-gapminder-files/master/data/gapminder-FiveYearData.csv"
gapminder = read.csv(data.url)

# Compute group mean of Life Expectancy by Country
## Using aggregate
le = aggregate(lifeExp ~ country, data = gapminder, mean)
gdp = aggregate(gdpPercap ~ country, data = gapminder, mean)
plot.data = data.frame(country = gdp$country, gdp = gdp$gdpPercap, le = le$lifeExp)


# Plot life expectancy by gdp for each country
ggplot(data = plot.data, mapping = aes(x=le, y=gdp)) +
  geom_point()
