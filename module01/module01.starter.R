## ------------- Module 1 Starter Code -------------

## ------------- Load Data -------------
# Replace "myDirectory" with full path to location of the data file
#  unless you have set path in your project file.
# setwd("myDirectory")

# Read data files as downloaded from Fluview, with minor cleaning
death = read.csv("data/Table_S1_clean.csv")

# Cast columns to correct data types
death$DEATHDT = as.Date(death$DEATHDT)
death$MONTH = as.factor(death$MONTH)


## ------------- Plot Deaths and Seasons -------------
# Create date sequences for labelling plots
months = seq(from=head(death$DEATHDT,1), to=tail(death$DEATHDT,1), by="months")
quarters = seq(from=head(death$DEATHDT,1), to=tail(death$DEATHDT,1), by="quarters")
years = seq(from=as.Date("1993-06-15"), to=as.Date("2008-06-15"), by="years")
seasons = seq(1993,2009)

# Plot observed counts
## draw a plot with two Date axes, one with quarters, and one with years
plot(death$DEATHDT, death$COUNT, type='n', bty='n', xaxt='n', yaxt='n', xlab = "Date", ylab='', ylim=c(3500,5500), xlim=c(head(death$DEATHDT,1), tail(death$DEATHDT,1)))
axis.Date(1, at=quarters, format="%b", cex.axis=0.6, mgp=c(3,0.5,0))
axis.Date(1, at=years, format="%Y", tick=FALSE, cex.axis=0.8, mgp=c(5,1.5,0))

# Create axis and add data for all-cause mortality
death.col = "dark gray"
axis(2, at = seq(3500,5500,500), col=death.col, col.axis=death.col, mgp=c(2,0.5,0), cex.axis=0.8)
mtext("Deaths in Canada (per week)", side=2, line=2, at=4500, col=death.col, cex=0.8)
lines(death$DEATHDT, death$COUNT, type='l', col=death.col)

# Add periseason boundaries (Nov to Apr) to plot as transparent polygons


# Identify influenza A season boundaries using calendar and WHO reporting data


# Calculate and plot season boundaries using neuzil and izurieta definitions




## ------------- Periseason -------------


## ------------- Serfling -------------


## ------------- Poisson -------------


## ------------- Plot Excess Deaths by Method -------------

