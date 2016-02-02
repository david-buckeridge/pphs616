## ------------- Module 1 Example Code -------------

## ------------- Load Data -------------

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

# Add periseason boundaries (Nov to Apr) to plot
nov = seq(from=as.Date("1992-11-01"), to=as.Date("2008-11-01"), by="years")
apr = seq(from=as.Date("1993-04-01"), to=as.Date("2009-04-01"), by="years")
y.pts = rep(c(3500,5500,5500,3500), length(nov))
x.pts = NULL
for (month in 1:length(nov)){
	x.pts = c(x.pts, c(nov[month], nov[month], apr[month], apr[month]))
} # for
polygon(x.pts, y.pts, density=NULL, col="#00009933", border=NA)

# Identify influenza A season boundaries using calendar and WHO reporting data
season.n = NULL # Neuzil
season.i = NULL # Izurieta
for (season in seasons) {
	n.tests = sum(death$FLUA[death$YRSEAS==season])
	season.n = c(season.n, death$FLUA[death$YRSEAS==season] > n.tests*0.01)
	season.i = c(season.i, death$FLUA[death$YRSEAS==season] > n.tests*0.05)
} # for

# Plot season using neuzil and izurieta
periseason = c("Nov","Dec","Jan","Feb","Mar","Apr")
death$peri = is.element(format(death$DEATHDT, format="%b"), periseason)
death$neuzil = season.n
death$izurieta = season.i
# Modify outlier that is not connected to season
death$neuzil[death$WEEK==206] = FALSE

# Add seasonal boundaries to plot
points(death$DEATHDT, (death$neuzil*3500-50), pch="-", cex=1)
points(death$DEATHDT, (death$izurieta*3500-25), pch="-", cex=1, col='red')


## ------------- Periseason -------------
# Calculate excess mortality using periseason approaches
weeks.p = sum(death$peri==TRUE & death$neuzil==FALSE & death$izurieta==FALSE)
count.p = sum(death$COUNT[(death$peri==TRUE & death$neuzil==FALSE & death$izurieta==FALSE)])

weeks.n = sum(death$peri==TRUE & death$neuzil==TRUE)
count.n = sum(death$COUNT[(death$peri==TRUE & death$neuzil==TRUE)])

weeks.i = sum(death$peri==TRUE & death$izurieta==TRUE)
count.i = sum(death$COUNT[(death$peri==TRUE & death$izurieta==TRUE)])

# Rate difference (deaths / week)
rd.n = count.n / weeks.n - count.p / weeks.p
rd.i = count.i / weeks.i - count.p / weeks.p

excess = data.frame(season=seasons, n.total=rep(NA,length(seasons)), n.weeks=rep(NA,length(seasons)), i.total=rep(NA,length(seasons)), i.weeks=rep(NA,length(seasons)))
for (season in seasons) {
	excess$n.weeks[excess$season==season] = nrow(death[death$peri==TRUE & death$neuzil==TRUE & death$YRSEAS==season,])
	excess$n.total[excess$season==season] = sum(rd.n * excess$n.weeks[excess$season==season])
	excess$i.weeks[excess$season==season] = nrow(death[death$peri==TRUE & death$izurieta==TRUE & death$YRSEAS==season,])
	excess$i.total[excess$season==season] = sum(rd.i * excess$i.weeks[excess$season==season])
} # for - seasons


## ------------- Serfling -------------
# Define variables

# Censor data during circulating influenza periods for fitting model

# Create data frame without outcome for prediction of censored weeks

# Fit the model

# Predict deaths for censored days (and obtain model fit for other days)

# Plot fit and predicted values
points(death$DEATHDT[death$neuzil==TRUE & death$peri==TRUE], serfling.predict[death$neuzil==TRUE & death$peri==TRUE], pch=1, cex=0.4, col="light blue")
points(death$DEATHDT[(death$neuzil==FALSE | death$peri==FALSE)], serfling.predict[(death$neuzil==FALSE | death$peri==FALSE)], pch=16, cex=0.4, col="dark blue")

# Determine excess deaths per season using Serfling model. Consider excess over all days in the season and only days the observed count is above the predicted.


## ------------- Poisson -------------
# Define data structures

# Fit the model

# Predict deaths for all days with and without influenza A circulating

# Determine excess deaths per season using Poisson model. 


## ------------- Plot Excess Deaths by Method -------------
# Create boxplot of excess deaths by method, with seasons as observations
# Load a graphics library
require(lattice)

# Prepare the data structure
excess.total = excess[,c("season","n.total","i.total","s.pos.total","p.pos.total")]
methods=c("neuzil","izurieta","serfling","poisson")
methods.vector = c(rep(methods[1],length(seasons)), rep(methods[2],length(seasons)), rep(methods[3],length(seasons)), rep(methods[4],length(seasons)))
excess.long = data.frame(season=rep(seasons,4), method=methods.vector, deaths=rep(NA,length(seasons)*4))
for (season in seasons) {
	for (method in methods) {
		excess.long$deaths[excess.long$season==season & excess.long$method==method] = excess.total[excess.total==season, (which(methods==method)+1)]
	} # for - methods
} # for - seasons

# Draw the plot
bwplot(deaths ~ method, data=excess.long, horizontal=FALSE)

