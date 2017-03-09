## 0. Load Libraries and Data
# load libraries
library(maptools)
library(spdep)
library(classInt)
library(RColorBrewer)

# define path to data
setwd("/Users/david/GitHub/pphs616")

fsa.shp.name = "data/Montreal_FSA_2001/Montreal_FSA_Census_2001_MTM"
fsa.visit.name = "data/montreal_ili_visits.csv"

# read shapefile with FSA boundaries and census variables
fsa.shp = readShapePoly(fsa.shp.name, IDvar='FSA')
#summary(fsa.shp)
#attributes(fsa.shp@data)

# read file with data on ili visits by FSA
fsa.visits = read.table(fsa.visit.name, header=TRUE, sep=",")
summary(fsa.visits)



## 1. Plot Census Data Using Different Types of Maps
## 1.1 Plot median family income as choropleth map
# define number of classes in plot
nclr = 5
# define color palette -- see http://colorbrewer2.org 
plotclr = brewer.pal(nclr,"Greens")
# define the classes or breaks for the data
class = classIntervals(fsa.shp@data$Median_Fam, nclr, style="quantile", dataPrecision=0)
# create a vector of colors for each region
colcode = findColours(class,plotclr)

# plot the region boundaries
plot(fsa.shp)
# add the colors, title and legend
plot(fsa.shp, col=colcode, add=T)
title(sub="Median Family Income", main='Classed Choropleth')
legend('topleft', legend=names(attr(colcode, "table")), fill=attr(colcode,"palette"), cex=0.9, bty='n')

## 1.2 Plot median family income as a proportional symbol or 'bubble map'
max.symbol.size=5
min.symbol.size=1
plotvar = fsa.shp@data$Median_Fam
# create symbols for each FSA with size scaled to income in FSA
symbol.size = ((plotvar-min(plotvar))/(max(plotvar)-min(plotvar))*(max.symbol.size-min.symbol.size) + min.symbol.size)
# plot fsa boundaries
plot(fsa.shp)
# get coordinates for centroids of FSA
mtl.cntr = coordinates(fsa.shp)
# plot circles of graduate size and color at centroids
points(mtl.cntr, pch=16, col=colcode, cex=symbol.size)
# outline the circles
points(mtl.cntr, cex=symbol.size)
title(sub="Median Family Income", main="Bubble Plot")
legend('topleft', legend=names(attr(colcode, "table")), fill=attr(colcode,"palette"), cex=0.9, bty='n')



## 2. Plot Crude ILI Visit Rates 
# Join ili data with data from shapfile - use merge to ensure correct linkage...
ili = data.frame(fsa=fsa.shp@data$FSA, pop=fsa.shp@data$POP96)
ili = merge(fsa.visits, ili, by='fsa')
# divide 3-year counts by 3 to approximate an annual rate
ili$rate = (ili$visits / 3) / ili$pop

# Plot map with four different levels of grouping of outcome
groups = c(3,5,7,9)
par(mfrow=c(2,2))

for (group in groups) {

  plotclr = brewer.pal(nclr, 'Reds')
  class.crude = classIntervals((ili$rate*1000), group, style='quantile', dataPrecision=0)
  colcode.crude = findColours(class.crude, plotclr)

  plot(fsa.shp)
  plot(fsa.shp, col=colcode.crude, add=T)
  title(sub="Crude Rates of ILI Visits by FSA (Annual Visits per 1,000)")
  legend('topleft', legend=names(attr(colcode.crude, "table")), fill=attr(colcode.crude,"palette"), cex=0.9, bty='n')
} # for - levels
  
par(mfrow=c(1,1))


## 3. Build a connectivity matrix
# Use triangulation to build connectivity
fsa.nb.tri = tri2nb(coordinates(fsa.shp), row.names=fsa.shp@data$FSA)
summary(fsa.nb.tri)
# What are the least and most connected regions? Verify these regions on the map, do they make sense?

plot(fsa.shp, border='darkgrey', las=1, main='Connectivity by Triangulation')
plot(fsa.nb.tri, coordinates(fsa.shp), add=TRUE)
# Does the plot correspond to your idea of connectivity? Why?


# Use nearest neighbors to build connectivity
k1 = knn2nb(knearneigh(coordinates(fsa.shp)))
max.distance = max(unlist(nbdists(k1, coordinates(fsa.shp))))
fsa.nb.knn = dnearneigh(coordinates(fsa.shp), 0, max.distance, fsa.shp@data$FSA)
summary(fsa.nb.knn)

plot(fsa.shp, border='darkgrey', las=1, main='Connectivity by Nearest Neighbors')
plot(fsa.nb.knn, coordinates(fsa.shp), add=TRUE)
# Does the plot correspond to your idea of connectivity? Why?

# try with topology of shape file
fsa.polys = polygons(fsa.shp)
fsa.nb.poly = poly2nb(fsa.polys, queen=FALSE)
summary(fsa.nb.poly)

plot(fsa.shp, border='darkgrey', las=1, main='Connectivity by Shared Borders')
plot(fsa.nb.poly, coordinates(fsa.shp), add=TRUE)
# Does the plot correspond to your idea of connectivity? Why?

# Read cleaned nb file (essentially the toplogy file with some manual editing...)
fsa.nb = read.gal('data/fsa.nb.clean')
summary(fsa.nb)

plot(fsa.shp, border='darkgrey', las=1, main='Connectivity by Shared Borders')
plot(fsa.nb, coordinates(fsa.shp), col="red", add=TRUE)
plot(fsa.nb.poly, coordinates(fsa.shp), add=TRUE)



## 4. Smooth Rates

ili.ebe = EBlocal(ili$visits/(3*52), ili$pop, fsa.nb)
ili.ebe[1:10,]

nclr = 5
plotclr = brewer.pal(nclr, 'Reds')
class.raw = classIntervals(round(ili.ebe$raw*10000,0), nclr, style='quantile')
# Note that we take the breaks from the crude rate map and pass them to the 
#  smoothed rates map so that we have the same class boundaries for both maps
class.est = classIntervals(round(ili.ebe$est*10000,0), n=nclr, style='fixed',
                           fixedBreaks=class.raw$brks)

colcode.raw = findColours(class.raw, plotclr)
colcode.est = findColours(class.est, plotclr)

par(mfrow=c(1,2))

plot(fsa.shp)
plot(fsa.shp, col=colcode.raw, add=T)
title(sub="Raw Rates of ILI Visits (Weekly Visits per 10,000)")
legend('topleft', legend=names(attr(colcode.raw, "table")), fill=attr(colcode.raw,"palette"), cex=0.9, bty='n')

plot(fsa.shp)
plot(fsa.shp, col=colcode.est, add=T)
title(sub="EBE (Local) Smoothed Rates of ILI Visits (Weekly Visits per 10,000)")
legend('topleft', legend=names(attr(colcode.est, "table")), fill=attr(colcode.est,"palette"), cex=0.9, bty='n')

par(mfrow=c(1,1))


## 5. Estimate Moran's I
# examine and transform data as needed
hist(ili.ebe$raw)
hist(sqrt(ili.ebe$raw))

# create row-standardized weight matrix
fsa.w = nb2listw(fsa.nb)
#str(fsa.w)

(ili.moran = moran.test(sqrt(ili.ebe$raw), fsa.w))
moran.plot(sqrt(ili.ebe$raw), fsa.w, labels=fsa.visits$fsa)


# plot local Moran's I values and compare to raw values
ili.moran.local = localmoran(sqrt(ili.ebe$raw), fsa.w)

nclr = 5
plotclr = brewer.pal(nclr, 'Reds')
class.ili = classIntervals(ili.moran.local[,1], nclr, style='quantile')

colcode.ili = findColours(class.ili, plotclr)

par(mfrow=c(1,2))

class.crude = classIntervals((ili$rate*1000), nclr, style='quantile', dataPrecision=0)
colcode.crude = findColours(class.crude, plotclr)

plot(fsa.shp)
plot(fsa.shp, col=colcode.crude, add=T)
title(sub="Crude Rates of ILI Visits by FSA (Annual Visits per 1,000)")
legend('topleft', legend=names(attr(colcode.crude, "table")), fill=attr(colcode.crude,"palette"), cex=0.9, bty='n')

plot(fsa.shp)
plot(fsa.shp, col=colcode.ili, add=T)
title(sub="Local Moran's I for ILI")
legend('topleft', legend=names(attr(colcode.ili, "table")), fill=attr(colcode.ili,"palette"), cex=0.9, bty='n')

par(mfrow=c(1,1))

