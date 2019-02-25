
### Setting up R environment ################################
# Refresh the environment 
rm(list= ls()) 

# Install and Load the necessary packages 
library("rgeos")
library("maptools")
library("sf")
library("spdep")
library("rgdal")
library("sp")
library("classInt") # For mapping 
library("RColorBrewer") # For mapping
library("grid") # For mapping
library("CARBayes") # Library to run conditional autoregressive model for areal data 
library("graphics")

# Move to your working directory, ideally as an RStudio project. Make sure that
# there is a sub-directory called "data" under this directory where all data are
# stored.
setwd('C:/Users/hmamiya/OneDrive/Coursework/PPHS/PPHS_module03')



### 1. Load and prepare data  ######################

# load GI count data by fsa, sex, and age. 
giData <- read.csv("data/giMontreal2006_quarter.csv", header = TRUE, stringsAsFactors = FALSE)
str(giData) #inspect 
head(giData) #inspect

# load 2011 census data containinig socio-economic and demographic data at FSA level 
fsaCensus2011 <- read.csv("./data/fsaCensusMontreal2011_imputed.csv", header = TRUE, stringsAsFactors = FALSE)
str(fsaCensus2011) #inspect
head(fsaCensus2011) #inspect


# Load shapefile (point of hospitals). The dsn argument indicates the path to
# the file, and the layer argument indicates the name of shapefile, usually
# without file extension.
hospPoint <- readOGR(dsn = "./data/Hospitals_Montreal", layer = "shapefile_Hospitals_Mtl")

# Load polygons for Forward Sortation Area in Monteral.
fsaShape <- readOGR(dsn = "./data/FSA_Montreal_2001", layer = "shapefile_FSA_Montreal_2001")

# FSA shape is prepared for you, but for your future study or fun activity can
# be downloaded from here too (2011 file below)
# https://www12.statcan.gc.ca/census-recensement/2011/geo/bound-limit/bound-limit-2011-eng.cfm



### Example showing re-projection of spatial data ######################

# Are they in the same coordinate refernce system (CRS)? If not, one needs to be
# re-projected to the same shape of the earth.
proj4string(fsaShape)
proj4string(hospPoint)
identicalCRS(fsaShape, hospPoint)
# If false, they cannot be overlaid (can't be plot on the same map), and
# distance-based analysis (e.g. distance between points) can be incorrect

fsaShape  <- spTransform(fsaShape, CRS("+init=epsg:2959"))
hospPoint <- spTransform(hospPoint, CRS("+init=epsg:2959"))
# coordinate reference system I used for Montreal is found here:
# http://spatialreference.org/ref/epsg/nad83csrs-utm-zone-18n/

identicalCRS(fsaShape, hospPoint)


## Question 1. Could the assessment of relative risk in section 3 of the 
##             module be affected by using an incorrect projection? Explain.


### How to access attributes in S4-class spatial data?  ######################

typeof(fsaShape) 
class(fsaShape) 
summary(fsaShape) #show summary of spatial and attribute data 
summary(fsaShape@data) #show summary attribute data (census data for each fsa)

# FSA-level population in 1996 census as an example
summary(fsaShape$POP96) 
summary(fsaShape@data$POP96) 
summary(fsaShape@data[, "POP96"]) 
fsaShape[["POP96"]]

# Unlike attribute data, polygon (spatial data) is saved like this...example
fsaShape@polygons[[1]] #first polygon (area, H1A postal code) 
fsaShape@proj4string # Projection 



### Attribute join i.e. Adding census attributes to FSA shape data ########################

# Analysis will be based on FSA, so first aggregate the observed 
# and expected GI count into FSA-level across age and sex groups in the GI outcome table
giDataFsa <- aggregate(data = giData, cbind(giCount, denomCount, giExpectedCount) ~ fsa, FUN = sum)

# Additionally, we will calculte FSA-level rate of GI
giDataFsa$giFsaRate <- giDataFsa$giCount / giDataFsa$denomCount

# NOW perform join 
# Merge FSA shapefle and GI count data.
fsaShape@data <- data.frame(fsaShape@data, giDataFsa[match(fsaShape$FSA, giDataFsa$fsa), ])
# Alternatively, do this, but never refer data frame using @data if using "merge" function
# fsaShape <- merge(fsaShape, giDataFsa, by.x= "FSA", by.y = "fsa")

# Merge FSA shapefile with 2011 census data i.e. attach census attributes to the shapefle 
fsaShape@data <- data.frame(fsaShape@data, fsaCensus2011[match(fsaShape$FSA, fsaCensus2011$fsa), ])




### 2. Plotting maps #############################################################

## 2.1 Plot Points
# Plot FSA shape first, then try to overlay with hospital points - Remember if
# using markdown, the two plot commands need to be executed together (not line
# by line)
plot(fsaShape)#plot shape of Montreal partitioned by FSA
plot(hospPoint, add=TRUE, col= "blue") # I am trying to overlay point (hospital) data to fsa polygon 

# Subset hopsital data by island
hospPointIsland <- hospPoint[fsaShape, ]
plot(fsaShape, main="FSA and hospitals"); plot(hospPointIsland, col = "blue", add=TRUE)

# if you like, display FSA label 
text(coordinates(fsaShape),as.character(fsaShape@data$FSA),cex=0.6)



## 2.2 Plot Risk Factor as Choropleth
# Now use census attributes that you joined (merged) to the FSA spatial polygon
# data, and make a map of one of these attributes, median family income, by FSA.

# define number of color bins to group continuous measure of median family income 
nclr = 5

# define color palette -- see http://colorbrewer2.org 
plotclr = brewer.pal(nclr,"Greens")

# define the classes or breaks for the data
class = classIntervals(fsaShape@data$med_income, nclr, style="quantile", dataPrecision=0)

# create a vector of colors for each region
colcode = findColours(class,plotclr)

# plot the region boundaries - again, run all the three lines as a chunk if using R markdown
plot(fsaShape, col=colcode)
title(sub="Median Family Income", main='Classed Choropleth')
legend('topleft', legend=names(attr(colcode, "table")), fill=attr(colcode,"palette"), cex=0.9, bty='n')


## 2.3 Plot median family income as a proportional symbol or 'bubble map'
max.symbol.size=5
min.symbol.size=1
plotvar = fsaShape@data$med_income
# create symbols for each FSA with size scaled to income in FSA
symbol.size = ((plotvar-min(plotvar))/(max(plotvar)-min(plotvar))*(max.symbol.size-min.symbol.size) + min.symbol.size)
# plot FSA boundaries
plot(fsaShape)
# get coordinates for centroids of FSA
mtl.cntr = coordinates(fsaShape)
# plot circles of graduate size and color at centroids
points(mtl.cntr, pch=16, col=colcode, cex=symbol.size)
# outline the circles
points(mtl.cntr, cex=symbol.size)
title(sub="Median Family Income", main="Bubble Plot")
legend('topleft', legend=names(attr(colcode, "table")), fill=attr(colcode,"palette"), cex=0.9, bty='n')



## 2.4 Plot disease outcomes as choropleth

# Create a map showing crude GI visits by FSA as rate (Number of GI visits over
# number of residents in each FSA) 

#color bin to categorize the intensity of GI visits
nclr = 5

# Plot map with four different levels of grouping of the outcome
groups = c(3,5,7,9)
par(mfrow=c(2,2))

for (group in groups) {
  plotclr = brewer.pal(nclr, 'Reds')
  class.crude = classIntervals((fsaShape@data$giFsaRate*1000), group, style='quantile', dataPrecision=0)
  colcode.crude = findColours(class.crude, plotclr)
  
  plot(fsaShape)
  plot(fsaShape, col=colcode.crude, add=T)
  title(sub="Crude Rates of GI Visits by FSA 
        (Annual Visits per 1,000)")
  legend('topleft', legend=names(attr(colcode.crude, "table")), fill=attr(colcode.crude,"palette"), cex=0.9, bty='n')
} # for - levels

par(mfrow=c(1,1))


## Question 2. Comment on the spatial patterns you do or do not observe at each level of 
##             grouping. Is there any relationship between the number of groups and the concept 
##             of smoothing? Explain. 




### 3. Analysis of areal disease count - Disease risk mapping ###############################################

# Here we will calculate and map the relative risk of GI cases by FSA using SMR
# (Standaridized Morbidity ratio), none-spatial model, and spatial model.


### 3.1 Crude SMR (Please review standardized morbidity ratio from the first year)

# SMR of GI visits for area i is: SMR_i = Observed_count_i/Expected_count_i
# Where age-sex standardized expected count of GI in area i can be calculated
# by indirect standardization. Expected count of area i is already provided to you and stored in
# the column `fsaShape@data$giExpectedCount`. This is a sum of expected count,
# where stratum-specific population (age and sex in this excercise) in each FSA
# is multiplied by the stratum-specific GI rate across the entire study region
# of Montreal.

# Note that the variance of SMR is; 
# VAR(SMR) = SMR/Expected_Count
# Thus, the smaller the expected count, larger the variance. 

# We will map SMR, a maximum likelihood estimator of area-level relative risk  

# Using the expected and oversed GI count, caulcate SMR and its variance 
fsaShape@data$SMR <- fsaShape@data$giCount / fsaShape@data$giExpectedCount
fsaShape@data$varSMR <- fsaShape@data$SMR / fsaShape@data$giExpectedCount


## Question 3 - Plot SMR, its variance, population counts, and disease counts 
##              in a 2x2 gridded plot as above. Describe the spatial trends in 
##              the crude SMR and variance.



### 3.2 Smoothed SMR using hierarchical Bayesian spatial model (Besag York Mollie
### conditional autoregressive model). 


# Create an R object (list) showing which FSA is adjacent to which
fsaNb = poly2nb(fsaShape, queen=FALSE) # this function creates a neighorhood object called nb 


# Inspect the list for the neighbourhood structure.  
head(fsaNb) # list of areas showing neighbor ID
summary(fsaNb) # Note that one region has no link (neighbour) -- is this realistic? 
is.symmetric.nb(fsaNb) # Neighborhood needs to be symmetric (i.e. area i is neighbour of j, and vice versa)

# Plot and see how connection is defined 
plot(fsaShape, border='darkgrey', las=1, main='Connectivity by Shared Borders', sub = "not quite correct definition of neighorhood")
plot(fsaNb, coordinates(fsaShape), add=TRUE)


# The spatial structure above is not valid due to the presence of an isolated
# area, so need to create new one. Already done for you and ready to load. Read
# another neighbour list where the connection was manually created to represent
# bridges so that there is no isolated neighbour
fsaNbBridge <- read.gal('data/fsaNbBridge')
summary(fsaNbBridge) #now empty neighbour shoud be gone

plot(fsaShape, border='lightgrey',  las=1, main='Neighborhood definition by Shared Borders')
plot(fsaNbBridge, coordinates(fsaShape), col="red", add=TRUE, cex = 0.3) #new spatial relationship
plot(fsaNb, coordinates(fsaShape), pch = 0.1, cex = 0.1, points=FALSE, add=TRUE, col="black") #old (and invalid) spatial relationship

# Finally generate a spatial weight matrix containing spatial structure 
# Matrix whose elements are coded 1 if two areas are adjacent(column and row are
# contiguous), otherwise zero
W <- nb2mat(fsaNbBridge, style = "B") 


# Center and scale covariates - helps convergence of MCMC 
fsaShape$income_center <- scale(log(fsaShape@data$med_income), center = TRUE, scale = TRUE)[,1]
fsaShape$immig_center <- scale((fsaShape@data$prop_immig), center = TRUE, scale = TRUE)[,1]
fsaShape$young_center <- scale((fsaShape@data$prop_age_under18), center = TRUE, scale = TRUE)[,1]
fsaShape$old_center <- scale((fsaShape@data$prop_age_over65), center = TRUE, scale = TRUE)[,1]
fsaShape$education_center <- scale((fsaShape@data$prop_ps), center = TRUE, scale = TRUE)[,1]
fsaShape$popdensity_center <- scale((fsaShape@data$popdensity_km), center = TRUE, scale = TRUE)[,1]

# Run Besag-York-Mollie Conditional Autoregressive (BYM-CAR) model for areal data
# This could take a long time 
giFitCar <- S.CARbym(giCount~income_center + immig_center + young_center + education_center + popdensity_center + offset(log(giExpectedCount)), 
              family="poisson", 
              data=fsaShape@data, 
              W = W, # Add the spatial structure here 
              burnin = 100000, 
              n.sample = 500000, thin = 20)


summary(giFitCar)
colnames(giFitCar$samples$beta) <- colnames(giFitCar$X)
# plottting could take a few min depending on computer
plot(giFitCar$samples$beta) #Important to check convergence 
plot(giFitCar$samples$psi[, sample(1:ncol(giFitCar$samples$psi), 6)]) #Check convergence of some spatial random effects
print(giFitCar) # Posterior summary (Inference on regression coefficients)


## Question 4 - Calculate the relative risk (RR) for each area (remember 
##              that observed count = RR*Expected count), where expected count is 
##              in fitted.values in the model output.  Map the RR, and compare
##              the estimated risk surface with that of the crude (non-model based) 
##              SMR. (Hint: after you caulcte the RR, you must join the results to
##              the fsaShape spatial data frame for mapping as demonstrated above).






