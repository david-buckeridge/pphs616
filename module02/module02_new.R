# ------------- Identify Data ------------
# define locations of data and key file mapping run ids to simulation scenarios
setwd('/Users/davidbuckeridge/GitHub/pphs616')
data.dir = "data/surveillance_subset_noBWA_100samples"
key.filename = "data/key.csv"

# load functions
source("functions/outbreak.functions.R")

# load libraries
library(surveillance)
#library(MESS)


## ------------- Read Simulated Outbreaks -------------

# Set this number low for initial attempts, then use all the runs (at the indicated
#  concentration and duration) to answer the questions.
nruns = 10

# Generate n (1 to 100) runids for scenario with concentration 0.1 and duration 24 hours
runids = get.runids(key.filename, concentration=0.01, duration=72, n=nruns)

# If you want to use the same sample of runs each time, save the runids and then reload
#  them again, as opposed to generating new ids

#write(runids,"runids.txt")
#runids = (read.table("runids.txt"))[,1]

# load runs corresponding to runids
# runs = load.runs(data.dir, runids, os="mac")
runs = load.runs(data.dir, runids, os="mac")



## ------------- Describe Outbreaks -------------

# Calculate summary outbreak information and truth vectors for runs
outbreaks = lapply(runs, o.summary)

# Plot distribution of outbreak by maximum height and duration
par(mfrow=c(1,2))
hist(unlist(sapply(outbreaks, "[", "height")), xlab="Maximum Height (Daily Visits)", main="Maximum Height")
hist(unlist(sapply(outbreaks, "[", "length")), xlab="Duration (Days)", main="Duration")
par(mfrow=c(1,1))


# Create a sts object from a simulated run
a.run = runs[[1]]
a.outbreak = outbreaks[[1]]

a.sts = sts(observed=a.run$baseline.outbreak_count,
            start=c(as.numeric(strftime(a.run$date[1], "%Y")), 
                    as.numeric(strftime(a.run$date[1], "%j"))),
            frequency=365,
            state=(a.run$baseline.outbreak_count > a.run$baseline_count))


outbreak.year <- which(isoWeekYear(as.Date(a.run$date))$ISOYear == isoWeekYear(a.outbreak$start)$ISOYear)

control = list(range = outbreak.year, method="C1", alpha=0.05)
surv = earsC(a.sts, control=control)
plot(surv)
summary(surv)



## ------------- Apply One Method to One Real Weekly Time Series --------

# read a single time series and create a sts object
flu = read.csv("data/Table_S1_clean.csv")

# use neuzil rule for defining true outbreaks
seasons = seq(1993,2009)
# Identify influenza A season boundaries using calendar and WHO reporting data
#  according to a simplified verion of the method described by Neuzil
season.n = NULL
for (season in seasons) {
  n.tests = sum(flu$FLUA[flu$YRSEAS==season])
  season.n = c(season.n, flu$FLUA[flu$YRSEAS==season] > n.tests*0.01)
} # for


flu.sts = sts(observed=as.integer(flu$COUNT), 
              start=c(1993,1), 
              frequency = 52, 
              state=as.numeric(season.n))

plot(flu.sts)

length(flu$COUNT)


# apply algorithm and identify outbreak
# compare performance of algorithms for single sts
algo.compare(algo.call(sts2disProg(flu.sts), control = control))




## ------------- Apply One Method to One Simulated Weekly Time Series --------


# simulate a time series
sts <- sim.pointSource(p = 0.99, r = 0.5, length = 400,
                       A = 1, alpha = 1, beta = 0, phi = 0,
                       frequency = 1, state = NULL, K = 1.7)

plot(sts)



## ------------- Apply Many Methods to Many Simulated Weekly Time Series --------

# simulate 10 series
ten <- lapply(1:10, function(x) {
  sim.pointSource(p = 0.975, r = 0.5, length = 400,
                  A = 1, alpha = 1, beta = 0, phi = 0,
                  frequency = 1, state = NULL, K = 1.7)
})


# Declare algorithms to apply and set their parameters 
control = list(
  list(funcName = "rki1"),
  list(funcName = "rki2"),
  list(funcName = "rki3"),
  list(funcName = "bayes1"),
  list(funcName = "bayes2"),
  list(funcName = "bayes3"),
  list(funcName = "cdc",alpha=0.05),
  list(funcName = "farrington",alpha=0.05))

# define interval in sts for surveillance
control = lapply(control,function(ctrl) {
  ctrl$range = 300:400; return(ctrl)})


# apply to all 10 series, with results as list
ten.surv <- lapply(ten, function(ts) {
  algo.compare(algo.call(ts, control=control)) })

#Average results
algo.summary(ten.surv)


## ------------- Apply Many Methods to Many Simulated Daily Time Series --------




## ------------- Describe Outbreaks -------------

# Calculate summary outbreak information and truth vectors for runs
outbreaks = lapply(runs, o.summary)

# Plot distribution of outbreak by maximum height and duration
par(mfrow=c(1,2))
hist(unlist(sapply(outbreaks, "[", "height")), xlab="Maximum Height (Daily Visits)", main="Maximum Height")
hist(unlist(sapply(outbreaks, "[", "length")), xlab="Duration (Days)", main="Duration")
par(mfrow=c(1,1))



# Convert to STS objects with daily (and with weekly?)





