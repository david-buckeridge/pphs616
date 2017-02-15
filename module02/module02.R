# ------------- Identify Data ------------
# define locations of data and key file mapping run ids to simulation scenarios
setwd('/Users/davidbuckeridge/GitHub/pphs616')
data.dir = "data/surveillance_subset_noBWA_100samples/"
key.filename = "data/key.csv"

# load functions
source("functions/outbreak.functions.R")

# load libraries
library(MESS)

## ------------- Read Files -------------

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
runs = load.runs(data.dir, runids, os="mac")



## ------------- Describe Outbreaks -------------

# Calculate summary outbreak information and truth vectors for runs
outbreaks = lapply(runs, o.summary)

# Plot distribution of outbreak by maximum height and duration
par(mfrow=c(1,2))
hist(unlist(sapply(outbreaks, "[", "height")), xlab="Maximum Height (Daily Visits)", main="Maximum Height")
hist(unlist(sapply(outbreaks, "[", "length")), xlab="Duration (Hours)", main="Duration")
par(mfrow=c(1,1))


## ------------- Apply C2 Algorithm -------------

# Apply C2 algorithm to runs
res.c2 = lapply(runs, c2_all, gap=2, window=28, threshold=2)
# Determine detection and timeliness for each run
res.c2.detect = mapply(o.detected, res.c2, outbreaks)
res.c2.prevent = mapply(o.prevented, res.c2, outbreaks)


## ------------- Apply Poisson Algorithm -------------

# Apply poisson algorithm to runs
res.p = lapply(runs, poisson_all, dow=FALSE, gap=2, window=56, interval=14, threshold=0.05)
# Determine detection and timeliness for each run
res.p.far = mapply(a.far, res.p, outbreaks)
res.p.detect = mapply(o.detected, res.p, outbreaks)
res.p.prevent = mapply(o.prevented, res.p, outbreaks)

## ------------- Create Data for Evaluation ------------- 

n.cutoffs = 100

performance.c2.all = a.performance.all(res.c2, outbreaks, n.cutoffs)
performance.c2.avg = a.performance.avg(performance.c2.all)

par(mfrow=c(1,2))
plot(performance.c2.avg$far, performance.c2.avg$detected, type='s', 
		xlab='False Positive Rate', ylab="Sensitivity", xlim=c(0,1))
plot(performance.c2.avg$far, performance.c2.avg$detected*performance.c2.avg$prevented, type='s', 
		xlab='False Positive Rate', ylab="Sensitivity x Prevented", xlim=c(0,1))
par(mfrow=c(1,1))

auc.c2 = auc(performance.c2.avg$far, performance.c2.avg$detected)
auc.c2.weighted = auc(performance.c2.avg$far, (performance.c2.avg$detected*performance.c2.avg$prevented))


performance.p.all = a.performance.all(res.p, outbreaks, n.cutoffs)
performance.p.avg = a.performance.avg(performance.p.all)

par(mfrow=c(1,2))
plot(performance.p.avg$far, performance.p.avg$detected, type='s', 
		xlab='False Positive Rate', ylab="Sensitivity", xlim=c(0,1))
plot(performance.p.avg$far, performance.p.avg$detected*performance.p.avg$prevented, type='s', 
		xlab='False Positive Rate', ylab="Sensitivity x Prevented", xlim=c(0,1))
par(mfrow=c(1,1))

auc.p = auc(performance.p.avg$far, performance.p.avg$detected)
auc.p.weighted = auc(performance.p.avg$far, (performance.p.avg$detected*performance.p.avg$prevented))
























