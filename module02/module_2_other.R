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
plot(surv, legend.opts=list(horiz=TRUE, x="topright"))


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


