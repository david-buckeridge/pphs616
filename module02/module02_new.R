library(surveillance)

# introduce ts and sts objects


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

# interpret results


# simulate a time series
sts <- sim.pointSource(p = 0.99, r = 0.5, length = 400,
                      A = 1, alpha = 1, beta = 0, phi = 0,
                       frequency = 1, state = NULL, K = 1.7)

plot(sts)



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

# compare performance of algorithms for single sts
algo.compare(algo.call(sts2disProg(flu.sts), control = control))


# simulate 10 series
ten <- lapply(1:10, function(x) {
      sim.pointSource(p = 0.975, r = 0.5, length = 400,
                      A = 1, alpha = 1, beta = 0, phi = 0,
                      frequency = 1, state = NULL, K = 1.7)
  })

# apply to all 10 series, with results as list
ten.surv <- lapply(ten, function(ts) {
     algo.compare(algo.call(ts, control=control)) })
  
#Average results
algo.summary(ten.surv)
