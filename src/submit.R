library("BatchExperiments")

## load registry defined in simulation.R
reg <- loadRegistry("reg")
## per-job resources for torque cluster with 80 cores
resources <- list(walltime = 86400L * 1, nodes = 1L, memory = "8gb")

## from earlier version but no need to redefine registry
removeExperiments(reg, findExperiments(reg, prob.pars = (eta < 1)))

## find ids for jobs not executed or on system, chunk them, and send them
## to the scheduler
ids <- findNotStarted(reg)
ids <- ids[!ids %in% findOnSystem(reg)]
submitJobs(reg, chunk(ids, n.chunks = 80, shuffle = TRUE), resources)

