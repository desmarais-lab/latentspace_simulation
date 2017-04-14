library("batchtools")

## load registry defined in simulation.R
reg = loadRegistry("lsm")
reg$cluster.functions = makeClusterFunctionsTORQUE("template.tmpl")

## Per-job resources for torque cluster with 80 cores
resources = list(walltime = 86400L * 1, nodes = 1L, memory = "8gb")

## find ids for jobs not executed or on system, chunk them, and send them
## to the scheduler
ids = getJobTable()[, .(job.id, problem, algorithm)]
ids = ids[!ids$job.id %in% c(findDone()$job.id, findOnSystem()$job.id), ]
ids[, chunk := chunk(job.id, chunk.size = 48), by = "problem"]
ids[, chunk := .GRP, by = c("problem", "chunk")]
chunks.to.submit = sort(unique(ids[ids$problem == "bilinear.test", chunk]))
submitJobs(ids[ids$chunk %in% chunks.to.submit[1:100], ], resources = resources)


## when done
res = reduceResultsDataTable()
write.csv(res, "results_amen.csv", row.names = FALSE)
