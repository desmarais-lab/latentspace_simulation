library("batchtools")
options(batchtools.progress = FALSE)

## load registry defined in simulation.R
reg = loadRegistry("lsm")
reg$cluster.functions = makeClusterFunctionsTORQUE("template.tmpl")

## Per-job resources for torque cluster with 80 cores
resources = list(walltime = 86400L * 1, nodes = 1L, memory = "8gb")

## find ids for jobs not executed or on system, chunk them, and send them
## to the scheduler
ids = getJobTable()[, .(job.id, problem, algorithm, nodes)]
exclusions = rbind(findOnSystem(), findDone(), findStarted())
ids = ids[!job.id %in% exclusions$job.id, ]
amen.experiments = findExperiments("bilinear.test", algo.name = "amen",
  ids = ids[!job.id %in% exclusions$job.id, ])
## amen.pars = getJobPars(amen.experiments)
## amen.to.time = amen.pars[, .SD[1, ], by = nodes]$job.id
## amen.timing = sapply(amen.to.time, function(x) system.time(testJob(x)))
## amen.chunksize = sapply(((24 * 60 * 60) / amen.timing) * .75, floor)
amen.experiments[, chunk := chunk(job.id, chunk.size = 25)]

submitJobs(amen.experiments[chunk < 100], resources = resources)
getStatus(ids[ids$nodes != 50 & ids$problem == "bilinear.test", "job.id"])

reducer = function(x) {
  if (all(is.na(x)) | is.null(x)) {
    ret = list("estimate" = NA, "coverage" = NA, "loss" = NA)
  } else {
    adjustment = ifelse("glm" %in% class(x$fit) &
      x$data$latent_space == 1 & x$data$family == "binomial", x$adjustment, 1)
    ret = tryCatch(list("estimate" = x$estimate * adjustment), error = function(e) e)
    ret$coverage = tryCatch(unname(ifelse(x$interval[1] * adjustment < 0 &
      x$interval[2] * adjustment > 0, 1, 0)), error = function(e) e)
    ret$loss = tryCatch(ifelse(all(class(x$fit) == "ame"),
      ms_error(unstack_vector(predict.ame(x$fit, x$data), x$data$nodes), x$data$y_new),
      x$loss), error = function(e) e)
  }
  return(ret)
}

done = findDone()
target = makeRegistry(file.dir = "results", make.default = FALSE)
target$cluster.functions = makeClusterFunctionsTORQUE("template.tmpl")
batchMapResults(reducer, done, target = target)
batchExport(list(ms_error = ms_error, unstack_vector = unstack_vector,
  predict.ame = predict.ame, stack_matrix = stack_matrix), reg = target)
ids = getJobTable(reg = target)
ids[, chunk := chunk(job.id, n.chunks = 2)]
submitJobs(ids, reg = target, resources = resources)

res = reduceResultsList(reg = target, missing.val = list("estimate" = NA,
  "coverage" = NA, "loss" = NA))
## pars = getJobPars(done)
to_reset = ids[job.id %in% findErrors(reg = target)$job.id, ]
pars = getJobPars(done[!job.id %in% to_reset$"..id"])
res = rbindlist(res, fill = TRUE)
res = cbind(res, pars)
fwrite(res, "results_amen.csv")
