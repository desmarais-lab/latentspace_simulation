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
exclusions = rbind(findOnSystem(), findDone())
ids[, chunk := chunk(job.id, chunk.size = 100), by = "problem"]
ids[, chunk := .GRP, by = c("problem", "chunk")]
chunks.to.submit = sort(unique(ids[ids$problem == "bilinear.test", chunk]))
submitJobs(ids[ids$chunk %in% chunks.to.submit & !ids$job.id %in% exclusions$job.id & ids$nodes != 50, ], resources = resources)
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
    ret$loss = tryCatch(x$loss, error = function(e) e)
  }
  return(ret)
}

done = findDone()
target = makeRegistry(file.dir = "results", make.default = FALSE)
target$cluster.functions = makeClusterFunctionsTORQUE("template.tmpl")
batchMapResults(reducer, done, target = target)
ids = getJobTable(reg = target)
ids[, chunk := chunk(job.id, n.chunks = 50)]
ids[, chunk := .GRP, by = "chunk"]
submitJobs(ids, reg = target, resources = resources)

res = reduceResultsList(reg = target, missing.val = list("estimate" = NA,
  "coverage" = NA, "loss" = NA))
res = rbindlist(res, fill = TRUE)
res = cbind(res, getJobPars(done))
fwrite(res, "results_amen.csv")
