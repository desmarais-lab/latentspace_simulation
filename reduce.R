library(BatchExperiments)
library(plyr) ## for rbind.fill

reg_open <- loadRegistry("reg")
reg_bruce <- loadRegistry("../lsm_bruce/reg")

reduce <- function(job, res) {
  if (!is.null(res)) {
    if (any(class(res$fit) == "glm") & res$data$latent_space == -1 & res$data$family == "binomial" &
          length(coef(res$fit)) < 3 & length(coef(res$fit)) <= 2) {
      estimate <- res$estimate * res$adjustment
      coverage <- unname(ifelse(res$interval[1] * res$adjustment < 0 & res$interval[2] * res$adjustment > 0, 1, 0))
    } else {
      estimate <- res$estimate
      coverage <- unname(ifelse(res$interval[1] < 0 & res$interval[2] > 0, 1, 0))
    }
    list("estimate" = estimate, "loss" = res$loss, "coverage" = coverage)
  } else {
    list("estimate" = NA, "loss" = NA, "coverage" = NA)
  }
}

res_reg_open <- batchMapQuick(reduceResultsExperiments,
                              chunk(findDone(reg_open), n.chunks = 80, shuffle = TRUE),
                              more.args = list(reg = reg_open, fun = reduce, progressbar = FALSE,
                                               impute.val = list("estimate" = NA, "loss" = NA, "coverage" = NA)),
                              resources = list(walltime = 86400L / 2, nodes = 1L, memory = "8gb"))
waitForJobs(res_reg_open)
res_open <- reduceResults(res_reg_open, fun = function(aggr, job, res) rbind.fill(aggr, res), init = data.frame())

res_reg_bruce <- batchMapQuick(reduceResultsExperiments,
                               chunk(findDone(reg_bruce), n.chunks = 200, shuffle = TRUE),
                               more.args = list(reg = reg_bruce, fun = reduce, progressbar = FALSE,
                                                impute.val = list("estimate" = NA, "loss" = NA, "coverage" = NA)),
                               resources = list(walltime = 86400L, nodes = 1L, memory = "8gb"))
waitForJobs(res_reg_bruce)
res_bruce <- reduceResults(res_reg_bruce, fun = function(aggr, job, res) rbind.fill(aggr, res), init = data.frame())
write.csv(rbind(res_open, res_bruce), "results.csv")
