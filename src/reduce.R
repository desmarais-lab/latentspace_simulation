library("BatchExperiments")
library("plyr") ## for rbind.fill

reg <- loadRegistry("reg")

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
    if (!any(class(res$fit) == "glm")) {
      iter <- nrow(res$fit$sample$beta)
    } else {
      iter <- NA
    }
    list("estimate" = estimate, "loss" = res$loss, "coverage" = coverage, "iter" = iter)
  } else {
    list("estimate" = NA, "loss" = NA, "coverage" = NA, "iter" = NA)
  }
}

res <- reduceResultsExperiments(reg, fun = reduce, progressbar = FALSE)
write.csv(res, "results.csv")
