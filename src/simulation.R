library("BatchExperiments")
library("latentnet")
library("foreach")
library("clusterGeneration")
library("iterators")

create_network <- function(y, name = "eweight") {
  library("network")
  net <- network(y, directed = FALSE)
  el <- as.matrix(net, "edgelist")
  w <- y[el]
  set.edge.attribute(net, name, w)
  net
}

rmvnorm_d <- function(sigma, d) {
  library("mvtnorm")
  
  var_d <- var(d)
  mu_d <- mean(d)
  n_var <- ncol(sigma)
  d_inv <- solve(as.matrix(var_d))
  sigma_x <- sigma[1:(n_var - 1), 1:(n_var - 1)] - sigma[1:(n_var - 1), n_var] %*%
    d_inv %*% sigma[n_var, 1:(n_var - 1)]
  mu_x <- sapply(d, function(z) sigma[1:(n_var - 1), n_var] %*% d_inv %*% z - mu_d)
  if (!is.matrix(mu_x))
    as.matrix(sapply(mu_x, rnorm, n = 1, sd = sqrt(sigma_x)))
  else
    t(apply(mu_x, 2, function(mu) rmvnorm(1, mu, sigma_x)))
}

ms_error <- function(x, y, each_column = FALSE) {
  if (each_column)
    sapply((x - y)^2, mean)
  else
    mean((x - y)^2)
}

unstack_vector <- function(x, n) {
  idx <- t(combn(1:n, 2))
  mat <- diag(n)
  mat[idx] <- x
  mat[idx[, c(2, 1)]] <- x
  stopifnot(isSymmetric(mat))
  mat
}

diagnostic <- function(fit) {
  stopifnot(any(class(fit) %in% "glm"))
  SVD <- svd(scale(solve(vcov(fit)), center = FALSE))
  max_d <- max(SVD$d)
  CI <- sapply(SVD$d, function(x) max_d / x)
  phi <- t((SVD$v %*% diag(1 / SVD$d))^2)
  pi <- prop.table(phi, 2)
  out <- data.frame(CI, pi)
  colnames(out) <- c("ci", names(coef(fit)))
  round(out, 3)
}

create_data <- function(static, family, eta, nodes, beta, latent_space) {
  library("clusterGeneration")

  C <- abs(cov2cor(genPositiveDefMat(static$p, "c-vine", eta = eta)$Sigma))
  d <- as.matrix(dist(replicate(static$p, rnorm(nodes, 0, static$sigma))))
  d <- d[lower.tri(d)]
  d <- d / (sd(d) / sqrt(diag(C)[nrow(C)]))
  X <- rmvnorm_d(C, d)
  X <- (X - colMeans(X)) / apply(X, 2, sd)
  lp <- beta * X + latent_space * d
  y_stack <- switch(family,
                    gaussian = rnorm(length(lp), lp, static$sigma),
                    binomial = rbinom(length(lp), 1, 1 / (1 + exp(-(lp)))),
                    poisson = rpois(length(lp), exp(lp)))
  y <- unstack_vector(y_stack, nodes)
  X_stack <- data.frame(X)
  X <- unstack_vector(X, nodes)
  y_stack_new <- switch(family,
                        gaussian = rnorm(length(lp), lp, static$sigma),
                        binomial = rbinom(length(lp), 1, 1 / (1 + exp(-(lp)))),
                        poisson = rpois(length(lp), exp(lp)))
  y_new <- unstack_vector(y_stack_new, nodes)
  ret <- list(
    p = if (family == "binomial") plogis(lp) else NULL,
    d = d,
    y = y,
    y_stack = y_stack,
    X = X,
    X_stack = X_stack,
    y_new = y_new,
    y_stack_new = y_stack_new,
    family = family,
    nodes = nodes,
    beta = beta,
    latent_space = latent_space
  )
  ret
}

lsm.wrapper <- function(static, dynamic, ...) {
  library("latentnet")
  library("coda")

  dots <- list(...)

  net <- create_network(dynamic$y)
  .GlobalEnv$X <- dynamic$X ## fuck you latentet

  lf <- c("gaussian" = "Gaussian", "binomial" = "Bernoulli", "poisson" = "Poisson")

  if (dots$scale) {
    glm_fit <- glm(y ~ X, dynamic$family, data.frame(dynamic$X_stack, "y" = dynamic$y_stack))
    prior_variance <- (abs(coef(glm_fit)[2]) * var(dynamic$X_stack)) / (2 - 4 / pi)
    prior_variance <- unname(as.numeric(prior_variance))
    prior <- ergmm.prior("Z.var" = prior_variance)
  } else {
    prior <- ergmm.prior(...)
  }

  fam.par <- list("prior.var" = static$sigma, "prior.var.df" = dynamic$nodes)
  if (dynamic$family != "gaussian") fam.par <- NULL

  flag <- TRUE
  iter <- 1
  while (flag) {
    fit <- ergmm(net ~ edgecov(X) + euclidean(d = 1),
                 "eweight", unname(lf[names(lf) %in% dynamic$family]),
                 fam.par = fam.par, prior = prior,
                 tofit = c("mcmc", "pmode"), seed = static$seed,
                 control = static$control)
    ## assess convergence, re-run if necessary
    geweke <- geweke.diag(as.mcmc(fit)[[1]])$z
    if (abs(unname(geweke[["lpY"]])) < 1 | iter == static$maxit) {
      flag <- FALSE
    } else {
      iter <- iter + 1
      static$control$sample.size <- static$control$sample.size * 2
    }
    flag <- FALSE
  }

  pred <- predict(fit, newdata = create_network(dynamic$y_new, dynamic$X), type = "pmode")
  
  list(
    "pred" = pred,
    "estimate" = fit$mcmc.pmode$beta[2],
    "interval" = HPDinterval(as.mcmc(fit$sample$beta[, 2])),
    "loss" = ms_error(pred, dynamic$y_new),
    "convergence" = geweke,
    "data" = dynamic,
    "fit" = fit
  )
}

glm.wrapper <- function(static, dynamic, ...) {
  fit <- glm(y ~ X, dynamic$family, data.frame(dynamic$X_stack, "y" = dynamic$y_stack))
  pred <- predict(fit, newdata = dynamic$X_stack, se.fit = TRUE, type = "response")
  list(
    "pred" = pred,
    "estimate" = unname(coef(fit)[2]),
    "adjustment" = sqrt(3.29 + var(dynamic$d)) / sqrt(3.29),
    "interval" = confint(fit, colnames(dynamic$X_stack)),
    "loss" = ms_error(pred$fit, dynamic$y_stack_new),
    "data" = dynamic,
    "fit" = fit
  )
}

truth.wrapper <- function(static, dynamic, ...) {
  fit <- glm(y ~ X + d, dynamic$family, data.frame(dynamic$X_stack, "y" = dynamic$y_stack, "d" = dynamic$d))
  pred <- predict(fit, newdata = data.frame(dynamic$X_stack, "d" = dynamic$d), se.fit = TRUE, type = "response")
  list(
    "pred" = pred,
    "estimate" = unname(coef(fit)[2]),
    "adjustment" = sqrt(3.29 + var(dynamic$d)) / sqrt(3.29),
    "interval" = confint(fit, colnames(dynamic$X_stack)),
    "loss" = ms_error(pred$fit, dynamic$y_stack_new),
    "data" = dynamic,
    "fit" = fit
  )
}

reg <- makeExperimentRegistry("lsm", "reg")

addProblem(reg, "test", dynamic = create_data, seed = 1234, overwrite = TRUE,
           static = list(p = 2, sigma = 1, seed = 1234, maxit = 2,
                         control = ergmm.control(sample.size = 10000, burnin = 10000, interval = 100)))

problem.pars <- list(
  "eta" = c(.1, 1, 100, 1000000),
  "family" = c("gaussian", "binomial", "poisson"),
  "nodes" = c(25, 50, 100),
  "beta" = c(1, 0),
  "latent_space" = c(-1, 0)
)

problem.design <- makeDesign("test", exhaustive = problem.pars)

lsm.pars <- list("scale" = c(TRUE, FALSE),
                 "beta.var" = c(1, 10))
glm.pars <- list()
truth.pars <- list()

lsm.design <- makeDesign("lsm", exhaustive = lsm.pars)
glm.design <- makeDesign("glm", exhaustive = glm.pars)
truth.design <- makeDesign("truth", exhaustive = truth.pars)

addAlgorithm(reg, "lsm", lsm.wrapper, overwrite = TRUE)
addAlgorithm(reg, "glm", glm.wrapper, overwrite = TRUE)
addAlgorithm(reg, "truth", truth.wrapper, overwrite = TRUE)

addExperiments(reg, problem.design, list("lsm" = lsm.design, "glm" = glm.design, "truth" = truth.design),
               repls = 500, skip.defined = TRUE)
batchExport(reg, create_network = create_network, ms_error = ms_error,
            rmvnorm_d = rmvnorm_d, unstack_vector = unstack_vector, diagnostic = diagnostic,
            overwrite = TRUE)

## generate a histogram plot for the off diagnoals of the covariance matrix used to generate
## d and X_stack
eta_hist <- foreach(eta = c(1, 100, 1000000), .combine = "rbind") %:%
  foreach(icount(1000), .combine = "rbind") %do% {
  c(eta, abs(cov2cor(genPositiveDefMat(2, "c-vine", eta = eta)$Sigma))[1, 2])
}

eta_hist <- data.frame(eta_hist)
colnames(eta_hist) <- c("eta", "cor")
p <- ggplot(eta_hist, aes(x = cor))
p <- p + geom_histogram()
p <- p + facet_wrap(~ eta, nrow = 1)
p <- p + labs(x = "off-diagonal correlation", y = "count (mc = 1000)",
              title = "distribution of correlations")
p <- p + theme_bw()
ggsave("max_r_vine.png", width = 8, height = 3)
