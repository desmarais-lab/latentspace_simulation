pkgs = c("batchtools", "latentnet", "amen", "foreach", "clusterGeneration",
  "iterators", "network", "mvtnorm", "coda")
invisible(lapply(pkgs, library, character.only = TRUE))

create_network = function(y, name = "eweight") {
  net = network(y, directed = FALSE)
  el = as.matrix(net, "edgelist")
  w = y[el]
  set.edge.attribute(net, name, w)
  net
}

rmvnorm_d = function(sigma, d) {
  var_d = var(d)
  mu_d = mean(d)
  n_var = ncol(sigma)
  d_inv = solve(as.matrix(var_d))
  sigma_x = sigma[1:(n_var - 1), 1:(n_var - 1)] - sigma[1:(n_var - 1), n_var] %*%
    d_inv %*% sigma[n_var, 1:(n_var - 1)]
  mu_x = sapply(d, function(z) sigma[1:(n_var - 1), n_var] %*% d_inv %*% z - mu_d)
  if (!is.matrix(mu_x))
    as.matrix(sapply(mu_x, rnorm, n = 1, sd = sqrt(sigma_x)))
  else
    t(apply(mu_x, 2, function(mu) rmvnorm(1, mu, sigma_x)))
}

ms_error = function(x, y, each_column = FALSE) {
  if (each_column)
    sapply((x - y)^2, mean)
  else
    mean((x - y)^2)
}

unstack_vector = function(x, n) {
  idx = t(combn(1:n, 2))
  mat = diag(n)
  mat[idx] = x
  mat[idx[, c(2, 1)]] = x
  stopifnot(isSymmetric(mat))
  mat
}

stack_matrix = function(x) x[lower.tri(x)]

diagnostic = function(fit) {
  stopifnot(any(class(fit) %in% "glm"))
  SVD = svd(scale(solve(vcov(fit)), center = FALSE))
  max_d = max(SVD$d)
  CI = sapply(SVD$d, function(x) max_d / x)
  phi = t((SVD$v %*% diag(1 / SVD$d))^2)
  pi = prop.table(phi, 2)
  out = data.frame(CI, pi)
  colnames(out) = c("ci", names(coef(fit)))
  round(out, 3)
}

create_data = function(data, job, p, eta, nodes, family, beta, latent_space, ...) {
  C = abs(cov2cor(genPositiveDefMat(data$p, "c-vine", eta = eta)$Sigma))
  if (data$bilinear) {
    u = rnorm(nodes, 0, data$sigma)
    ## v = rnorm(nodes, 0, data$sigma)
    d = u %o% u ## symmetric
  } else {
    d = as.matrix(dist(replicate(data$p, rnorm(nodes, 0, data$sigma))))
  }

  d = d[lower.tri(d)]
  d = d / (sd(d) / sqrt(diag(C)[nrow(C)]))
  X = rmvnorm_d(C, d)
  X = (X - colMeans(X)) / apply(X, 2, sd)
  lp = beta * X + latent_space * d
  y_stack = switch(family,
                    gaussian = rnorm(length(lp), lp, data$sigma),
                    binomial = rbinom(length(lp), 1, 1 / (1 + exp(-(lp)))),
                    poisson = rpois(length(lp), exp(lp)))
  y = unstack_vector(y_stack, nodes)
  X_stack = data.frame(X)
  X = unstack_vector(X, nodes)
  y_stack_new = switch(family,
                        gaussian = rnorm(length(lp), lp, data$sigma),
                        binomial = rbinom(length(lp), 1, 1 / (1 + exp(-(lp)))),
                        poisson = rpois(length(lp), exp(lp)))
  y_new = unstack_vector(y_stack_new, nodes)
  ret = list(
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
    latent_space = latent_space,
    bilinear = data$bilinear
  )
  ret
}

amen.wrapper = function(data, job, instance, ...) {
  dots = list(...)
  lf = c("gaussian" = "nrm", "binomial" = "bin")

  flag = TRUE
  iter = 1

  while (flag) {
    fit = ame(instance$y, instance$X, R = 1,
      rvar = FALSE, cvar = FALSE, dcor = FALSE, nvar = FALSE,
      symmetric = TRUE, plot = FALSE, model = unname(lf[names(lf) %in% instance$family]),
      seed = data$seed, print = FALSE,
      nscan = data$ame_mcmc, odens = data$thin)

    raft = as.numeric(max(raftery.diag(as.mcmc(fit$BETA))$resmatrix[, 2]))
    if (data$ame_mcmc > raft | iter == data$maxit) {
      flag = FALSE
    } else {
      iter = iter + 1
      data$ame_mcmc = raft * data$thin
    }
  }

  pred = colMeans(fit$BETA %*% t(as.matrix(cbind(1, instance$X_stack)))) +
    stack_matrix(fit$U %*% t(fit$U))

  list(
    "pred" = pred,
    "estimate" = mean(fit$BETA[, 2]),
    "interval" = HPDinterval(as.mcmc(fit$BETA[, 2])),
    ## "loss" = ms_error(unstack_vector(pred, instance$nodes), instance$y_new),
    "loss" = ms_error(pred, instance$y_stack_new),
    "convergence" = raft,
    "data" = instance,
    "fit" = fit
  )
  
}

lsm.wrapper = function(data, job, instance, ...) {
  dots = list(...)
  net = create_network(instance$y)
  .GlobalEnv$X = instance$X ## fuck you latentet

  lf = c("gaussian" = "Gaussian", "binomial" = "Bernoulli", "poisson" = "Poisson")

  fam.par = list("prior.var" = data$sigma, "prior.var.df" = instance$nodes)
  if (instance$family != "gaussian") fam.par = NULL

  flag = TRUE
  iter = 1
  formula = paste0("net ~ edgecov(X)")
  while (flag) {
    if (instance$bilinear)
      form = paste0(formula, "+ bilinear(d = 1)")
    else
      form = paste0(formula, "+ euclidean(d = 1)")
    system.time(fit = ergmm(as.formula(form),
                 "eweight", unname(lf[names(lf) %in% instance$family]),
                 fam.par = fam.par, prior = ergmm.prior(),
                 tofit = c("mcmc", "pmode"), seed = data$seed,
                 control = data$control))
    ## assess convergence, re-run if necessary
    geweke = geweke.diag(as.mcmc(fit)[[1]])$z
    if (abs(unname(geweke[["lpY"]])) < 1 | iter == data$maxit) {
      flag = FALSE
    } else {
      iter = iter + 1
      data$control$sample.size = data$control$sample.size * 2
    }
    flag = FALSE
  }

  pred = predict(fit, newdata = create_network(instance$y_new, instance$X), type = "pmode")
  
  list(
    "pred" = pred,
    "estimate" = fit$mcmc.pmode$beta[2],
    "interval" = HPDinterval(as.mcmc(fit$sample$beta[, 2])),
    "loss" = ms_error(pred, instance$y_new),
    "convergence" = geweke,
    "data" = instance,
    "fit" = fit
  )
}

glm.wrapper = function(data, job, instance, ...) {
  fit = glm(y ~ X, instance$family, data.frame(instance$X_stack, "y" = instance$y_stack))
  pred = predict(fit, newdata = instance$X_stack, se.fit = TRUE, type = "response")
  list(
    "pred" = pred,
    "estimate" = unname(coef(fit)[2]),
    "adjustment" = sqrt(3.29 + var(instance$d)) / sqrt(3.29),
    "interval" = confint(fit, colnames(instance$X_stack)),
    "loss" = ms_error(pred$fit, instance$y_stack_new),
    "data" = instance,
    "fit" = fit
  )
}

truth.wrapper = function(data, job, instance, ...) {
  fit = glm(y ~ X + d, instance$family, data.frame(instance$X_stack, "y" = instance$y_stack, "d" = instance$d))
  pred = predict(fit, newdata = data.frame(instance$X_stack, "d" = instance$d),
    se.fit = TRUE, type = "response")
  list(
    "pred" = pred,
    "estimate" = unname(coef(fit)[2]),
    "adjustment" = sqrt(3.29 + var(instance$d)) / sqrt(3.29),
    "interval" = confint(fit, colnames(instance$X_stack)),
    "loss" = ms_error(pred$fit, instance$y_stack_new),
    "data" = job,
    "fit" = fit
  )
}

reg = makeExperimentRegistry("lsm", packages = pkgs)

addProblem("euclidean.test", fun = create_data, seed = 1234,
  data = list(p = 2, sigma = 1, seed = 1234, maxit = 2,
    control = ergmm.control(sample.size = 10000, burnin = 10000, interval = 100),
    bilinear = FALSE))

addProblem("bilinear.test", fun = create_data, seed = 1234,
  data = list(p = 2, sigma = 1, seed = 1234, maxit = 2, bilinear = TRUE, ame_mcmc = 100000,
    thin = 25))

euclidean.pars = list(
  "eta" = c(.1, 1, 100, 1000000),
  "family" = c("gaussian", "binomial", "poisson"),
  "nodes" = c(25, 50, 100),
  "beta" = c(1, 0),
  "latent_space" = c(-1, 0)
)

bilinear.pars = list(
  "eta" = c(.1, 1, 100, 1000000),
  "family" = c("gaussian", "binomial"),
  "nodes" = c(25, 50, 100),
  "beta" = c(1, 0),
  "latent_space" = c(1, 0)
)

addAlgorithm("lsm", lsm.wrapper)
addAlgorithm("amen", amen.wrapper)
addAlgorithm("glm", glm.wrapper)
addAlgorithm("truth", truth.wrapper)

addExperiments(list("euclidean.test" = expand.grid(euclidean.pars, stringsAsFactors = FALSE)),
  list("lsm" = data.frame("beta.var" = c(1, 10)),
    "glm" = data.frame(), "truth" = data.frame()), 500)

addExperiments(list("bilinear.test" = expand.grid(bilinear.pars, stringsAsFactors = FALSE)),
  list("amen" = data.frame(), "glm" = data.frame(), "truth" = data.frame()), 500)

batchExport(list(create_network = create_network, ms_error = ms_error,
  rmvnorm_d = rmvnorm_d, unstack_vector = unstack_vector, diagnostic = diagnostic,
  stack_matrix = stack_matrix))

## generate a histogram plot for the off diagnoals of the covariance matrix used to generate
## d and X_stack
eta_hist = foreach(eta = c(1, 100, 1000000), .combine = "rbind") %:%
  foreach(icount(1000), .combine = "rbind") %do% {
  c(eta, abs(cov2cor(genPositiveDefMat(2, "c-vine", eta = eta)$Sigma))[1, 2])
}

eta_hist = data.frame(eta_hist)
colnames(eta_hist) = c("eta", "cor")
p = ggplot(eta_hist, aes(x = cor))
p = p + geom_histogram()
p = p + facet_wrap(~ eta, nrow = 1)
p = p + labs(x = "off-diagonal correlation", y = "count (mc = 1000)",
              title = "distribution of correlations")
p = p + theme_bw()
ggsave("max_r_vine.png", width = 8, height = 3)
