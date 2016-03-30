set.seed(1987)

library(clusterGeneration)
library(far)
library(foreach)
library(iterators)
library(doParallel)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)

cl <- makePSOCKcluster(6)
registerDoParallel(cl)

loss <- mse <- function(a, b) mean((a - b)^2)

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

families <- c("gaussian", "binomial", "poisson")
M <- 5
mc <- 100
n <- 100
p <- 2
sigma <- 1
res_eta <- 20
res_beta <- 20
r <- sort(runif(res_eta, 1, M))
beta <- lapply(seq_len(res_beta), function(x) runif(p, -1, 1))

sim <- foreach(f = families) %:% foreach(b = beta) %:% foreach(r = r, .packages = "clusterGeneration") %:%
  foreach(icount(mc)) %dopar% {
    m = cov2cor(genPositiveDefMat(p, "c-vine", eta = r)$Sigma)
    X <- t(t(chol(m)) %*% matrix(rnorm(n * p), p, n))
    lp <- X %*% b
    y <- switch(f,
                gaussian = rnorm(n, lp, sigma),
                binomial = rbinom(n, 1, 1 / (1 + exp(-(lp)))),
                poisson = rpois(n, exp(lp)))
    fit <- glm(y ~ -1 + X, family = f)
    list("beta" = coef(fit), "se" = sqrt(diag(vcov(fit))), "d" = diagnostic(fit), "max_r" = max(cor(X) - diag(p)),
         "mse" = mse(b, coef(fit)))
  }

est_r <- sapply(sim, function(f) sapply(f, function(b) sapply(b, function(m) do.call(c, lapply(m, function(x) x$max_r)))))
colnames(est_r) <- families
est_r <- as.data.frame(est_r)
est_r$eta <- rep(r, each = mc)
est_r <- melt(est_r, id.vars = "eta", variable.name = "family", value.name = "r")
ggplot(est_r, aes(r)) + geom_histogram() + facet_wrap( ~ eta) +
  labs(x = "maximum absolute value of off-diagonal elements")
ggsave("max_r_vine.png", width = 10, height = 6)

est_cn <- sapply(sim, function(f) sapply(f, function(b) sapply(b, function(m) do.call(c, lapply(m, function(x) max(x$d$ci))))))
colnames(est_cn) <- families
est_cn <- as.data.frame(est_cn)
est_cn$eta <- rep(r, each = mc)
est_cn <- melt(est_cn, id.vars = "eta", variable.name = "family", value.name = "icn")
ggplot(est_cn, aes(eta, icn)) +
  geom_smooth(method = "gam", formula = y ~ s(x), se = TRUE) +
  facet_wrap(~ family, scales = "free_y") +
  labs(x = expression(eta),
       y = expression(paste("condition number of ", I(beta))))
ggsave("glm_eta_icn.png", width = 10, height = 5)

est_mse <- sapply(sim, function(f) sapply(f, function(b) sapply(b, function(m) do.call(c, lapply(m, function(x) x$mse)))))
colnames(est_mse) <- families
est_mse <- as.data.frame(est_mse)
est_mse$eta <- rep(r, each = mc)
est_mse <- melt(est_mse, id.vars = "eta", variable.name = "family", value.name = "mse")
ggplot(est_mse, aes(eta, mse)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE) +
  facet_wrap(~ family, scales = "free_y") +
  labs(x = expression(eta), y = "MSE")
ggsave("glm_eta_mse.png", width = 10, height = 5)

est <- left_join(est_mse, est_cn, by = c("eta", "family"))
ggplot(est, aes(icn, mse)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE) +
  facet_wrap(~ family, scales = "free") +
  labs(x = expression(paste("condition number of ", I(beta))), y = "MSE")
ggsave("glm_icn_mse.png", width = 10, height = 5)

ggplot(est, aes(eta, mse)) + geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), se = TRUE) +
  facet_wrap(~ family, scales = "free") +
  labs(x = expression(eta), y = "MSE")
ggsave("glm_eta_mse.png", width = 10, height = 5)
