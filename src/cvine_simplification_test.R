# this script tests whether a single draw from a beta distribution
# is equivalent to the c-vine method for generating correlations in 2 dimensions
require(clusterGeneration)

# dimension is always 2
dim = 2

# change eta to re-run test
eta = 2

# required for replicating genPositiveDefMat()
rangeVar = c(1, 10)

# assure same seed for both routines
set.seed(123)

## begin unpacking of 'c-vine' based generation with dim = 2 ##

# generate correlation from beta
rho <- 2 * rbeta(1, eta, eta) - 1

# reshape to a correlation matrix
rr <- matrix(c(1, rho, rho, 1), 2, 2)

# create random matrix by which to scale the covariance 
sigma2 <- runif(dim, min = rangeVar[1], max = rangeVar[2])

# extract diagonal, to be used as standard deviations
dd <- diag(sqrt(sigma2))

# scale correlation to get covariance
Sigma <- dd %*% rr %*% dd

# convert back to correlation matrix
cor.unpacked <- cov2cor(Sigma)

## replicating with clusterGeneration ##
set.seed(123)
Sigma.gpdm <- genPositiveDefMat(d=dim,"c-vine",eta=eta)$Sigma
cor.gpdm <- cov2cor(Sigma.gpdm)

## test equivalence ##
all((c(round(cor.unpacked,10))==c(round(rr,10))) & (c(round(cor.gpdm,10))==c(round(rr,10))))




