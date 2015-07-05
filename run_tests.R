# Run tests

library(testthat)
source("detailed_MHMCMC_functions.R")
test_dir("~/projectes/aprenc_bayesian_MCMC/simple_MHMCM_in_R/",
         reporter = "summary")
