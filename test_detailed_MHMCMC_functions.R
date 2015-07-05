# suite of tests for the Metropolis-Hastings MCMC functions
# detailed_MHMCMC_functions.R

context("basic tests")

describe("likelihood", {
  it("should return a numeric outcome", {
    expect_is(likelihood(c(1, 2, 3)), class = "numeric")
  })
})