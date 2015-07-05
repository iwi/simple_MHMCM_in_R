# Suite of tests for the Metropolis-Hastings MCMC functions

context("mhmcmc.R")

describe("likelihood", {
  it("should return a numeric outcome", {
    expect_is(likelihood(c(1, 2, 3)), class = "numeric")
  })
})
