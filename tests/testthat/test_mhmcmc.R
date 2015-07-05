# Suite of tests for the Metropolis-Hastings MCMC functions

context("mhmcmc.R")

describe("likelihood", {
  it("should return a numeric outcome", {
    set.seed(1)
    x <- seq(-100, 100, by = 1)
    y <- 1 * x + 2 + rnorm(0, 3)
    expect_is(likelihood(c(1, 2, 3), y, x), class = "numeric")
  })
})
