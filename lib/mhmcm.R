########################### FUNCTIONS
######## Metropolis-Hastings MCMC - linear regression Bayesian example
### from: https://theoreticalecology.wordpress.com/2010/09/17/metropolis-hastings-mcmc-in-r/
#

#' Calculator of the likelihood function assuming linearity.
#'
#' @param param [array] Composed by slope, intercept, st_deviation
#' @return sum_likelihoods [number]
likelihood <- function (param) {
  slope <- param[1]
  intercept <- param[2]
  sd <- param[3]

  prediction = slope * x + intercept
  single_likelihoods <- dnorm(y,
                              mean = prediction,
                              sd = sd,
                              log = TRUE)
  sum_likelihoods <- sum(single_likelihoods)  # We can sum them because they're logs

  return(sum_likelihoods)
}

#' Prior distribution.
#' note that we're getting the logarithms to avoid too small numbers
#' multiplications.
#'
#' @param param [array] Composed by slope, intercept, st_deviation
#' @return [number]
prior <- function (param) {
  # change the names of the parameters
  slope <- param[1]
  intercept <- param[2]
  sd <- param[3]

  # slope prior distribution is assumed uniform between 0 and 10
  slope_prior <- dunif(slope,
                       min = 0,
                       max = 10,
                       log = TRUE)

  # intercept prior distribution is assumed normal with stdev 5
  intercept_prior <- dnorm(intercept,
                           sd = 5,
                           log = TRUE)

  # stdev prior is assumed uniform between 0 and 30
  sd_prior <- dunif(sd,
                    min = 0,
                    max = 30,
                    log = TRUE)

  return(slope_prior + intercept_prior + sd_prior)
}


#' Posterior distribution.
#' P(B) in P(A|B) = P(B|A)P(A)/P(B) is omitted
#' I'm assuming this is because we're using the ratio of P(A|B) and P(A'|B) and
#' P(B)s cancel each other.
#'
#' @param param [array] Composed by slope, intercept, st_deviation
#' @return [number] Returns a Real.
posterior <- function (param) {
  return (likelihood(param) + prior(param))
}


#' Metropolis algorithm
proposal_function <- function (param) {
  # define the standard deviation of the parameters' jump
  slope_sd <- 0.1
  intercept_sd <- 0.5
  sd_sd <- 0.3

  return(rnorm(3,
               mean = param,
               sd= c(slope_sd,
                     intercept_sd,
                     sd_sd)
  ))
}

run_metropolis_MCMC <- function(start_value,
                                iterations) {
  chain <- array(dim = c(iterations + 1, 3))
  chain[1, ] <- start_value
  for (i in 1:iterations){
    proposal <- proposal_function(chain[i, ])

    probab <- exp(posterior(proposal) - posterior(chain[i, ]))
    if (runif(1) < probab) {
      chain[i + 1, ] <- proposal
    } else {
      chain[i + 1, ] <- chain[i, ]
    }
  }
  return(chain)
}

#' Summary
summary_plot <- function(chain,
                         burnIn,
                         true_slope,
                         true_intercept,
                         true_sd) {
  par(mfrow = c(2,3))
  hist(chain[-(1:burnIn),1],nclass=30, , main="Posterior slope", xlab="True value = red line")
  abline(v = mean(chain[-(1:burnIn),1]))
  abline(v = true_slope, col="red" )
  hist(chain[-(1:burnIn),2],nclass=30, main="Posterior intercept", xlab="True value = red line")
  abline(v = mean(chain[-(1:burnIn),2]))
  abline(v = true_intercept, col="red" )
  hist(chain[-(1:burnIn),3],nclass=30, main="Posterior std deviation", xlab="True value = red line")
  abline(v = mean(chain[-(1:burnIn),3]) )
  abline(v = true_sd, col="red" )
  plot(chain[-(1:burnIn),1], type = "l", xlab="True value = red line" , main = "Chain values of slope", )
  abline(h = true_slope, col="red" )
  plot(chain[-(1:burnIn),2], type = "l", xlab="True value = red line" , main = "Chain values of intercept", )
  abline(h = true_intercept, col="red" )
  plot(chain[-(1:burnIn),3], type = "l", xlab="True value = red line" , main = "Chain values of std dev", )
  abline(h = true_sd, col="red" )
}
