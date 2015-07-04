########################### Metropolis-Hastings MCMC - linear regression Bayesian example
### from: https://theoreticalecology.wordpress.com/2010/09/17/metropolis-hastings-mcmc-in-r/

### Fabricate the sample data
true_slope <- 5
true_intercept <- 0
true_sd <- 10
sample_size <- 50

# create independent x-values
x <- (-(sample_size-1)/2):((sample_size-1)/2)

# create dependent values according to ax + b + N(0,sd)
set.seed(11111)
y <-  true_slope * x + true_intercept + rnorm(n = sample_size,
                                              mean = 0,
                                              sd = true_sd)

# Calculator of the likelihood function assuming linearity
# param is an array of [slope, intercept, st_deviation]

likelihood <- function(param) {
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

########## Example: plot the likelihood profile of the slope a
slope_values <- function(x) { return(likelihood(c(x, true_intercept, true_sd))) }
sd_values <- function(sd) { return(likelihood(c(true_slope, true_intercept, sd))) }
slope_likelihoods <- lapply(seq(3, 10, by = 0.05), slope_values)
sd_likelihoods <- lapply(seq(5, 25, by = 0.05), sd_values)
# plot (seq(3, 10, by = 0.05), slope_likelihoods , type="l", xlab = "values of slope parameter a", ylab = "Log likelihood")
# plot(seq(5, 25, by = 0.05), sd_likelihoods, type= "l", xlab = "values of sd parameter", ylab = "log likelihood")
##########


## Prior distribution
# note that we're getting the logarithms to avoid too small numbers multiplications
prior <- function(param){
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


## Posterior distribution
posterior <- function(param){
  return (likelihood(param) + prior(param))
}


######## Metropolis algorithm ################
proposal_function <- function(param){
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

run_metropolis_MCMC <- function(start_value, iterations) {
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

start_value <- c(4, 0.5, 9)
chain <- run_metropolis_MCMC(start_value, 10000)

burnIn <- 5000
acceptance <- 1 - mean(duplicated(chain[-(1:burnIn), ]))


### Summary: #######################
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

# for comparison:
summary(lm(y~x))
mean(chain[,1])
cat(paste("true slope: ", true_slope, "\n", "true"))

mean(chain[,2])
paste("true intercept: ", true_intercept)

mean(chain[,3])
paste("true sd: ", true_sd)

