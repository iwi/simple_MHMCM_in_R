################# Metropolis-Hastings MCMC - linear regression Bayesian example
# from:
# https://theoreticalecology.wordpress.com/2010/09/17/metropolis-hastings-mcmc-in-r/

source("./lib/mhmcm.R")

### Fabricate the sample data
true_slope <- 5
true_intercept <- 0
true_sd <- 10
sample_size <- 50

# create independent x-values
x <- (-(sample_size-1)/2):((sample_size-1)/2)

# create dependent values according to
# true_slope * x + true_intercept + N(0,sd)
set.seed(11111)
y <-  true_slope * x + true_intercept + rnorm(n = sample_size,
                                              mean = 0,
                                              sd = true_sd)

########## Example: plot the likelihood profile of the slope a

slope_values <- function(x) {
  return(likelihood(c(x, true_intercept, true_sd)))
  }
sd_values <- function(sd) {
  return(likelihood(c(true_slope, true_intercept, sd)))
  }
slope_likelihoods <- lapply(seq(3, 10, by = 0.05), slope_values)
sd_likelihoods <- lapply(seq(5, 25, by = 0.05), sd_values)
plot (seq(3, 10, by = 0.05),
      slope_likelihoods ,
      type="l",
      xlab = "values of slope parameter a",
      ylab = "Log likelihood")
plot(seq(5, 25, by = 0.05),
     sd_likelihoods,
     type= "l",
     xlab = "values of sd parameter",
     ylab = "log likelihood")

##########

# Define start value and iterations
start_value <- c(slope = 4,
                 intercept = 0.5,
                 sd=9)
number_of_iterations <- 10000
burnIn <- number_of_iterations / 2

# Run the Metropolis-Hastings process
chain <- run_metropolis_MCMC(start_value,
                             number_of_iterations)

acceptance <- 1 - mean(duplicated(chain[-(1:burnIn), ]))

summary_plot(chain,
             burnIn,
             true_slope,
             true_intercept,
             true_sd)

# for comparison:

freq_lm <- lm(y~x)
summary(freq_lm)
str(freq_lm)

cat(" ## slope \n",
    "true: ", true_slope, "\n",
    "MH-MCMC: ", mean(chain[-(1:burnIn), 1]), "\n",
    "lm: ", freq_lm$coefficients[2], "\n",
    "## intercept \n",
    "true: ", true_intercept, "\n",
    "MH-MCMC: ", mean(chain[-(1:burnIn), 1]), "\n",
    "lm: ", freq_lm$coefficients[1], "\n",
    "## sd \n",
    "true", true_sd, "\n",
    "MH-MCMC: ", mean(chain[-(1:burnIn), 3]), "\n",
    "lm: ", "\n")

mean(chain[-(1:burnIn), 2])
paste("true intercept: ", true_intercept)

mean(chain[-(1:burnIn), 3])
paste("true sd: ", true_sd)
