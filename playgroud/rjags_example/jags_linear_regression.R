# https://github.com/andrewcparnell/jags_examples/blob/master/R%20Code/jags_linear_regression.R

rm(list=ls()) # Clear the workspace
library(R2jags)

# Some R code to simulate data from the above model
n = 100
alpha = 2
beta = 3
sigma = 1
# Set the seed so this is repeatable
set.seed(123)
x = sort(runif(n, 0, 10)) # Sort as it makes the plotted lines neater
y = rnorm(n, mean = alpha + beta * x, sd = sigma)

# Also creat a plot
plot(x, y)
lines(x, alpha + beta * x)

# Jags code ---------------------------------------------------------------

# Jags code to fit the model to the simulated data

model_code = '
model
{
  # Likelihood
  for (i in 1:n) {
    y[i] ~ dnorm(alpha + beta * x[i], sigma^-2)
  }
  # Priors
  alpha ~ dnorm(0, 100^-2)
  beta ~ dnorm(0, 100^-2)
  sigma ~ dunif(0, 10)
}
'

# Set up the data
model_data = list(n = n, y = y, x = x)

# Choose the parameters to watch
model_parameters =  c("alpha", "beta", "sigma")

# Run the model
model_run = jags(data = model_data,
                 parameters.to.save = model_parameters,
                 model.file=textConnection(model_code),
                 n.chains=4, # Number of different starting positions
                 n.iter=1000, # Number of iterations
                 n.burnin=200, # Number of iterations to remove at start
                 n.thin=2) # Amount of thinning


# Simulated results -------------------------------------------------------

# Check the output - are the true values inside the 95% CI?
# Also look at the R-hat values - they need to be close to 1 if convergence has been achieved
plot(model_run)
print(model_run)
traceplot(model_run)

# Create a plot of the posterior mean regression line
post = print(model_run)
alpha_mean = post$mean$alpha[1]
beta_mean = post$mean$beta[1]

plot(x, y)
lines(x, alpha_mean + beta_mean * x, col = 'red')
lines(x, alpha + beta * x, col = 'blue')
legend('topleft',
       legend = c('Truth', 'Posterior mean'),
       lty=1,
       col=c('blue','red'))
# Blue and red lines should be pretty close
