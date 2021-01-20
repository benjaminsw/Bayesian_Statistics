# ref: https://github.com/benjaminsw/jags_examples/blob/master/R%20Code/jags_logistic_regression.R

# Some boiler plate code to clear the workspace, and load in required packages
rm(list=ls()) # Clear the workspace
library(R2jags)
library(boot) # Package contains the logit transform

# Simulate data -----------------------------------------------------------

# Some R code to simulate data from the above model
T = 100
set.seed(123)
x_1 = sort(runif(T,0,10))
x_2 = sort(runif(T,0,10))
alpha = 1
beta_1 = 0.2
beta_2 = -0.5
logit_p = alpha + beta_1 * x_1 + beta_2 * x_2
p = inv.logit(logit_p)
y = rbinom(T,1,p)

# Have a quick look at the effect of x_1 and x_2 on y
plot(x_1,y)
plot(x_2,y) # Clearly when x is high y tends to be 0

# Jags code ---------------------------------------------------------------

# Jags code to fit the model to the simulated data
model_code = '
model
{
  # Likelihood
  for (t in 1:T) {
    y[t] ~ dbin(p[t], K)
    logit(p[t]) <- alpha + beta_1 * x_1[t] + beta_2 * x_2[t]
  }
  # Priors
  alpha ~ dnorm(0.0,0.01)
  beta_1 ~ dnorm(0.0,0.01)
  beta_2 ~ dnorm(0.0,0.01)
}
'

# Set up the data
model_data = list(T = T, y = y, x_1 = x_1, x_2 = x_2, K = 1)

# Choose the parameters to watch
model_parameters =  c("alpha", "beta_1", "beta_2")

# Run the model
model_run = jags(data = model_data,
                 parameters.to.save = model_parameters,
                 model.file = textConnection(model_code),
                 n.chains = 4,
                 n.iter = 1000,
                 n.burnin = 200,
                 n.thin = 2)

# Simulated results -------------------------------------------------------

# Check the output - are the true values inside the 95% CI?
# Also look at the R-hat values - they need to be close to 1 if convergence has been achieved
plot(model_run)
print(model_run)
traceplot(model_run)

# Create a plot of the posterior mean regression line
post = print(model_run)
alpha_mean = post$mean$alpha
beta_1_mean = post$mean$beta_1
beta_2_mean = post$mean$beta_2

# As we have two explanatory variables I'm going to create two plots
# holding one of the variables fixed whilst varying the other
par(mfrow = c(2, 1))
plot(x_1, y)
lines(x_1,
      inv.logit(alpha_mean + beta_1_mean * x_1 + beta_2_mean * mean(x_2)),
      col = 'red')
plot(x_2, y)
lines(x_2,
      inv.logit(alpha_mean + beta_1_mean * mean(x_1) + beta_2_mean * x_2),
      col = 'red')

# Line for x_1 should be increasing with x_1, and vice versa with x_2