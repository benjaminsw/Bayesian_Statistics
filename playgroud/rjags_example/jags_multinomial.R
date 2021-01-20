# ref: https://github.com/benjaminsw/jags_examples/blob/master/R%20Code/jags_multinomial.R

rm(list=ls()) # Clear the workspace
library(R2jags)
# Simulate data -----------------------------------------------------------

# Some R code to simulate data from the above model
N = 500 # Number of observations
M = 3 # Number of categories
S = rpois(N, 20) + 1 # Sum of values (different for each observation)
K = 2 # Number of covariates

softmax = function(x) exp(x)/sum(exp(x))

beta = matrix(rnorm(M*K), nrow = M, ncol = K)
x = matrix(rnorm(N*K), nrow = N, ncol = K)
p = y = matrix(NA, nrow = N, ncol = M)
for(i in 1:N) {
  p[i,] = softmax(beta%*%x[i,])
  y[i,] = rmultinom(1, size = S[i], prob = p[i,])
}
# Jags code ---------------------------------------------------------------

# Jags code to fit the model to the simulated data
model_code = '
model
{
  # Likelihood
  for (i in 1:N) { # Observaton loops
    y[i,] ~ dmulti(p[i,], S[i])
    for(j in 1:M) { # Category loop
      exp_z[i,j] <- exp(z[i,j])
      p[i,j] <- exp_z[i,j]/sum(exp_z[i,])
      z[i,j] <- beta[j,]%*%x[i,]
    }
  }
  # Prior
  for(j in 1:M) {
    for(k in 1:K) {
      beta[j,k] ~ dnorm(0, 0.1^-2)
    }
  }
}
'

# Set up the data
model_data = list(N = N, y = y, x = x, S = S, K = K, M = M)

# Choose the parameters to watch
model_parameters =  c("beta", "p")

# Run the model
model_run = jags(data = model_data,
                 parameters.to.save = model_parameters,
                 model.file = textConnection(model_code))

# Simulated results -------------------------------------------------------

# Results and output of the simulated example, to include convergence checking, output plots, interpretation etc
plot(model_run)
print(model_run)

# Compare the predicted vs true values of beta
model_run$BUGSoutput$mean$beta
beta

# However you're better off lookin at the predicted probabilities as these
# will be more directly comparable
p_pred = model_run$BUGSoutput$mean$p
head(cbind(p[,1], p_pred[,1]), 20)