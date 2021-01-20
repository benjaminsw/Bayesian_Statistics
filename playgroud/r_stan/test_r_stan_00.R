#ref: https://faculty.ucr.edu/~jflegal/203/STAN_tutorial.pdf
# https://www.r-bloggers.com/2019/01/an-introduction-to-stan-with-r/
# https://ourcodingclub.github.io/tutorials/stan-intro/

if (!require("pacman")) install.packages("pacman")
pacman::p_load(rstan)

set.seed(0)

eightschools <- "
//The data block reads external information.
data {
    int<lower=0> J;           // number of schools
    real  y[J];                // estimated treatment effects
    real<lower=0> sigma[J];   // s.e. of effect estimates
}
//The parameters block defines the sampling space.
parameters {
    real mu;                  // mean effect for schools
    real<lower=0> tau;        // variance
    real eta[J];              // individual school effect
}
//The transformed parameters block allows for parameter processing before the posterior is computed.
transformed parameters {    // theta is a function of our parameters
    real theta[J];for (j in 1:J)theta[j] = mu + tau * eta[j];
}
//In the model block we define our posterior distributions.
model {
    target += normal_lpdf(eta | 0, 1);        // eta ~ N(0,1)
    target += normal_lpdf(y | theta, sigma);  // y ~ N(theta, sigma^2), theta(mu, tau, eta)
}"


schools_dat <- list(
    J = 8,
    y = c(28,  8, -3,  7, -1,  1, 18, 12),
    sigma = c(15, 10, 16, 11,  9, 11, 10, 18)
)

fit <- stan(
      model_code = eightschools,
      data = schools_dat,
      iter = 10000,
      warmup = 100,
      chains = 4
)