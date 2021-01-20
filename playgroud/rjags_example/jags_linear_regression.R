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