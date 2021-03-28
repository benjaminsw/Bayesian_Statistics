# Appendix: R code for the Binomial data example
yi <- c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0)
n <- length(yi)
theta <- seq(0.0001, 0.9999, length=100)
# Analysis with the Un(0,1) prior
# prior 1:  Un(0,1)
par(mar=c(3,2,2,1)+0.1)
alpha1 <- beta1 <- 1
par(mfrow=c(4,4))
y <- 0
dens <- dbeta(theta, alpha1, beta1)
plot(theta, dens, type="l", ylim=c(0,6), ylab="density")
title(main=paste("n=", 0, "; y=", 0, sep=""))
for (i in (1:n))
{
  y <- y + yi[i]
  dens <- dbeta(theta, alpha1+y, beta1+i-y)
  plot(theta, dens, type="l", ylim=c(0,6), ylab="density")
  if(yi[i] == 1) out <- "S" else out <- "F"
  title(main=paste("  n=", i, "; y=", y, "   ", out, sep=""))
}

# Exact 95% Posterior interval in (lower, upper)
par(mfrow=c(2,1), mar=c(3,2,2,1)+0.1)
y <- sum(yi)
alphaU <- alpha1 + y
betaU <- beta1 + n - y
dens <- dbeta(theta, alphaU, betaU)
plot(theta, dens, type="l", ylab="density")
title(main=paste("Posterior distribution of theta: Beta(",
        alphaU, ",", betaU, ")", sep=""))
modtheta <- (alphaU - 1) / (alphaU + betaU - 2)
lines(rep(modtheta, 2), c(0, 0.2))
lower <- qbeta(p=0.025, alphaU, betaU)
upper <- qbeta(p=0.975, alphaU, betaU)
lines(c(lower, upper), rep(0, 2))
#> c(lower, upper)
#[1] 0.04047373 0.38347624
# Summaries of the posterior using simulation
set.seed(432)                     # set seed to be able to reproduce results
nsamp <- 10000
draws <- rbeta(nsamp, alphaU, betaU)
hist(draws, breaks=seq(0, 1, by=0.01), prob=T, main="")
title(main=paste(nsamp, " draws from a Beta(",
        alphaU, ",", betaU, ") distribution", sep=""))
sorted <- sort(draws)
lower95 <- sorted[nsamp * 0.025]
upper95 <- sorted[nsamp * 0.975 + 1]
lower50 <- sorted[nsamp * 0.25]
upper50 <- sorted[nsamp * 0.75 + 1]
med <- median(sorted)

lines(c(lower95, upper95), rep(0, 2), lwd=2)
lines(c(lower50, upper50), rep(0, 2), lwd=4)
points(med, 0, pch=16, cex=1)
#> c(lower95, upper95)
#[1] 0.03951703 0.38566722
# Comparison of the analyses with the 3 priors
# prior 2: alpha, beta s.t. E(theta)=0.5, sqrt(Var(theta)) = 0.2
alpha2 <- beta2 <- 2.625
# prior 3: Jeffrey’s prior
alpha3 <- beta3 <- 0.5
par(mfrow=c(4,4), mar=c(3,2,2,1)+0.1)
y <- 0
dens <- cbind(dbeta(theta, alpha1, beta1),
              dbeta(theta, alpha2, beta2),
              dbeta(theta, alpha3, beta3))
matplot(theta, dens, type="l", lty=c(1,2,4), col=1, ylim=c(0,6),
        ylab="density")
title(main=paste("n=", 0, "; y=", 0, sep=""))
for (i in (1:n))
{
  y <- y + yi[i]
  dens <- cbind(dbeta(theta, alpha1+y, beta1+i-y),
                dbeta(theta, alpha2+y, beta2+i-y),
                dbeta(theta, alpha3+y, beta3+i-y))
  matplot(theta, dens, type="l", lty=c(1,2,4), col=1, ylim=c(0,6),
          ylab="density")
  if(yi[i] == 1) out <- "S" else out <- "F"
  title(main=paste("  n=", i, "; y=", y, "      ", out, sep=""))
}