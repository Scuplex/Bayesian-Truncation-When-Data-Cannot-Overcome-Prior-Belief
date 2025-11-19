T <- 60 # number of trials
theta <- 0.7 # true parameter value
y <- rbinom(T, size = 1, prob = theta) # simulate data
ST <- sum(y) # number of successes

x <- seq(0, 1, length.out = 400) # grid of theta values

prior_vals <- ifelse(x >= 0 & x <= 0.5, 8 * x, 0) # prior triangular

lik_vals <- dbeta(x, ST + 1, T - ST + 1) # likelihood (Beta)

# Compute normalising constant C using incomplete beta
a <- ST + 2
b <- (T - ST + 1)
z <- 0.5
C_partial <- pbeta(z, a, b) * beta(a, b)
C <- 8 * C_partial

# Compute posterior (analytic normalisation)
post_vals_analytic <- ifelse(x <= 0.5, (8 * x * x^ST * (1-x)^(T-ST)) / C, 0)

mle <- ST / T # maximum likelihood estimate for plotting purposes

# Plot prior and posterior
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))

# Plot prior
plot(x, prior_vals,
     type = "l",
     col = "red",
     lwd = 2,
     main = "Triangular Prior",
     xlab = expression(theta),
     ylab = "Density")

# Plot posterior (analytic normalization)
plot(x, post_vals_analytic,
     type = "l",
     col = "blue",
     lwd = 3,
     main = "Posterior",
     xlab = expression(theta),
     ylab = "Density")
abline(v = mle, col = "red", lwd = 2, lty = 3)
legend("topleft",
       legend = c("Posterior", "MLE"),
       col = c("blue", "red"),
       lty = c(1, 3),
       lwd = c(3, 2))
