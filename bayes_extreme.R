T <- 6 # number of trials
theta <- 0.7 # true parameter value
y <- rbinom(T, size = 1, prob = theta) # simulate data
ST <- sum(y) # number of successes

x <- seq(0, 1, length.out = 400) # grid of theta values

prior_vals <- ifelse(x >= 0 & x <= 0.5, 8 * x, 0) # prior triangular

lik_vals <- dbeta(x, ST + 1, T - ST + 1) # likelihood (Beta)

post_vals <- prior_vals * lik_vals # unnormalized posterior
 
post_vals <- post_vals / sum(post_vals) # normalize posterior using sum instead of integrate due to discrete grid(400) Riemann sum approximation :)

mle <- ST / T # maximum likelihood estimate for plotting purposes



# Plot prior and posterior
plot(x, post_vals,
     type = "l",
     col = "blue",
     lwd = 3,
     ylim = c(0, max(post_vals)),
     main = "Triangular Prior and Posterior",
     xlab = expression(theta),
     ylab = "Density")

lines(x, prior_vals / max(prior_vals) * max(post_vals),
      col = "black", lty = 2, lwd = 2)

abline(v = mle, col = "red", lwd = 2, lty = 3)

legend("topleft",
       legend = c("Posterior", "Prior (scaled)", "MLE"),
       col = c("blue", "black", "red"),
       lty = c(1, 2, 3),
       lwd = c(3, 2, 2))
