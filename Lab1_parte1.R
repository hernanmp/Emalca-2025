library(CVXR)


set.seed(123)
n <- 1014        # Signal dimension
m <- 64        # Number of measurements
A <- matrix(rnorm(m * n), m, n)  # Measurement matrix



x_true <- rep(0, n)
x_true[sample(1:n, 10)] <- rnorm(5)  # Sparse true signal
plot(x_true)
b <- A %*% x_true


x <- Variable(n)
objective <- Minimize(norm1(x))
constraints <- list(A %*% x == b)
problem <- Problem(objective, constraints)

result <- solve(problem)
x_est <- result$getValue(x)


# Plot results
plot(x_true, type = "h", lwd = 3, col = "blue", ylim = range(c(x_true, x_est)),
     main = "True vs Recovered Signal")
lines(x_est, type = "h", col = "red", lwd = 2)
legend("topright", legend = c("True", "Recovered"), col = c("blue", "red"), lwd = 2)

sum((x_est - x_true)^2)
###########################################
