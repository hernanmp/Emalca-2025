library(glmnet)

# Simulate data
set.seed(42)
n <- 200  # number of observations
p <- 500   # number of predictors

X <- matrix(rnorm(n * p), nrow = n, ncol = p)
beta <- c(rep(3, 5), rep(0, p - 5))  # sparse true coefficients
y <- X %*% beta + rnorm(n)

# Fit Lasso regression (alpha=1 for Lasso)
lasso_fit <- glmnet(X, y, alpha = 1)

# Plot solution paths
plot(lasso_fit, xvar = "lambda", label = TRUE)
title("Lasso Coefficient Paths")

# Cross-validation to select best lambda
cv_fit <- cv.glmnet(X, y, alpha = 1)
plot(cv_fit)
title("Cross-Validation Error")

# Best lambda value
best_lambda <- cv_fit$lambda.min
cat("Best lambda:", best_lambda, "\n")

# Coefficients at best lambda
beta_hat = coef(cv_fit, s = "lambda.min")
plot(beta_hat)
plot(beta_hat[1:50])

