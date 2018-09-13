

# Select Build, Build and reload to build and lode into the R-session.

mylm <- function(formula,
                 data = list(),
                 contrasts = NULL,
                 ...) {
  # Extract model matrix & responses
  mf <- model.frame(formula = formula, data = data)
  x  <-
    model.matrix(attr(mf, "terms"),
                 data = mf,
                 contrasts.arg = contrasts)
  y  <- model.response(mf)


  # Add code here to calculate coefficients, residuals, fitted values, etc...
  # and store the results in the list est
  est <- list(terms = terms, model = mf)


  n <- length(y)
  p <- ncol(x)
  terms <- attr(mf, "terms")
  est$H <- H  <- solve(t(x) %*% x) %*% t(x)
  est$beta <- beta <- H %*% y
  est$residuals <- residuals <- y - x %*% beta
  sigma2 <- drop(t(residuals) %*% (residuals) / (n - p))
  est$covar <- covar <- solve(t(x) %*% x) * sigma2


  statistics <- rep(0, p)
  pvalues <- rep(0, p)
  for (j in 1:p) {
    statistics[j] <- beta[j] / sqrt(covar[j, j])
    pvalues[j] <- 2*(1-pnorm(statistics[j]))
  }
  # Store call and formula used
  est$statistics <- statistics
  est$pvalues <- pvalues

  est$call <- match.call()
  est$formula <- formula
  est$sigma <- sqrt(sigma2)
  # Set class name. This is very important!
  class(est) <- 'mylm'

  # Return the object with all results
  return(est)
}

print.mylm <- function(est, ...) {
  # Code here is used when print(object) is used on objects of class "mylm"
  # Useful functions include cat, print.default and format
  cat('Info about mylm\n')
  print(est$beta)
  print(est$pvalues)
}

summary.mylm <- function(est, ...) {
  # Code here is used when summary(object) is used on objects of class "mylm"
  # Useful functions include cat, print.default and format
  cat('Summary of mylm\n')

  cat("Residuals:\n")
  max_res <- max(est$residuals)
  min_res <- min(est$residuals)
  mean_res <- mean(est$residuals)
  median_res <- median(est$residuals)
  cat("Min\tMax\tMean\t\n")
  cat(sprintf("%.5f\t%.5f\t%.5f\t\n", max_res, mean_res, median_res))


  cat("Coefficients:\n")
  print(est$beta)
  print(est$pvalues)
}

plot.mylm <- function(est, ...) {
  # Code here is used when plot(object) is used on objects of class "mylm"
  plot(est$covar, title = est$formula)
}



# This part is optional! You do not have to implement anova
anova.mylm <- function(object, ...) {
  # Code here is used when anova(object) is used on objects of class "mylm"

  # Components to test
  comp <- attr(object$terms, "term.labels")

  # Name of response
  response <- deparse(object$terms[[2]])

  # Fit the sequence of models
  txtFormula <- paste(response, "~", sep = "")
  model <- list()
  for (numComp in 1:length(comp)) {
    if (numComp == 1) {
      txtFormula <- paste(txtFormula, comp[numComp])
    }
    else{
      txtFormula <- paste(txtFormula, comp[numComp], sep = "+")
    }
    formula <- formula(txtFormula)
    model[[numComp]] <- lm(formula = formula, data = object$model)
  }

  # Print Analysis of Variance Table
  cat('Analysis of Variance Table\n')
  cat(c('Response: ', response, '\n'), sep = '')
  cat('          Df  Sum sq X2 value Pr(>X2)\n')
  for (numComp in 1:length(comp)) {
    # Add code to print the line for each model tested
  }

  return(model)

}
