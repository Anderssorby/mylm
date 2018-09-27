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
  est$dof <- dof <- n-length(beta)
  est$ssr <- ssr <- sum(residuals^2) # Residual sum of squares
  est$rse <- rse <- sqrt(ssr/dof) # Residual standard error
  est$sst <- sst <- sum((y-mean(y))^2)  # Total sum of squares
  est$sse <- sse <- sst-ssr
  est$r2 <- r2 <- 1-ssr/sst # R^2
  est$r2adj <- r2adj <- 1-(1-r2)*(n-1)/(n-length(beta))
  est$Fstat <- Fstat <- (sse)/(length(beta) - 1) * (n-p)/ssr # F-statistic
  est$Fpval <- Fpval <- 1-pchisq(Fstat*(p-1), df = p-1)

  # z-test
  statistics <- rep(0, p)
  pvalues <- rep(0, p)
  for (j in 1:p) {
    statistics[j] <- beta[j] / sqrt(covar[j, j])
    pvalues[j] <- 2*(1-pnorm(abs(statistics[j])))
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
  cat("Call:\nmylm : formula = ")
  print(est$formula)
  cat('\nCoefficients:\n')
  v = as.vector(as.numeric(format(est$beta, digits = 4, nsmall = 4, trim = T))) # formatting s.t. there are only five decimals
  names(v) = rownames(est$beta)
  v
}

summary.mylm <- function(est, ...) {
  #df <- as.data.frame(matrix(c(reg$beta,sqrt(diag(reg$covar)),reg$statistics,reg$pvalues),ncol = 4))
  # Code here is used when summary(object) is used on objects of class "mylm"
  # Useful functions include cat, print.default and format
  cat('Summary of mylm\n\n')

  cat("Residuals: \n")
  #max_res <- max(est$residuals)
  #min_res <- min(est$residuals)
  #mean_res <- mean(est$residuals)
  #median_res <- median(est$residuals)
  #cat("Min \t Median \t Max \t \n")
  #cat(sprintf("%.5f\t%.5f\t%.5f\t\n", min_res, median_res, max_res)) # WHAT ABOUT 1Q AND 3Q?

  v = quantile(est$residuals, names = T)
  names(v) = c("Min", "1Q", "Median", "3Q", "Max")
  print(v, digits = 3)

  cat("\nCoefficients:\n")

  mat = as.matrix(cbind(est$beta, sqrt(diag(est$covar)), est$statistics, est$pvalues))
  colnames(mat) = c("Estimate", "Std. Error", "z value", "Pr(>|z|)")
  print(mat, digits = 4)    # how many digits?
  cat("---\n")
  cat("Signif. codes:\t0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")
  cat("\n\nResidual standard error:", est$rse, "on", est$dof, "degrees of freedom\n")
  cat("Multiple R-squared:", est$r2, "\tAdjusted R-squared:", est$r2adj, "\n")
  cat("F-statistic:", est$Fstat, "on", length(est$beta)-1, "and", est$dof, "DF, p-value: <", est$Fpval)
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
