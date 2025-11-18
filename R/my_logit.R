#' Logistic regression using IRLS
#'
#' @param formula Model formula, e.g., y ~ x1 + x2
#' @param data A data.frame containing the variables
#' @param max_iter Maximum number of iterations
#' @param tol Tolerance for convergence
#'
#' @return An object of class "my_logit".
#'
#' @examples
#' fit <- my_logit(am ~ mpg + wt, data = mtcars)
#' summary(fit)
#' residuals(fit)[1:5]
#'
#' @importFrom stats model.frame model.response model.matrix pnorm
#' @export
my_logit <- function(formula, data, max_iter = 25, tol = 1e-6) {

  mf <- model.frame(formula, data = data)
  y  <- model.response(mf)
  X  <- model.matrix(formula, data = data)

  if (!all(y %in% c(0, 1))) {
    stop("Response must be 0/1 for my_logit().")
  }

  n <- nrow(X)
  p <- ncol(X)

  beta <- rep(0, p)
  converged <- FALSE

  for (i in seq_len(max_iter)) {
    eta <- as.vector(X %*% beta)
    mu  <- 1 / (1 + exp(-eta))

    W <- mu * (1 - mu)
    W[W == 0] <- 1e-6

    z  <- eta + (y - mu) / W
    WX <- sqrt(W) * X
    wz <- sqrt(W) * z

    XtWX <- crossprod(WX)
    XtWz <- crossprod(WX, wz)

    beta_new <- solve(XtWX, XtWz)

    if (max(abs(beta_new - beta)) < tol) {
      beta <- beta_new
      converged <- TRUE
      break
    }

    beta <- beta_new
  }

  if (!converged) {
    i <- max_iter
  }

  eta <- as.vector(X %*% beta)
  mu  <- 1 / (1 + exp(-eta))

  # deviance residuals
  mu_clip <- pmin(pmax(mu, 1e-10), 1 - 1e-10)

  dev_comp <- ifelse(
    y == 1,
    2 * log(1 / mu_clip),
    2 * log(1 / (1 - mu_clip))
  )
  sign_res <- ifelse(y == 1, 1, -1)
  dev_res  <- sign_res * sqrt(dev_comp)

  W_final <- mu * (1 - mu)
  W_final[W_final == 0] <- 1e-6

  XtWX_final <- crossprod(sqrt(W_final) * X)
  vcov_beta  <- tryCatch(
    solve(XtWX_final),
    error = function(e) matrix(NA_real_, p, p)
  )
  se_beta <- sqrt(diag(vcov_beta))

  out <- list(
    coefficients      = beta,
    fitted.values     = mu,
    linear.predictors = eta,
    residuals         = dev_res,
    converged         = converged,
    iterations        = i,
    formula           = formula,
    vcov              = vcov_beta,
    se                = se_beta,
    y                 = y,
    X                 = X,
    n                 = n
  )

  class(out) <- "my_logit"
  out
}

#' @export
print.my_logit <- function(x, ...) {
  info <- list(
    formula      = x$formula,
    coefficients = x$coefficients,
    converged    = x$converged,
    iterations   = x$iterations
  )
  print(info)
  invisible(x)
}

#' @export
summary.my_logit <- function(object, ...) {

  est  <- object$coefficients
  se   <- object$se
  zval <- est / se
  pval <- 2 * (1 - pnorm(abs(zval)))

  data.frame(
    Estimate  = est,
    Std.Error = se,
    z.value   = zval,
    Pr.z.     = pval
  )
}

#' @export
residuals.my_logit <- function(object, ...) {
  object$residuals
}
