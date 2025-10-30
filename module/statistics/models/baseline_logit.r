box::use(
    ../../matrix_ops[`*`, `^`]
)

#' Predicted Probabilities for Baseline Logit Model
#'
#' @param beta Coefficient matrix of dimension (J - 1) x p
#' @param X Design matrix of dimension n x p
#' @return Matrix of predicted probabilities of dimension n x J
#' @keywords internal
probs = function (beta, X) {
    n = nrow(X)
    J = nrow(beta) + 1
    
    eta = X * t(beta)
    exp_eta = exp(eta)
    denom = 1 + rowSums(exp_eta)
    
    probs = matrix(0, n, J)
    probs[, 1:(J-1)] = exp_eta / denom
    probs[, J] = 1 / denom
    
    probs
}

#' Custom Log-Likelihood function for Baseline Logit Model
#'
#' @param beta Coefficient matrix of dimension (J-1) x p
#' @param X Design matrix of dimension n x p
#' @param Y Indicator matrix of dimension n x J
#' @return Scalar log-likelihood value
#' @keywords internal
loglik = function (beta, X, Y) {
    probs = compute_probs(beta, X)
    ll = sum(Y * log(probs + 1e-10))
    ll
}

#' Gradient Vector for Baseline Logit Model
#'
#' @param beta Coefficient matrix of dimension (J-1) x p
#' @param X Design matrix of dimension n x p
#' @param Y Indicator matrix of dimension n x J
#' @param probs Predicted probability matrix of dimension n x J
#' @return Vectorized gradient of length (J-1) * p
#' @keywords internal
gradient = function (beta, X, Y, probs) {
    J = ncol(Y)
    p = ncol(X)
    grad = matrix(0, J - 1, p)
    
    for (j in 1:(J-1)) {
        grad[j, ] = t(X) * (Y[, j] - probs[, j])
    }
    
    out = as.vector(t(grad))
    out
}

#' Compute Hessian Matrix for Baseline Logit Model
#'
#' @param beta Coefficient matrix of dimension (J-1) x p
#' @param X Design matrix of dimension n x p
#' @param probs Predicted probability matrix of dimension n x J
#' @return Hessian matrix of dimension ((J-1)*p) x ((J-1)*p)
#' @keywords internal
hessian = function (beta, X, probs) {
    n = nrow(X)
    p = ncol(X)
    J = ncol(probs)
    
    total_params = (J - 1) * p
    H = matrix(0, total_params, total_params)
    
    for (j in 1 : (J-1)) {
        for (k in 1:(J-1)) {
            row_start = (j - 1) * p + 1
            row_end = j * p
            col_start = (k - 1) * p + 1
            col_end = k * p
            
            if (j == k) {
                W = diag(as.vector(probs[, j] * (1 - probs[, j])))
            } else {
                W = diag(as.vector(-probs[, j] * probs[, k]))
            }
            
            H[row_start:row_end, col_start:col_end] = -t(X) * W * X
        }
    }
    
    H
}

#' Baseline Logit Regression Model
#'
#' Estimates a baseline-category logit model (also known as multinomial logit)
#' for nominal outcomes with more than two categories using maximum likelihood
#' estimation via Newton-Raphson algorithm.
#'
#' @param y Factor or numeric vector of outcomes. If numeric, should be coded
#'   as 1, 2, ..., J where J is the number of categories. The highest category
#'   number is used as the baseline/reference category.
#' @param X Design matrix of dimension n x p, typically including an intercept
#'   column. Should not contain missing values.
#' @param max_iter Maximum number of Newton-Raphson iterations. Default is 100.
#' @param tol Convergence tolerance for change in log-likelihood. Default is 1e-8.
#'
#' @return An object of class \code{baseline_logit} containing:
#' \describe{
#'   \item{coefficients}{Matrix of coefficient estimates with dimension (J-1) x p,
#'     where each row contains coefficients for one non-baseline category}
#'   \item{se}{Matrix of standard errors with same dimension as coefficients}
#'   \item{vcov}{Variance-covariance matrix of the parameters}
#'   \item{fitted.values}{Matrix of fitted probabilities with dimension n x J}
#'   \item{loglik}{Log-likelihood value at convergence}
#'   \item{n_obs}{Number of observations}
#'   \item{n_categories}{Number of outcome categories}
#'   \item{n_params}{Number of parameters per equation}
#' }
#'
#' @details
#' The baseline logit model estimates log-odds relative to a baseline category:
#' \deqn{\log(P(Y=j)/P(Y=J)) = \beta_{0j} + \beta_{1j}X_1 + \beta_{2j}X_2 + \ldots}
#'
#' The model uses Newton-Raphson optimization to maximize the multinomial
#' log-likelihood function. Standard errors are computed from the inverse of
#' the negative Hessian matrix at convergence.
#'
#' @references
#' Agresti, A. (2013). \emph{Categorical Data Analysis} (3rd ed.). Wiley.
#'
#' Long, J. S. (1997). \emph{Regression Models for Categorical and Limited
#' Dependent Variables}. Sage Publications.
#'
#' @examples
#' set.seed(123)
#' n = 200
#' X = cbind(1, rnorm(n), rnorm(n))
#' colnames(X) = c("(Intercept)", "X1", "X2")
#'
#' eta1 = X * c(0.5, 1, -0.5)
#' eta2 = X * c(-0.3, -0.5, 1)
#' probs = cbind(exp(eta1), exp(eta2), 1)
#' probs = probs / rowSums(probs)
#' y = apply(probs, 1, function (p) sample(1:3, 1, prob = p))
#'
#' model = baseline_logit(y, X)
#' print(model)
#'
#' @export
baseline_logit = function (y, X, max_iter = 100, tol = 1e-8) {
    
    if (is.factor(y)) {
        y_numeric = as.numeric(y)
    } else {
        y_numeric = y
    }
    
    n = nrow(X)
    p = ncol(X)
    J = length(unique(y_numeric))
    
    Y = matrix(0, n, J)
    for (i in 1:n) {
        Y[i, y_numeric[i]] = 1
    }
    
    beta = matrix(0, J - 1, p)
    
    for (iter in 1 : max_iter) {
        probs = compute_probs(beta, X)
        ll_old = compute_loglik(beta, X, Y)
        
        g = compute_gradient(beta, X, Y, probs)
        H = compute_hessian(beta, X, probs)
        
        beta_vec = as.vector(t(beta))
        tryCatch({
            beta_vec_new = beta_vec - solve(H) * g
        }, error = function (e) {
            stop("Hessian is singular. Try different starting values or check for separation.")
        })
        
        beta_new = matrix(beta_vec_new, J - 1, p, byrow = TRUE)
        ll_new = compute_loglik(beta_new, X, Y)
        
        if (abs(ll_new - ll_old) < tol) {
            beta = beta_new
            cat("Converged in", iter, "iterations\n")
            break
        }
        
        beta = beta_new
        
        if (iter == max_iter) {
            warning("Maximum iterations reached without convergence")
        }
    }
    
    probs_final = compute_probs(beta, X)
    ll_final = compute_loglik(beta, X, Y)
    
    H_final = compute_hessian(beta, X, probs_final)
    vcov = tryCatch({
        solve(-H_final)
    }, error = function (e) {
        warning("Could not compute variance-covariance matrix")
        return(NULL)
    })
    
    if (!is.null(vcov)) {
        se = matrix(sqrt(diag(vcov)), J - 1, p, byrow = TRUE)
    } else {
        se = NULL
    }
    
    out = list(
        coefficients = beta,
        se = se,
        vcov = vcov,
        fitted.values = probs_final,
        loglik = ll_final,
        n_obs = n,
        n_categories = J,
        n_params = p
    )
    
    class(out) = "baseline_logit"
    out
}