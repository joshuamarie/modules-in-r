box::use(
    ../../matrix_ops[`*`, `^`],
    ../../tables[draw_table],
    rlang[is_formula, abort, enexpr], 
    purrr[map_dbl, map2_dbl], 
    dplyr[bind_cols, tbl = tibble, select, mutate],
    tidyselect[eval_select], 
    stats[pnorm, model.frame, model.response]
)

sigmoid = function (z) 
    1 / (1 + exp(-z))

binary_response = function (y) {
    y = as.matrix(y)
    
    if (ncol(y) > 1) {
        abort('The response variable `y` must contain only 1 variable')
    }
    unique_vals = unique(y[, 1])
    if (length(unique_vals) != 2) {
        abort('Response variable must be binary with exactly 2 unique values')
    }
    if (all(sort(unique_vals) == c(0, 1))) {
        return(list(y = y, levels = NULL, reference = NULL))
    }
    if (is.factor(y[, 1])) {
        lvls = levels(y[, 1])
        reference = lvls[1]
        target = lvls[2]
        y_binary = ifelse(y == target, 1, 0)
    } else {
        sorted_vals = sort(unique_vals)
        reference = sorted_vals[1]
        target = sorted_vals[2]
        y_binary = ifelse(y == target, 1, 0)
    }
    
    y_binary = matrix(y_binary, ncol = 1)
    
    list(
        y = y_binary,
        levels = c(reference, target),
        reference = reference
    )
}

coef_logit = function (y, X, max_iter = 100, tol = 1e-7) {
    X = cbind(1, X)
    
    y_prep = binary_response(y)
    y = y_prep$y
    
    n = nrow(X)
    p = ncol(X)
    beta = matrix(0, nrow = p, ncol = 1)
    
    for (iter in 1 : max_iter) {
        z = X * beta
        pi = sigmoid(z)
        
        W = diag(as.vector(pi * (1 - pi)))
        
        gradient = t(X) * (y - pi)
        hessian = -t(X) * W * X
        
        beta_new = beta - solve(hessian) * gradient
        
        if (max(abs(beta_new - beta)) < tol) {
            beta = beta_new
            break
        }
        
        beta = beta_new
    }
    
    coef_names = c("beta0", colnames(X)[-1])
    names(beta) = coef_names
    
    list(
        coefficients = beta,
        levels = y_prep$levels,
        reference = y_prep$reference
    )
}

standard_error_logit = function(y, X, coefs) {
    X = cbind(1, X)
    z = X * coefs
    pi = sigmoid(z)
    
    W = diag(as.vector(pi * (1 - pi)))
    
    fisher_info = t(X) * W * X
    cov_mat = solve(fisher_info)
    se_coef = sqrt(diag(cov_mat))
    
    se_coef
}

z_stat = function (est, se) {
    z_val = map2_dbl(est, se, \(est, se) est / se)
    z_val
}

odds_ratio = function (coefs) {
    exp(coefs)
}

vif_logit = function (X) {
    X = as.matrix(X)
    p = ncol(X)
    
    vif_values = map_dbl(
        1 : p, function(i) {
            X_curr = X[, i, drop = FALSE]
            X_other = cbind(1, X[, -i, drop = FALSE])
            
            coefs_vif = (t(X_other) * X_other) ^ -1 * (t(X_other) * X_curr)
            y_hat_vif = X_other * coefs_vif
            
            mu_y = mean(X_curr)
            ssr = sum((y_hat_vif - mu_y)^2)
            sst = sum((X_curr - mu_y)^2)
            r_squared = ssr / sst
            
            1 / (1 - r_squared)
        }
    )
    
    vif_values
}

#' Custom Logistic Regression Model
#'
#' Performs logistic regression using maximum likelihood estimation via
#' Newton-Raphson (IRLS) algorithm. This function implements logistic
#' regression from scratch using matrix operations.
#'
#' @param obj An object used to select a method. Can be a data frame, formula,
#'   or matrix/data frame of predictor variables.
#' @param ... Additional arguments passed to methods.
#'
#' @return A tibble containing the regression results with columns:
#'   \itemize{
#'     \item \code{terms}: Variable names (including intercept as "beta0")
#'     \item \code{coefficients}: Estimated regression coefficients (log-odds)
#'     \item \code{odds_ratio}: Exponentiated coefficients (odds ratios)
#'     \item \code{std_err}: Standard errors of the coefficients
#'     \item \code{z_statistic}: z-statistics for hypothesis testing
#'     \item \code{pval}: Two-tailed p-values for each coefficient
#'     \item \code{vif}: Variance Inflation Factor (only when \code{vif = TRUE}, NA for intercept)
#'     \item \code{tolerance}: Tolerance values (only when \code{vif = TRUE}, 1/VIF, NA for intercept)
#'   }
#'
#' @examples
#' box::use(
#'     ./module/statistics/models/logistic
#' )
#' 
#' # Using formula notation with pipe
#' mtcars |> 
#'     logistic$logistic_reg(am ~ wt + hp)
#'
#' # Using column names
#' mtcars |> 
#'     logistic$logistic_reg(c(wt, hp), am)
#'
#' # Using formula directly
#' logistic$logistic_reg(am ~ wt, data = mtcars)
#' 
#' # Show VIF 
#' mtcars |> 
#'     logistic$logistic_reg(c(disp, hp, wt, drat), am, vif = TRUE)
#'
#' @export
logistic_reg = function (obj, ...) 
    UseMethod("logistic_reg")

#' @rdname logistic_reg
#' @param x For data frame method: either a formula or column selection for 
#'   predictor variables (supports tidyselect syntax).
#' @param y For data frame method: column selection for the response variable.
logistic_reg.data.frame = function (obj, x, y, ...) {
    x_expr = enexpr(x)
  
    if (is_formula(x_expr)) {
        dat = model.frame(x, data = obj)
        X = select(dat, -1)
        y = model.response(dat)
        out = logistic_reg.default(X, y, ...)
    } else {
        X = select(obj, {{ x }})
        y = select(obj, {{ y }})
        out = logistic_reg.default(X, y, ...)
    }
    
    out
}

#' @rdname logistic_reg
#' @param data A data frame containing the variables in the formula.
logistic_reg.formula = function (obj, data, ...) {
    dat = model.frame(obj, data = data)
    X = select(dat, -1)
    y = model.response(dat)
    out = logistic_reg.default(X, y, ...)
    out
}

#' @rdname logistic_reg
#' @param vif Logical. If \code{TRUE}, calculates Variance Inflation Factor
#'   and tolerance for each predictor. Default is \code{FALSE}.
#' @param max_iter Maximum number of iterations for Newton-Raphson algorithm.
#'   Default is 100.
#' @param tol Convergence tolerance. Default is 1e-8.
#' 
#' @keywords internal
logistic_reg.default = function (obj, y, vif = FALSE, max_iter = 100, tol = 1e-8, ...) {
    n = nrow(obj)
    p = ncol(obj) + 1
    
    coef_result = coef_logit(y, obj, max_iter = max_iter, tol = tol)
    coefs = coef_result$coefficients
    levels_info = coef_result$levels
    reference = coef_result$reference
    
    y_prep = binary_response(y)
    y_binary = y_prep$y
    
    se = standard_error_logit(y_binary, obj, coefs)
    stat = z_stat(coefs, se)
    pval = map_dbl(stat, \(z) 2 * pnorm(abs(z), lower.tail = FALSE))
    or = odds_ratio(coefs)
    
    out = tbl(
        terms = names(coefs),
        coefficients = as.vector(coefs), 
        odds_ratio = as.vector(or),
        std_err = se,
        z_statistic = stat, 
        pval = pval
    )
    
    if (vif && ncol(obj) > 1) {
        vif_values = vif_logit(obj)
        out = mutate(
            out,
            vif = round(c(NA, vif_values), digits = 3),
            tolerance = round(c(NA, 1 / vif_values), digits = 3)
        )
    }
    
    X_full = cbind(1, obj)
    fitted_probs = sigmoid(X_full * coefs)
    
    res = list(
        fitted = fitted_probs,
        out = out,
        levels = levels_info,
        reference = reference
    )
    class(res) = "logistic_reg"
    
    res
}

print.logistic_reg = function (x, ...) {
    out = mutate(x$out, across(where(is.numeric), \(x) round(x, digits = 4)))
    
    cat("\n Custom Logistic Regression output: \n\n")
    
    if (!is.null(x$levels)) {
        cat(sprintf(" Response encoding: %s = 0 (reference), %s = 1\n\n", 
                    x$levels[1], x$levels[2]))
    }
    
    draw_table(out, ...)
    cat("\n\n")
}

box::register_S3_method("print", "logistic_reg")

if (is.null(box::name())) {
    box::use(./`__tests__`)
}
