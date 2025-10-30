box::use(
    ../../matrix_ops[`*`, `^`],
    ../../tables[draw_table],
    rlang[is_formula, abort, quo, enexpr], 
    purrr[map_dbl, map2_dbl], 
    dplyr[bind_cols, tbl = tibble, select, mutate],
    tidyselect[eval_select], 
    stats[pt, model.frame, model.response]
)

coef_lr = function (y, X) {
    Xt = cbind(1, X)
    y = as.matrix(y)
    
    if (ncol(y) > 1) {
        abort('The response variable `y` must contain only 1 variable')
    }
    
    coefs = (t(Xt) * Xt) ^ -1 * (t(Xt) * y)
    coef_names = c("beta0", colnames(X))
    names(coefs) = coef_names
    
    coefs
}

standard_error = function (y, X, coefs) {
    X = cbind(1, X)
    n = nrow(X)
    p = ncol(X)
    y_hat = X * coefs                    
    e = y - y_hat                        
    rss = t(e) * e
    
    var_coef = as.double(rss / (n - p))  
    cov_mat = var_coef * (t(X) * X) ^ -1   
    se_coef = sqrt(diag(cov_mat))
    se_coef
}

t_stat = function (est, se) {
    t_val = map2_dbl(est, se, \(est, se) est / se)
    t_val
}

vif = function (X) {
    X = as.matrix(X)
    p = ncol(X)
    
    vif_values = map_dbl(
        1 : p, function (i) {
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

#' Custom Linear Regression Model
#'
#' Performs linear regression using ordinary least squares (OLS) estimation.
#' This function implements the mathematical solution to linear regression
#' from scratch using matrix operations.
#'
#' @param obj An object used to select a method. Can be a data frame, formula,
#'   or matrix/data frame of predictor variables.
#' @param ... Additional arguments passed to methods.
#'
#' @return A tibble containing the regression results with columns:
#'   \itemize{
#'     \item \code{terms}: Variable names (including intercept as "beta0")
#'     \item \code{coefficients}: Estimated regression coefficients
#'     \item \code{std_err}: Standard errors of the coefficients
#'     \item \code{t_statistic}: t-statistics for hypothesis testing
#'     \item \code{pval}: Two-tailed p-values for each coefficient
#'     \item \code{vif}: Variance Inflation Factor (only when \code{vif = TRUE}, NA for intercept)
#'     \item \code{tolerance}: Tolerance values (only when \code{vif = TRUE}, 1/VIF, NA for intercept)
#'   }
#'
#' @examples
#' box::use(
#'     ./module/statistics/models/linear
#' )
#' 
#' # Using formula notation with pipe
#' mtcars |> 
#'     linear$linear_reg(mpg ~ wt + hp)
#'
#' # Using column names
#' mtcars |> 
#'     linear$linear_reg(c(wt, hp), mpg)
#'
#' # Using formula directly
#' linear$linear_reg(mpg ~ wt, data = mtcars)
#' 
#' # Show VIF 
#' mtcars |> 
#'     linear$linear_reg(c(disp, hp, wt, drat), mpg, vif = TRUE)
#'
#' @export
linear_reg = function (obj, ...) 
    UseMethod("linear_reg")

#' @rdname linear_reg
#' @param x For data frame method: either a formula or column selection for 
#'   predictor variables (supports tidyselect syntax).
#' @param y For data frame method: column selection for the response variable.
linear_reg.data.frame = function (obj, x, y, ...) {
    x_expr = enexpr(x)
  
    if (is_formula(x_expr)) {
        dat = model.frame(x, data = obj)
        X = select(dat, -1)
        y = model.response(dat)
        out = linear_reg.default(X, y)
    } else {
        X = select(obj, {{ x }})
        y = select(obj, {{ y }})
        out = linear_reg.default(X, y, ...)
    }
    
    out
}

#' @rdname linear_reg
#' @param data A data frame containing the variables in the formula.
linear_reg.formula = function (obj, data, ...) {
    dat = model.frame(obj, data = data)
    X = select(dat, -1)
    y = model.response(dat)
    out = linear_reg.default(X, y, ...)
    out
}

#' @rdname linear_reg
#' @param vif Logical. If \code{TRUE}, calculates Variance Inflation Factor
#'   and tolerance for each predictor. Default is \code{FALSE}.
#' 
#' @keywords internal
linear_reg.default = function (obj, y, vif = FALSE, ...) {
    n = nrow(obj)
    p = ncol(obj) + 1
    coefs = coef_lr(y, obj)
    se = standard_error(y, obj, coefs)
    stat = t_stat(coefs, se)
    pval = map_dbl(stat, \(t) 2 * pt(abs(t), df = n - p, lower.tail = F))
    
    out = tbl(
        terms = names(coefs),
        coefficients = as.vector(coefs), 
        std_err = se,
        t_statistic = stat, 
        pval = pval
    )
    
    if (vif && ncol(obj) > 1) {
        vif_values = vif(obj)
        out = mutate(
            out,
            vif = round(c(NA, vif_values), digits = 3),
            tolerance = round(c(NA, 1 / vif_values), digits = 3)
        )
    }
    
    res = list(
        fitted = cbind(1, obj) * coefs, 
        out = out
    )
    class(res) = "linear_reg"
    
    res
}

print.linear_reg = function(x, ...) {
    out = mutate(x$out, across(where(is.numeric), \(x) round(x, digits = 2)))
    
    cat("\n Custom Linear Regression output: \n\n")
    draw_table(out, ...)
    cat("\n\n")
}

box::register_S3_method("print", "linear_reg")

if (is.null(box::name())) {
    box::use(./`__tests__`)
}
