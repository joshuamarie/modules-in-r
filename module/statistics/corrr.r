box::use(
    rlang[enquos, eval_tidy, abort, is_null], 
    glue[glue], 
    dplyr[select], 
    stats[cor],
    purrr[map, every]
)

#' Correlation matrix from the entire data frame
#' @keywords internal
corr_matrix = function (data, method, use) {
    out = cor(as.matrix(data), method = method, use = use)
    out
}

#' Scalar correlation coefficient from raw numeric vectors
#' @keywords internal
corr_vectors = function (values, method, use) {
    if (!every(values, is.numeric)) {
        abort("All arguments must be numeric.")
    }
    
    if (length(values) == 2) {
        out = cor(values[[1]], values[[2]], method = method, use = use)
    } else {
        out = cor(do.call(cbind, values), method = method, use = use)
    }
    
    out
}

#' Data masking functionality
#' @keywords internal
corr_tidy = function (dots, data, method, use) {
    if (length(dots) == 2) {
        x = eval_tidy(dots[[1]], data = data)
        y = eval_tidy(dots[[2]], data = data)
        out = cor(x, y, method = method, use = use)
    } else {
        dat = select(data, !!!dots)
        out = cor(as.matrix(dat), method = method, use = use)
    }
  
    out
}

#' Calculating correlation with 3 cases
#' 
#' This function provides three ways to calculate correlations:
#' 
#' **Case 1: Full correlation matrix**
#' - When `...` is empty and `data` is provided
#' - Returns correlation matrix for all columns in the dataset
#' - Example: `corr(data = mtcars)`
#' 
#' **Case 2: Raw numeric vectors**
#' - When `...` contains vectors but `data` is NULL
#' - Evaluates arguments as standalone numeric vectors
#' - Example: `corr(c(1, 2, 3), c(4, 5, 6))`
#' 
#' **Case 3: Tidy evaluation with data**
#' - When both `...` and `data` are provided
#' - Uses data masking to select columns from the dataset
#' - Example: `corr(speed, dist, data = cars)`
#' 
#' @param ... Column names (with data masking), raw numeric vectors, or empty for full matrix.
#'   Can specify no columns (Case 1), raw vectors (Case 2), or column names (Case 3).
#' @param data A data frame. If provided, data masking is applied to `...`.
#'   Required for Case 1 and Case 3.
#' @param method Correlation method: "pearson" (default), "kendall", or "spearman".
#'   Passed to \code{stats::cor()}.
#' @param use Method for handling missing values. Passed to \code{stats::cor()}.
#'   Default is "everything".
#' 
#' @return A correlation coefficient (scalar) when two variables are specified,
#'   or a correlation matrix when multiple variables or full dataset is used.
#'   It throws an error when both `data` and `...` are not provided. 
#' 
#' @examples
#' box::use(
#'     corr = ./module/statistics/corrr
#' )
#' 
#' corr$corr(speed, dist, data = cars) 
#' corr$corr(data = mtcars)
#' 
#' @export
corr = function(..., data = NULL, method = c("pearson", "kendall", "spearman"), use = "everything") {
    dots = enquos(...) 
    method = match.arg(method)
    
    # Case 1: No arguments - return full correlation matrix
    if (length(dots) == 0) {
        if (is_null(data)) {
            abort("`.data` must be provided when both `x` and `y` are missing.")
        }
        return(corr_matrix(data, method, use))
    }
    
    # Case 2: Arguments without data - evaluate as raw vectors
    if (is_null(data)) {
        values = map(dots, eval_tidy)
        return(corr_vectors(values, method, use))
    }
    
    # Case 3: Data masking
    out = corr_tidy(dots, data, method, use)
    out
}

#' Pipe-friendly correlation calculation
#' 
#' A pipe-optimized wrapper around \code{corr()} that always requires a dataset
#' as the first argument, making it suitable for use in piped workflows with
#' \code{|>} or \code{\%>\%}.
#' 
#' **Behavior:**
#' -  When `...` is empty: Returns correlation matrix for all columns in `.data`
#' -  When `...` specifies columns: Returns correlation matrix for selected columns only
#' 
#' @param .data A data frame (required). Cannot be NULL.
#' @param ... Column names to select from `.data`. If empty, uses all columns.
#'    Supports tidy selection.
#' @param alternative Direction of alternative hypothesis: "two.sided" (default),
#'    "less", or "greater". Currently not implemented.
#' @param method Correlation method: "pearson" (default), "kendall", or "spearman".
#'    Passed to \code{corr()}.
#' @param use Method for handling missing values. Passed to \code{stats::cor()}.
#'    Default is "everything".
#' 
#' @return A correlation matrix for the specified or all columns in `.data`.
#' 
#' @examples
#' box::use(
#'     corr = ./module/statistics/corrr
#' )
#' 
#' mtcars |> 
#'     corr$cor_pipe()
#' 
#' mtcars |> 
#'     corr$cor_pipe(mpg, hp, wt)
#' 
#' iris |> 
#'     corr$cor_pipe(Sepal.Length, Sepal.Width, method = "spearman")
#' 
#' @export
cor_pipe = function(.data, ..., alternative = "two.sided", method = c("pearson", "kendall", "spearman"), use = "everything") {
    method = match.arg(method)
    
    if (is_null(.data)) {
        abort("`.data` must be provided, it cannot be NULL in this function.")
    }
    
    dots = enquos(...)
    
    if (length(dots) == 0) {
        return(corr(data = .data, method = method))
    }
    
    out = corr(!!!dots, data = .data, method = method)
    out
}

