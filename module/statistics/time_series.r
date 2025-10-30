#' Simple Moving Average
#' 
#' Calculates a simple moving average (also known as rolling mean) for a time 
#' series using a specified window size. Each value in the output is the mean 
#' of a window of consecutive observations from the input series.
#' 
#' @param x A numeric vector representing the time series data
#' @param win An integer specifying the window size (number of observations to 
#'   average). Default is 3
#' 
#' @return A numeric vector of length n - win + 1, where n is the length of x.
#'   Each element represents the mean of a window of size win from the original 
#'   series
#' 
#' @details The function computes moving averages by sliding a window of fixed 
#'   size across the time series. The output length is reduced because only 
#'   complete windows are used.
#' 
#' @examples
#' box::use(
#'     md_ts = ./module/time_series
#' )
#' 
#' time(AirPassengers)
#' md_ts$moving_average(AirPassengers)
#' 
#' @export
moving_average = function (x, win = 3) {
    nn = length(x)
    ll = nn - win + 1
    res = numeric(ll)
    
    for (i in 1 : ll) {
        res[i] = mean(x[i : (i + win - 1)])
    }
    res
}

#' Simple Exponential Smoothing
#' 
#' Performs simple exponential smoothing on a time series using a specified
#' smoothing parameter alpha. The smoothed values are computed recursively,
#' with each smoothed value being a weighted average of the current observation
#' and the previous smoothed value.
#' 
#' @param x A numeric vector representing the time series data
#' @param alpha A numeric value between 0 and 1 representing the smoothing 
#'   parameter. Higher values give more weight to recent observations. 
#'   Default is 0.3
#' @param init Initial value for smoothing. If NULL (default), uses the first
#'   observation as the initial value
#' 
#' @return A numeric vector of the same length as x containing the smoothed values
#' 
#' @details The simple exponential smoothing formula is:
#'   S[t] = alpha * x[t] + (1 - alpha) * S[t-1]
#'   where S[1] is initialized to x[1] or a specified initial value.
#'   
#'   Alpha controls the rate of decay for past observations. Values closer to 1
#'   give more weight to recent observations, while values closer to 0 give more
#'   weight to historical data.
#' 
#' @examples
#' box::use(
#'     md_ts = ./module/time_series
#' )
#' 
#' md_ts$SES(AirPassengers)
#' 
#' @export
SES = function (x, alpha = 0.3, init = NULL) {
    n = length(x)
    res = numeric(n)
    
    if (is.null(init)) {
        res[1] = x[1]
    } else {
        res[1] = init
    }
    
    for (i in 2 : n) {
        res[i] = alpha * x[i] + (1 - alpha) * res[i - 1]
    }
    
    res
}

#' Inefficient ACF Calculation
#' 
#' Computes the autocorrelation function (ACF) for a time series using an 
#' inefficient but explicit implementation. The ACF measures the correlation 
#' between observations at different time lags.
#' 
#' @param x A numeric vector representing the time series data
#' @param lag_max An integer specifying the maximum lag at which to calculate 
#'   the ACF. Default is 10
#' 
#' @return A numeric vector of length lag_max + 1 containing the autocorrelation
#'   values at lags 0 through lag_max. The first element (lag 0) is always 1.
#' 
#' @details This function computes the sample autocorrelation coefficient at 
#'   each lag using the formula:
#'   rho[t] = sum((x[k] - mean(x)) * (x[k-t] - mean(x))) / sum((x[i] - mean(x))^2)
#'   
#'   Note: This is an intentionally inefficient implementation for educational 
#'   purposes. For production use, consider using stats::acf() instead.
#' 
#' @examples
#' box::use(
#'     md_ts = ./module/time_series
#' )
#' 
#' md_ts$ACF(AirPassengers)
#' 
#' @export
ACF = function (x, lag_max = 10) {
    out = numeric(lag_max + 1)
    mu_x = mean(x, na.rm = TRUE)
    n = length(x)
    denom = 0
    
    for (i in 1 : n) {
        denom = denom + (x[i] - mu_x)^2
    }
    
    for (t in 0 : lag_max) {
        num = 0
        
        for (k in (t + 1) : n) {
            num = num + (x[k] - mu_x) * (x[k - t] - mu_x)
        }
        
        out[t + 1] = num / denom
    }
    
    out
}

#' Inefficient PACF Calculation
#' 
#' Computes the partial autocorrelation function (PACF) for a time series using 
#' an inefficient but explicit implementation via the Durbin-Levinson recursion. 
#' The PACF measures the correlation between observations at different lags after 
#' removing the effects of intermediate lags.
#' 
#' @param x A numeric vector representing the time series data
#' @param lag_max An integer specifying the maximum lag at which to calculate 
#'   the PACF. Default is 10
#' 
#' @return A numeric vector of length lag_max containing the partial 
#'   autocorrelation values at lags 1 through lag_max
#' 
#' @details This function uses the Durbin-Levinson algorithm to compute partial 
#'   autocorrelations from the autocorrelation function. The PACF at lag k 
#'   represents the correlation between x[t] and x[t-k] after controlling for 
#'   the linear effects of x[t-1], x[t-2], ..., x[t-k+1].
#'   
#'   The function first computes the ACF, then applies the recursive formula:
#'   phi[n,n] = (rho[n] - sum(phi[n-1,k] * rho[n-k])) / (1 - sum(phi[n-1,i] * rho[i]))
#'   
#'   Note: This is an intentionally inefficient implementation for educational 
#'   purposes. For production use, consider using stats::pacf() instead.
#' 
#' @examples
#' box::use(
#'     md_ts = ./module/time_series
#' )
#' 
#' md_ts$PACF(AirPassengers)
#' 
#' @seealso \code{\link{ACF}} for the autocorrelation function
#' 
#' @export
PACF = function (x, lag_max = 10) {
    rho = ACF(x, lag_max = lag_max)[-1]
    
    phi = matrix(nrow = lag_max, ncol = lag_max)
    res = numeric(lag_max)
    
    phi[1, 1] = rho[1]
    res[1] = phi[1, 1]
    
    for (n in 2 : lag_max) {
        num = rho[n]
        
        for (k in 1 : (n - 1)) {
            num = num - phi[n - 1, k] * rho[n - k]
        }
        
        denom = 1
        
        for (i in 1 : (n - 1)) { 
            denom = denom - phi[n - 1, i] * rho[i] 
        }
        
        phi[n, n] = num / denom
        
        for (k in 1 : (n - 1)) {
            phi[n, k] = phi[n - 1, k] - phi[n, n] * phi[n - 1, n - k]
        }
        
        res[n] = phi[n, n]
    }
    
    res
}
