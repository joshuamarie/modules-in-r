# ---Linear Regression from ../statistics/models/linear.r

test_that('Linear Regression from formula S3 method works', {
    model = linear_reg(mpg ~ wt + hp, mtcars, vif = TRUE)
    
    expect_s3_class(model, "linear_reg")
    expect_true("out" %in% names(model))
    expect_true("fitted" %in% names(model))
    expect_true(all(c("terms", "coefficients", "std_err", "t_statistic", "pval", "vif", "tolerance") %in% names(model$out)))
    expect_equal(nrow(model$out), 3)
    expect_equal(model$out$terms, c("beta0", "wt", "hp"))
})

test_that('Linear Regression from data frame S3 method works', {
    model = 
        mtcars |> 
        linear_reg(mpg ~ wt + hp, vif = TRUE)
    
    expect_s3_class(model, "linear_reg")
    expect_error(nrow(model$out), 3)
})

test_that('Linear Regression calculates correct coefficients', {
    # Compare with base R lm()
    model = linear_reg(mpg ~ wt + hp, mtcars)
    base_model = lm(mpg ~ wt + hp, mtcars)
    
    expect_equal(model$out$coefficients, as.vector(coef(base_model)), tolerance = 1e-6)
})

test_that('Linear Regression with column selection works', {
    model =
        mtcars |> 
        linear_reg(c(wt, hp), mpg)
    
    expect_s3_class(model, "linear_reg")
    expect_equal(nrow(model$out), 3)
    
    # Without vif flag, should not have vif columns
    expect_false("vif" %in% names(model$out))
})
