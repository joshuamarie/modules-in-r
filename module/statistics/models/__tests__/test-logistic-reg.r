# ---Logistic Regression from ../statistics/models/logistic.r

test_that('Logistic Regression from formula S3 method works', {
    model = logistic_reg(am ~ wt + hp, mtcars, vif = TRUE)
    
    expect_s3_class(model, "logistic_reg")
    expect_true("out" %in% names(model))
    expect_true("fitted" %in% names(model))
    expect_true("levels" %in% names(model))
    expect_true("reference" %in% names(model))
    expect_true(all(c("terms", "coefficients", "odds_ratio", "std_err", "z_statistic", "pval", "vif", "tolerance") %in% names(model$out)))
    expect_equal(nrow(model$out), 3)
    expect_equal(model$out$terms, c("beta0", "wt", "hp"))
})

test_that('Logistic Regression from data frame S3 method works', {
    model = 
        mtcars |> 
        logistic_reg(am ~ wt + hp, vif = TRUE)
    
    expect_s3_class(model, "logistic_reg")
    expect_equal(nrow(model$out), 3)
})

test_that('Logistic Regression calculates correct coefficients', {
    # Compare with base R glm()
    model = logistic_reg(am ~ wt + hp, mtcars)
    base_model = glm(am ~ wt + hp, data = mtcars, family = binomial(link = "logit"))
    
    expect_equal(model$out$coefficients, as.vector(coef(base_model)), tolerance = 1e-4)
})


test_that('Logistic Regression with column selection works', {
    model = 
        mtcars |> 
        logistic_reg(c(wt, hp), am)
    
    expect_s3_class(model, "logistic_reg")
    expect_equal(nrow(model$out), 3)
    
    # Without vif flag, should not have vif columns
    expect_false("vif" %in% names(model$out))
})
