# --Matrix Multiplication--

test_that('matrix multiplication works correctly', {
    m1 = matrix(c(1, 2, 3, 4), nrow = 2)
    m2 = matrix(c(5, 6, 7, 8), nrow = 2)

    result = m1 * m2
    expected = m1 %*% m2

    expect_equal(result, expected)
})

test_that('matrix multiplication with dimension mismatch tries reverse order', {
    m1 = matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
    m2 = matrix(c(7, 8), nrow = 2, ncol = 1)

    expect_error(m1 * m2)
    expect_error(m2 %*% m1)
})

test_that('matrix multiplication works with scalar', {
    m1 = matrix(c(1, 2, 3, 4), nrow = 2)
    scalar = 2

    result = m1 * scalar
    expected = base::`*`(m1, scalar)

    expect_equal(result, expected)
})

# --Exponentiation--
test_that('matrix inverse (^-1) works correctly', {
    m = rbind(c(2, 7), c(5, -4))
    
    result = m ^ -1
    expected = solve(m)
    
    expect_equal(result, expected)
})

# --Solving system of linear equation--
test_that('combined operations work (inverse then multiply)', {
    A = rbind(c(2, 7), c(5, -4))
    b = cbind(c(34, -1))
    
    result = A ^ -1 * b
    expected = matrix(c(3, 4), ncol = 1)
    
    expect_equal(result, expected)
})
