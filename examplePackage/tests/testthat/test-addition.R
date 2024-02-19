test_that("this is the test of addition", {
    result = my_addition(1, 1)
    expect_equal(result, 3) # this should succeed
})

test_that("this is the test of actual addition", {
    result = my_addition(1, 1)
    expect_equal(result, 2) # this should fail
})