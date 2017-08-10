install.packages("testthat")
library(testthat)
# test of schaffer function
test_that("schaffer", {
  
  schaffer_test <- schaffer(c(0,0))
  
  expect_that( schaffer_test, is_a("numeric") )
  expect_that( schaffer_test, equals(0))
})

# test of PSO function
test_that("pso", {
  pso_test <- pso_function(50, 2, 0, 10)
  expect_that( pso_test, is_a("list") )
  matrix_pso_test <- matrix(unlist(pso_test), ncol = 2, byrow = TRUE)
  expect_that(matrix_pso_test[nrow(matrix_pso_test), 1], 
                equals(0, tolerance  = 0.001))
  expect_that(matrix_pso_test[nrow(matrix_pso_test), 2], 
              equals(0, tolerance  = 0.001))
})
