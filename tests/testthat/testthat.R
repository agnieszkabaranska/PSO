library(psopt)
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
 
 # test of explore graph
 with_mock(

   `base::lines` = function(output, col, type, lwd) {
     expect_that(col, equals('green'))
     expect_that(type, equals('l'))
     expect_that(lwd, equals(2))
   },
   psopt:::explore_graph(list(c(1, 1, 2, 2), c(1, 2)))
 )
 
# test of iter time function
 with_mock(
   `graphics::plot` = function(x, xlab, ylab) {
     expect_that(xlab, equals('iteration'))   
     },
   psopt:::iter_time(list(c(1, 1, 1, 2), c(1, 2)))
 )
 
 