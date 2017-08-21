#' Particle Swarm Optimization Algorithm
#' 
#' This function performs optimization using PSO Algorithm. \cr
#' according to Edwin Chong and Stanisław Żak "An Introduction to Optimization" 
#' Fourth Edition and J. Dreo, A. Petrowski, P.Siarry, E.Taillard 
#' "Metaheuristics for Hard optimization"
#' 
#' This function was prepared for 2 dimensional problems,
#' so we set always here dim = 2 (dim - dimension ) \cr 
#' We control using: \cr
#' size - number of particles, \cr
#' min, max - minimum and maximum value of velocity and position, \cr 
#' 
#' @import stats
#' @import testthat
#' 
#' @param size a number.
#' @param dim a number.
#' @param min a number.
#' @param max a number.
#' 
#' @return a list.
#' 
#' @examples
#' PSO <- psopt:::pso_function(50, 2, 0, 10)

pso_function = function(size, dim, min, max){
  # schaffer function
  schaffer <- function(x){
    counter <- (sin(x[1]^2-x[2]^2)^2)-0.5
    nominative <- ( 1+0.001*(x[1]^2+x[2]^2))^2
    return( 0.5 + (counter / nominative))
  }

  # setting initial values of position and velocity
  set.seed(40)
  SWARM <- matrix(NA, nrow = size, ncol = dim*2)
  for ( i in 1:size ) {
    for ( j in 1:(2*dim)) {
      SWARM[i, j] <- runif(1, min, max)
    }
  }
  # checking schaffer function value using position argument 
  FITNESS <- matrix(NA, nrow = size, ncol = 1)
  for ( i in 1:size){
    FITNESS[i,] <- schaffer(SWARM[i, c(1, 2)])
  }

  # global and individual best position
  personal_best <- SWARM[, c(1, 2)]
  global_best <- personal_best[which.min(FITNESS), ]

  history <- global_best
  iteration_time <- vector()

  # constant values
  omega <- runif(1, 0, 1)
  c1 <- runif(1, 0, 2)
  c2  <- runif(1, 0, 2)

  result <- list( history = c(), iteration_time = c())

  # updating values of position and velocity
  for (k in 1:1000) {
    starT <- Sys.time()
    rk <- runif(2, 0, 1)
    sk <- runif(2, 0, 1)

    # calculating new value of position and velocity
    SWARM[, c(3,4)] <- omega * SWARM[, c(3,4)] + c1 * rk * (personal_best[] - SWARM[, c(1,2)]) + c2 *sk * (global_best[] - SWARM[, c(1,2)] )
    SWARM[, c(1,2)] <- SWARM[, c(1,2)] + SWARM[, c(3,4)]

    # checking if we get new personal best value
    for (i in 1:size){
      current_fitness <- schaffer(SWARM[i, c(1,2)])
      if (current_fitness < FITNESS[i]) {
        FITNESS[i] <- current_fitness
        # 
        personal_best[i,1] <- SWARM[i, 1]
        personal_best[i,2] <- SWARM[i, 2]
        
      }
    }

    global_best <- personal_best[which.min(FITNESS), ]

    # history of space exploring + space exploring time
    # print(global_best)
    history <- c(history, global_best)
    stopT <- Sys.time()
    time_taken <- stopT - starT
    iteration_time <- c(iteration_time, time_taken)

  }
  result$history <- history
  result$iteration_time <- iteration_time
  return(result)
}

#' Schaffer function
#' 
#' This function belongs to group of test functions for optimization. \cr
#' Shaffer functions takes two parameters. \cr
#' More function you can find here - \cr 
#' \url{https://en.wikipedia.org/wiki/Test_functions_for_optimization}
#' 
#' @param x two dimensional vector.
#' @return a value of schaffer function
#' 
#' @examples
#' psopt:::schaffer(c(0,0))

schaffer = function(x){
  counter <- (sin(x[1]^2-x[2]^2)^2)-0.5
  nominative <- ( 1+0.001*(x[1]^2+x[2]^2))^2
  return( 0.5 + (counter / nominative))
}

#' Explore graph
#' 
#' This function prepares graph, which presents how the function ("pso_function")\cr
#' explore the space of possible solutions, to find the best solution. \cr
#' It should be called after calling pso_function.
#' 
#' @import graphics 
#' 
#' @param PSO a list.
#' @return a graph
#' 
#' @examples
#' PSO <- psopt:::pso_function(50, 2, 0, 10)
#' psopt:::explore_graph(PSO)

explore_graph = function(PSO) {

  history <- PSO[1]
  output <- matrix(unlist(history), ncol = 2, byrow = TRUE)
  n_grid=100
  x_seq <- seq(-10, 10, length = n_grid)
  matrVal <- matrix(0, nrow = n_grid, ncol = n_grid)
  for(iRow in 1 : n_grid){
    for(iCol in 1 : n_grid){
      matrVal[iRow, iCol] <- schaffer(c(x_seq[iRow], x_seq[iCol]))
    }
  }
  contour(x_seq, x_seq, matrVal)
  lines(output, col = 'green', type = 'l', lwd=2)
}

#' Execution time of an iteration \cr
#' 
#' This function prepares a chart, which presents how long \cr
#' ech iteration of pso_function was calculated.  \cr
#' It should be called after calling pso_function.
#' 
#' @import graphics 
#' 
#' @param PSO a list.
#' @return a chart.
#' 
#' @examples
#' PSO <- psopt:::pso_function(50, 2, 0, 10) 
#' psopt:::iter_time(PSO)

iter_time = function(PSO){
  iter_time <- data.frame(PSO[2])
  plot(iter_time$iteration_time, xlab = "iteration", ylab = "time [sec]")
}
