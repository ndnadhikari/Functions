## Author: Niranjan Adhikari

## Part 1: Factorial Function 

# Factorial of number n is  n * (n-1) * (n - 2) * . * 1. The factorial of 
# 0 is defined to be 1 and factorial of (-1) is not exist. There is limitation 
# of my code is that it is not expected to run well for a fractional number.

## Loop version 
Factorial_loop <- function(x=1) {
  if (x < 0) {
    print("Sorry, factorial does not exist for negative numbers")
  } else if (x == 0) {
    return(1)
  } else {
    y <- 1
    for ( i in 1:x) {
      y <- y*i
    }
    return(y)
  }
}

# Experiment of Loop
Factorial_loop(4)
Factorial_loop(0)
Factorial_loop(-5)
Factorial_loop(3.5) ##  could not work for fractions and so returns wrong output. 
Factorial_loop("Australia") ## NAs introduced by coercion. loop doesnot work for character. 


## If required download library 
if (!require('purrr', quietly = TRUE)) {
  stop('Please install the purrr package')
}

library(purrr) # this library is required for reduce() function.

## Reduce version 
Factorial_reduce <- function(x=1) {
  if (x < 0) {
    print("Sorry, factorial does not exist for negative numbers")
  } else if (x == 0) {
    return(1)
  } else {
    reduce(as.numeric(1:x), function(x,y) {
      x * y
    })
  }
}

# Experiment of reduce
Factorial_reduce(3)
Factorial_reduce(0)
Factorial_reduce(-1)

## Recursion Version

Factorial_func <- function(x=1) {
  if (x < 0) {
    print("Sorry, factorial does not exist for negative numbers")
  } else if(x == 0) {1}
  else {x * Factorial_func(x-1)
  }
}

# Experimentof recursion 
Factorial_func(3)
Factorial_func(0)
Factorial_func(-2)

## Memoization version

Factorial_mem <- function(x=1) {
  # stopifnot(x >= 0)
  if (x < 0) {
    print("Sorry, factorial does not exist for negative numbers")
  }
  Fact_tbl<- c(1)
  Factorial_mem_in <- function(x=1) {
    if(!is.na(Fact_tbl[x])) {Fact_tbl[x]}
    
    else{
      Fact_tbl[x] <<- x * Factorial_mem_in(x-1)
      return(Fact_tbl[x])}
  }
}

## sink() function produce the outoput of result as a text file
sink("factorial_output_experiment.txt")

## cat(function print out this line in output)
cat("Timing of different factorial functions for specific inputs \n")

library(microbenchmark) ## This pakage is used to compare the operation time 
                        # of the function. 
## Comparison for n=5 (smaller integer)
Factorial_performance_1 <- microbenchmark(a <- Factorial_loop(5),
                                          b <- Factorial_reduce(5),
                                          c <- Factorial_func(5),
                                          d <- Factorial_mem(5))
Factorial_performance_1

## Comparison for n=500 (larger integer)
Factorial_performance_2 <- microbenchmark(a <- Factorial_loop(500),
                                          b <- Factorial_reduce(500),
                                          c <- Factorial_func(500),
                                          d <- Factorial_mem(500))

Factorial_performance_2

## Note: While comparing the all of the above four different version of a 
# factorial function, we have found that the factorial loop is the fastest for smaller 
# integer while memorization version is fastest for larger integer. 

sink()
