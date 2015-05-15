##  Quiz2 Review
##  Q1
cube <- function(x, n) {
  x^3
}
cube(3)

## Q2 The following code will produce a warning in R.
## Why
## the condition has length > 1 
## and only the first element will be used
x <- 1:10
if(x > 5) {
  x <- 0
}

## Q3
f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}
z <- 10
f(3)

## Q4 What is the value of 'y' after 
## evaluating this expression?
x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}

## Q5 Which symbol in the above function is a free variable?
h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}

## Q6
## What is an environment in R?
## a collection of symbol/value pairs

## Q7
## The R language uses what type of scoping rule 
## for resolving free variables?
## lexical scoping

## Q8
## How are free 
## variables in R functions resolved?
## The values of free variables are searched 
## for in the environment in which the function 
## was defined

## Q9
## All objects must be stored in memory

## Q10
## In R, what is the parent frame?
## It is the environment in which 
## a function was called