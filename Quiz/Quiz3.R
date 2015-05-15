## Quiz3 Review
## Q1
## Take a look at the 'iris' dataset 
## that comes with R. The data can be loaded with the code:

library(datasets)
data(iris)

## There will be an object called 'iris' in your workspace. 
## In this dataset, what is the mean of 'Sepal.Length' for the species virginica? 
data_virginica <- split(iris, iris$Species)
data_virginica <- data_virginica[["virginica"]]
Result <- mean(data_virginica[["Sepal.Length"]])
Result

## easier way
Result <- lapply(split(iris,iris$Species), function(x) {mean(x[,"Sepal.Length"])})
Result

## Q2
## what R code returns a vector of the means of 
## the variables 'Sepal.Length', 'Sepal.Width', 
## 'Petal.Length', and 'Petal.Width'?

apply(iris[, 1:4], 2, mean)

## Q3
## Load the 'mtcars' dataset in R with the following code
library(datasets)
data(mtcars)
?mtcars
##  the average miles per gallon (mpg) by number 
## of cylinders in the car (cyl)?
tapply(mtcars$mpg, mtcars$cyl, mean)

## Q4
##  what is the absolute difference between 
## the average horsepower of 4-cylinder cars 
## and the average horsepower of 8-cylinder cars?
Result <- tapply(mtcars$hp, mtcars$cyl, mean)
abs(Result[["4"]]-Result[["8"]])

## Q5
## If you run
debug(ls)
## what happens when you next call the 'ls' function?
ls()
undebug(ls)