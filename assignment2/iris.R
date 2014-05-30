library(datasets)
data(iris)
?iris
s <-split(iris, iris$Species)
lapply(s, function(x) colMeans(x[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]))
apply(iris[, 1:4], 2, mean)
libraty(datasets)
data(mtcars)
#How can one calculate the average miles per gallon (mpg) by number of cylinders in the car (cyl)?
s <-split(mtcars, mtcars$cyl)
lapply(s, function(x) colMeans(x[, c("mpg", "disp", "hp")]))
sapply(split(mtcars$mpg, mtcars$cyl), mean)
#what is the absolute difference between the average horsepower of 4-cylinder cars and
#the average horsepower of 8-cylinder cars?
sapply(split(mtcars$hp, mtcars$cyl), mean)
