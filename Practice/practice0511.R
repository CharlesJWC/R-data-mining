# install.packages("neuralnet")
library(neuralnet)
library(nnet) # using class.ind
data(iris)
species.ind <- class.ind(iris$Species)
iris <- cbind(iris, species.ind)
samp <- c(sample(1:50,25), sample(51:100,25),sample(101:150,25))
iris.tr<-iris[samp,]
iris.te<-iris[-samp,]
ir1 <- neuralnet(setosa+versicolor+virginica~Sepal.Length + Sepal.Width + Petal.Length +
                   Petal.Width, hidden=2, data=iris.tr)
ir2 <- neuralnet(setosa+versicolor+virginica~Sepal.Length + Sepal.Width + Petal.Length +
                   Petal.Width, hidden=c(3,3), data=iris.tr)
plot(ir1)
plot(ir2)
