data(cars)
View(cars)
plot(cars$speed, cars$dist)
library(MASS)
boxcox(lm(dist~speed,data=cars),lambda=seq(0,1,by=.1))
cars$dist1<-(cars$dist^0.5 -1)/0.5
plot(cars$speed, cars$dist1)
summary(lm(dist~speed, data=cars))
summary(lm(dist1~speed, data=cars))

###############################################

data(iris)
lmout<-lm(Sepal.Length~Sepal.Width+Petal.Length+Petal.Width, data=iris)
summary(lmout)
