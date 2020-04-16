
setwd("D:/Workspace/R_Workspace/Mnist")

mnist_train<-read.csv("mnist_train.csv", header = FALSE)
names(mnist_train)[1]<-paste("y")
mnist_test<-read.csv("mnist_test.csv", header = FALSE)
names(mnist_test)[1]<-paste("y")
library(nnet)
normalize <-function(x) {return(x/255)}
MnistTrainNorm<-as.data.frame(lapply(mnist_train[,-1], normalize))
MnistTrainNorm$y<-as.factor(mnist_train$y)
MnistTestNorm<-as.data.frame(lapply(mnist_test[,-1], normalize))

library(caret)
#nearZeroVar(MnistTrainNorm,saveMetrics= TRUE)
keeps<-paste("V", c(154:161, 180:190, 207:219, 234:247, 262:275, 290:303, 318:331,
                    345:358, 373:386, 401:414, 429:443, 457:471, 485:498, 513:526,
                    541:554, 569:581, 597:609, 626:635, 655:662, 685:687), sep="")
keeps1<-append("y",keeps)
samp<-c(sample(1:60000,10000))
MnistTrainNorm<-MnistTrainNorm[samp,keeps1]
MnistTestNorm<-MnistTestNorm[,keeps]
memory.limit(40000)
t.start<-Sys.time()
nnModel<-nnet(y~., data=MnistTrainNorm, size = 50, MaxNWts=39301)
t.end<-Sys.time()
t.end - t.start

y<-as.factor(mnist_test$y)
p<-predict(nnModel, MnistTestNorm, type="class")
table(y,p)
sum(diag(table(y,p)))/sum(table(y,p))
