

setwd("D:/Workspace/R_Workspace/Termproject/Data")
cowData <- read.csv("cow_dataset_label.csv")
str(cowData)

setwd("D:/Workspace/R_Workspace/Termproject/Data")
wine_red <- read.csv("winequality-red.csv")
summary(wine_red)

wine_white <- read.csv("winequality-white.csv")
summary(wine_white)




library(dplyr)
wine_white <- wine_white %>%
  mutate(label = ifelse(quality >= 7, "good", "bad"))
  
summary(wine_white)


idx = sample(2, nrow(wine_white), replace = TRUE, prob = c(0.8,0.2))
wine_train <- wine_white[idx == 1,]
wine_test <- wine_white[idx == 2,]

library(C50) 
wine_DT_model <- C5.0(wine_train[1:11], wine_train[12])

head(wine_train[13])


prop.table(table(cow_train$label))
prop.table(table(cow_test$label))



wine_train$label <- factor(wine_train[13],c("good","bad"))
wine_test$label <- factor(wine_test[13],c("good","bad"))
summary(wine_train$label)


?read.csv
library(gmodels) # CrossTable을 사용하기 위한 라이브러리
library(caret)   # UpSampling, DownSampling을 위한 라이
wine_test$pred <- predict(wine_train_label,newdata=wine_test)
confusionMatrix(wine_test$pred,wine_test_label)
CrossTable(wine_test_label, wine_test$pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual label', 'predicted label'))


table(wine_train$label)
prop.table(table(mushrooms$type))


table(cowData$F1)
table(cowData$F2)
table(cowData$F3)
table(cowData$F4)

table(cowData$label)
prop.table(table(cowData$label))

summary(cowData)

idx = sample(2, nrow(cowData), replace = TRUE, prob = c(0.8,0.2))
cow_train <- cowData[idx == 1,]
cow_test <- cowData[idx == 2,]

prop.table(table(cow_train$label))
prop.table(table(cow_test$label))


library(C50)     # C5.0을 사용하기 위한 라이브러리
library(gmodels) # CrossTable을 사용하기 위한 라이브러리
library(caret)   # UpSampling, DownSampling을 위한 라이브러리

cow_DT_model <- C5.0(cow_train[-7], cow_train$label)

summary(cow_DT_model)

plot(cow_DT_model, type ="simple")
cow_test$pred <- predict(cow_DT_model,newdata=cow_test)
confusionMatrix(cow_test$pred,cow_test$label)
CrossTable(cow_test$label, cow_test$pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual label', 'predicted label'))

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
# apply normalization to entire data frame
letter_features_norm <- as.data.frame(lapply(letterdata[2:17], normalize))



library(nnet)
t.start <- Sys.time()
nnet_model <- nnet(cow_train$label ~ ., data=cow_train[-7], 
                   size = 22, decay=5e-4, maxit=100)
t.end <- Sys.time()
t.end-t.start


pred_train <- predict(nnet_model, cow_train[-7], type="class")
train_table <- table(cow_train$label, pred_train)
sum(diag(train_table)/sum(train_table))



pred_test <- predict(nnet_model, cow_test[-7], type="class")
test_table <- table(cow_test$label, pred_test)
sum(diag(test_table)/sum(test_table))


table(cow_test$label, pred_test)
table(cow_train$label, pred_train)
