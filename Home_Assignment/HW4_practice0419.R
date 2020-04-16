# c5.0 함수의 옵션을 미리 설정
c50_options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE) 
# winnow : 특징들을 미리 거를지 여부
# noGlobalPruning : Decision tree가 과도하게 크지 않도록 모델링 후 가지를 칠지 여부(Post Pruning)

# Train 데이터를 통한  Decision tree model 생성
mushrooms_model <- C5.0(type ~ ., data = mushrooms_train, control=c50_options, rules=FALSE)
####################################################################3


setwd("D:/Workspace/R_Workspace/Data/chapter 5")

mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)

set.seed(7777)
total_num = nrow(mushrooms)
mushrooms_rand <- mushrooms[order(runif(total_num)), ]
train_num = round(nrow(mushrooms) * 0.7)

# split the data frames
mushrooms_train <- mushrooms_rand[1:train_num, ]
mushrooms_test  <- mushrooms_rand[(train_num+1):total_num, ]
# train과 test 비율? ->  7:3(선택) / 8:2 / 9:1

table(mushrooms$type)
table(mushrooms_train$type)
table(mushrooms_test$type)
nrow(mushrooms_train)
nrow(mushrooms_test)

# check the proportion of class variable
prop.table(table(mushrooms_train$type))
prop.table(table(mushrooms_test$type))



## Step 3: Training a model on the data ----
# build the simplest decision tree

# install.packages("C50")
library(C50)
mushrooms_model <- C5.0(mushrooms_train[-1], mushrooms_train$type) # 첫번째 열이 판단할 정답

# display simple facts about the tree
mushrooms_model
# display detailed information about the tree
summary(mushrooms_model)

## Step 4: Evaluating model performance ----
# create a factor vector of predictions on test data
mushrooms_pred <- predict(mushrooms_model, mushrooms_test)

# cross tabulation of predicted versus actual classes
library(gmodels)
CrossTable(mushrooms_test$type, mushrooms_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual type', 'predicted type'))

## Step 5: Improving model performance ----

## Boosting the accuracy of decision trees
# boosted decision tree with 10 trials
mushrooms_boost10 <- C5.0(mushrooms_train[-1], mushrooms_train$type,
                          trials = 10)
mushrooms_boost10
summary(mushrooms_boost10)

mushrooms_boost_pred10 <- predict(mushrooms_boost10, mushrooms_test)
CrossTable(mushrooms_test$type, mushrooms_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

# boosted decision tree with 100 trials (not shown in text)
mushrooms_boost100 <- C5.0(mushrooms_train[-1], mushrooms_train$type,
                           trials = 100)
mushrooms_boost_pred100 <- predict(mushrooms_boost100, mushrooms_test)
CrossTable(mushrooms_test$type, mushrooms_boost_pred100,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## Making some mistakes more costly than others
# create a cost matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
error_cost

# apply the cost matrix to the tree
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                    costs = error_cost)
credit_cost_pred <- predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))



install.packages("caret")
library(caret)

?C5.0

plot(mushrooms_model, type = "simple")
confusionMatrix(train_1$pred, train_1$Species)

############################################################

library(C50)     # C5.0을 사용하기 위한 라이브러리
library(gmodels) # CrossTable을 사용하기 위한 라이브러리
library(caret)

# train과 test 비율? ->  7:3 / 8:2(선택)

# train / test data 분리 (8:2) 
# 방법 1
idx <- sample(2, nrow(mushrooms), replace = TRUE, prob = c(0.8, 0.2))
mushrooms_train <- mushrooms[idx == 1, ]
mushrooms_test <- mushrooms[idx == 2, ]

# 방법 2
set.seed(7777)
total_num = nrow(mushrooms)
mushrooms_rand <- mushrooms[order(runif(total_num)), ]
train_num = round(nrow(mushrooms) * 0.8)
mushrooms_train <- mushrooms_rand[1:train_num, ]
mushrooms_test  <- mushrooms_rand[(train_num+1):total_num, ]

# modeling
c5_options <- C5.0Control(winnow = FALSE, noGlobalPruning = FALSE)
mushrooms_model <- C5.0(type ~ ., data = mushrooms_train, control=c5_options, rules=FALSE)
summary(mushrooms_model)

plot(mushrooms_model,type = "simple")

# validation
mushrooms_train$pred <- predict(mushrooms_model, newdata = mushrooms_train)
confusionMatrix(mushrooms_train$pred, mushrooms_train$type)
CrossTable(mushrooms_test$type, mushrooms_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual type', 'predicted type'))

mushrooms_test$pred <- predict(mushrooms_model, newdata = mushrooms_test)
confusionMatrix(mushrooms_test$pred, mushrooms_test$type)
CrossTable(mushrooms$type, mushrooms_test$pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual type', 'predicted type'))

table(mushrooms_train$type)

#에러를 줄일 방법


## Boosting the accuracy of decision trees
# boosted decision tree with 10 trials
mushrooms_boost10 <- C5.0(mushrooms_train[-1], mushrooms_train$type,
                          trials = 10)
mushrooms_boost_pred10 <- predict(mushrooms_boost10, mushrooms_test)
CrossTable(mushrooms_test$type, mushrooms_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

## Making some mistakes more costly than others
# create a cost matrix
error_cost <- matrix(c(0, 1, 4, 0), nrow = 2)
# apply the cost matrix to the tree
credit_cost <- C5.0(credit_train[-17], credit_train$default,
                    costs = error_cost)


## Upsampling을 통한 데이터 공평하게 만들기
mushrooms_train_upsample<-upSample(subset(mushrooms_train, select=-type),mushrooms_train$type)
names(mushrooms_train_upsample)[22] <-"type"
table(mushrooms_train_upsample$type)

############################################################

library(C50)
mushroom_c5rules <- C5.0(type ~ odor + gill_size, data = mushrooms, rules = TRUE)
summary(mushroom_c5rules)