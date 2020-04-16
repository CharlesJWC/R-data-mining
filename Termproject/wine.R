library(C50)     # C5.0을 사용하기 위한 라이브러리
library(gmodels) # CrossTable을 사용하기 위한 라이브러리
library(caret)   # UpSampling, DownSampling을 위한 라이브러리

#####################################################################
## Step 1 : 데이터 불러오기 

setwd("D:/Workspace/R_Workspace/Data/chapter 5")
mushrooms <- read.csv("mushrooms.csv", stringsAsFactors = TRUE)

# 한가지 값만 있는 값 제거
mushrooms$veil_type <- NULL

# mushrooms data 분포 확인
table(mushrooms$type)
prop.table(table(mushrooms$type))

######################################################################
## Step 2 : train / test 데이터 분리하기
# 최적의 train / test 비율? ->  7:3 / 8:2(선택)

# train / test data 분리 (8:2) 
# 분리 방법 1
set.seed(7777)
total_num = nrow(mushrooms)
mushrooms_rand <- mushrooms[order(runif(total_num)), ]
train_num = round(nrow(mushrooms) * 0.8)
mushrooms_train <- mushrooms_rand[1:train_num, ]
mushrooms_test  <- mushrooms_rand[(train_num+1):total_num, ]

# 분리 방법 2 
# 매 수행마다 확률적으로 개수가 달라질 수 있음
idx <- sample(2, nrow(mushrooms), replace = TRUE, prob = c(0.8, 0.2))
mushrooms_train <- mushrooms[idx == 1, ]
mushrooms_test <- mushrooms[idx == 2, ]

# train / test data 분포 확인
table(mushrooms_train$type)
table(mushrooms_test$type)
prop.table(table(mushrooms_train$type))
prop.table(table(mushrooms_test$type))

######################################################################
## Step 3 : train 데이터를 통한 Decision tree modeling
# build the simplest decision tree

# Train 데이터를 통한  Decision tree model 생성
mushrooms_model <- C5.0(mushrooms_train[-1], mushrooms_train$type) 

# Decision tree modeling 결과 확인 
mushrooms_model           # display simple facts about the tree
summary(mushrooms_model)  # display detailed information about the tree
plot(mushrooms_model, type = "simple")

#####################################################################
## Step 4: Decision tree model 성능 평가 및 검증

# Decision tree를 통한 Train data 결과 확인 
mushrooms_train_pred <- predict(mushrooms_model, newdata = mushrooms_train)
confusionMatrix(mushrooms_train_pred, mushrooms_train$type)
# cross tabulation of predicted versus actual classes
CrossTable(mushrooms_train$type, mushrooms_train_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual type', 'predicted type'))

# Decision tree를 통한 Test data 결과 확인
mushrooms_test_pred <- predict(mushrooms_model, newdata = mushrooms_test)
confusionMatrix(mushrooms_test_pred, mushrooms_test$type)
CrossTable(mushrooms_test$type, mushrooms_test_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual type', 'predicted type'))

#####################################################################
## Step 5: Decision tree model 성능 향상을 위한 방법
# (Miss matching Error를 줄일 방법)

# 방법 1 : Boosting Iterations (에러 데이터 위주로 다시 학습)
## Boosting the accuracy of decision trees

# boosted decision tree with 10 trials
mushrooms_boost10 <- C5.0(mushrooms_train[-1], mushrooms_train$type, 
                          trials = 10)
mushrooms_boost10
mushrooms_boost10_pred <- predict(mushrooms_boost10, newdata = mushrooms_test)
CrossTable(mushrooms_test$type, mushrooms_boost10_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual type', 'predicted type'))


# 방법 2 : Cost based Decision Making
# 특정 Error에 Cost를 부과하는 방법
# create a cost matrix
matrix_dimensions <-list(c("edible", "poisonous"), c("edible", "poisonous"))
names(matrix_dimensions) <-c("predicted", "actual")
error_cost <- matrix(c(0, 1, 2, 0), nrow = 2, dimnames = matrix_dimensions)
# apply the cost matrix to the tree
mushrooms_cost <- C5.0(mushrooms_train[-1], mushrooms_train$type, 
                       costs = error_cost)
mushrooms_cost
mushrooms_cost_pred <- predict(mushrooms_cost, newdata = mushrooms_test)
CrossTable(mushrooms_test$type, mushrooms_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual type', 'predicted type'))


## 방법 3 : Up/Down sampling을 통한 레이블된 데이터 비율 동일하게 만들기
mushrooms_train_upsample<-upSample(subset(mushrooms_train, select=-type),mushrooms_train$type)
names(mushrooms_train_upsample)[22] <-"type"
mushrooms_train_upsample = cbind(mushrooms_train_upsample[22],mushrooms_train_upsample[-22])

# 데이터 전후 비교
table(mushrooms_train$type)
table(mushrooms_train_upsample$type)

mushrooms_upsampling <- C5.0(mushrooms_train_upsample[-1], mushrooms_train_upsample$type) 
mushrooms_upsampling
mushrooms_upsampling_pred <- predict(mushrooms_upsampling, newdata = mushrooms_test)
CrossTable(mushrooms_test$type, mushrooms_upsampling_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual type', 'predicted type'))

######################################################################