##### Chapter 3: Classification using Nearest Neighbors --------------------

## Example: Classifying Cancer Samples ----
## Step 2: Exploring and preparing the data ---- 

setwd("D:/Workspace/R_Workspace/Data/chapter 3")

# import the CSV file
wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

impCalc <- function(){
  imp<-data.frame()
  for(i in 3:32){
    temp<-t.test(wbcd[,i]~wbcd$diagnosis) # wbcd[,i] 종속변수 ,wbcd$diagnosis 독립변수
    imp<-rbind(imp,cbind(i,temp$p.value))
  }
  return(imp)
}

imp <-impCalc()
View(imp)
View(wbcd)



# examine the structure of the wbcd data frame
str(wbcd)

# drop the id feature
wbcd <- wbcd[-1]

# table of diagnosis
table(wbcd$diagnosis)

# recode diagnosis as a factor
wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

# table or proportions with more informative labels
round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

# summarize three numeric features
summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
normalize(c(1, 2, 3, 4, 5))
normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize)) # ---------------------------------??

# confirm that normalization worked
summary(wbcd_n[c("radius_mean", "area_mean", "smoothness_mean")])
summary(wbcd_n$area_mean)

# create training and test data
wbcd_train <- wbcd_n[1:469, ] # 열에 아무것도 안주면 모든 열을 다 Select 하는 
wbcd_test <- wbcd_n[470:569, ]

# create labels for training and test data

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

## Step 3: Training a model on the data ----

# load the "class" library
library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, # ---------------------------------??
                      cl = wbcd_train_labels, k=1)#k=21)
# k = 1  유클리디언 거리계산 제일 작은거 하나를 찾아라 
# k = 3  유클리디언 거리계산 제일 작은거 3개를 찾아라 (레이블 다수결로 결정) 

## Step 4: Evaluating model performance ----

# load the "gmodels" library
library(gmodels)
# install.packages("gmodels")

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, # ---------------------------------??
           prop.chisq=FALSE)

## Step 5: Improving model performance ----

# use the scale() function to z-score standardize a data frame
wbcd_z <- as.data.frame(scale(wbcd[-1])) # ---------------------------------??

# confirm that the transformation was applied correctly
summary(wbcd_z$area_mean)

# create training and test datasets
samp <- sample(c(1:569), 100)# 랜덤하게 100개 뽑기
wbcd_train <- wbcd_z[-samp, ]
wbcd_test <- wbcd_z[samp, ]
wbcd_train_labels <- wbcd[-samp, 1]
wbcd_test_labels <- wbcd[samp, 1]

# wbcd_train <- wbcd_z[1:469, ]
# wbcd_test <- wbcd_z[470:569, ]

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=5)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

# try several different values of k
wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
