

##### Part 1: Neural Networks -------------------
## Example: Modeling the Strength of Concrete  ----
## Step 2: Exploring and preparing the data ----
# read in data and examine structure

setwd("D:/Workspace/R_Workspace/Data/chapter 7")
letterdata <- read.csv("letterdata.csv")

str(letterdata)

# custom normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
# apply normalization to entire data frame
letter_features_norm <- as.data.frame(lapply(letterdata[2:17], normalize))

# confirm that the range is now between zero and one
summary(letter_features_norm$xbox)
# compared to the original minimum and maximum
summary(letterdata)

# create training and test data
letter_features_train <- letter_features_norm[1:15000, ]
letter_label_train <- letterdata[1][1:15000, ]

letter_features_test <- letter_features_norm[15001:20000, ]
letter_label_test <- letterdata[1][15001:20000, ]


## Step 3: Training a model on the data ----
# train the neuralnet model


library(nnet)
t.start <- Sys.time()
nnet_model <- nnet(letter_label_train ~ ., data=letter_features_train, 
                   size = 22, decay=5e-4, maxit=5000)
t.end <- Sys.time()
t.end-t.start


pred_train <- predict(nnet_model, letter_features_train, type="class")
train_table <- table(letter_label_train, pred_train)
sum(diag(train_table)/sum(train_table))


pred_test <- predict(nnet_model, letter_features_test, type="class")
test_table <- table(letter_label_test, pred_test)
sum(diag(test_table)/sum(test_table))


table(letter_label_test, pred_test)
table(letter_label_train, pred_train)
