##### Chapter 4: Classification using Naive Bayes --------------------

## Example: Filtering spam SMS messages ----
## Step 2: Exploring and preparing the data ---- 

# reference
# https://rpubs.com/hoakevinquach/SMS-Spam-or-Ham-Text
# https://rpubs.com/jesuscastagnetto/caret-naive-bayes-spam-ham-sms



setwd("D:/Workspace/R_Workspace/Data/chapter 4")

# read the sms data into the sms data frame
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE, encoding="UTF-8")

# examine the structure of the sms data
# str(sms_raw)
# View(sms_raw)
sms_raw$text[1072] <- sms_raw$type[1072]
sms_raw$type[1072] <- "ham"
replacePuctuation2space <- function(x){gsub("[[:punct:]]+", " ", x)}
sms_raw$text <- replacePuctuation2space(sms_raw$text)
rm(replacePuctuation2space)

# convert spam/ham to factor.
sms_raw$type <- factor(sms_raw$type)

# examine the type variable more carefully
# str(sms_raw$type)
# sms_raw$text[11]

# build a corpus using the text mining (tm) package
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))

# examine the sms corpus
# print(sms_corpus)
# inspect(sms_corpus[1:10])
# inspect(sms_corpus[11])


# clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

# corpus_clean <- tm_map(corpus_clean, stemDocument)   # ???ٸ? ????
removeURL <- function(x) gsub("httpalnum:*", "", x)
corpus_clean <- tm_map(corpus_clean, removeURL) # URL ��??
rm(removeURL)
rm(big5)

# examine the clean corpus
# inspect(sms_corpus[1:3])
# inspect(corpus_clean[1:3])
# View(corpus_clean)
# corpus_list = unlist(as.list(corpus_clean))
# View(corpus_list)
# rm(corpus_list)


# create a document-term sparse matrix
sms_dtm <- DocumentTermMatrix(corpus_clean)
rm(corpus_clean)
rm(sms_corpus)
# sms_dtm
# inspect(sms_dtm[1:5, 1:5])

# sms_dtm <- removeSparseTerms(sms_dtm, 0.90) #Sparsity?? 90% ?̻??? ???? ?ش? ?ܾ? ??��
# dic <- c("prices", "crude", "oil")      #--- ???? ?????? ?ܾ ?????ϴ? ???? ????
# sms_dtm <- DocumentTermMatrix(corpus_clean, list(dictionary = dic))


#------------------------------------------------
sms_mat <- TermDocumentMatrix(corpus_clean)
inspect(sms_mat)
sms_matrix <- as.matrix(sms_dtm)
#--------------------------------------------
# 
# 
# m <- TermDocumentMatrix(doc)          #--- TermDocumentMatrix ????
# m <- t(m)                             #--- DocumentTermMatrix?? ??ȯ
# data <- as.matrix(m)                  #--- DocumentTermMatrix?? matrix?? ??ȯ
# 
# m$dimnames                              #--- ???? (document)?? ?ܾ? (term) ????
# m$dimnames$Docs
# m$dimnames$Terms
# 
# inspect(m)                              #--- ?ܾ??? ?????? Ȯ??
# inspect(m[1:2, 3:5])                    #--- ó�� 2?? ?????? 3??°???? 5??° ?ܾ??? ?????? Ȯ??
# 
# findFreqTerms(m, 10)                    #--- 10ȸ ?̻? ?????? ?ܾ? ǥ??
# findFreqTerms(m, 10, 15)                #--- 10ȸ ?̻?, 15ȸ ???? ?????? ?ܾ? ǥ??
# findAssocs(m, "oil", 0.65)              #--- "oil" ?ܾ??? ??????(???? ?????? Ȯ??)?? 65% ?̻??? ?ܾ ǥ??



# install.packages("tidytext")
# library(tidytext)
# sms_df <- tidy(sms_mat)

# install.packages("lubridate")
# install.packages("quanteda")
# library(lubridate)
# library(quanteda)
# devtools::install_github("kbenoit/quanteda")
# sms_df <- cast_sparse(sms_dtm)


View(sms_dtm$dimnames)


sms<-apply(sms_dtm)
smsData<-as.data.frame(sms)
smsData$type <-smsData

smsDataCorpus <- VCorpus(VectorSource(smsData$text))



sms_dtm
nrow(sms_raw)
ncol(sms_matrix)

sms_matrix <- as.matrix(sms_dtm)
sms_df <- as.data.frame(sms_matrix, 	
                        row.names = rownames(sms_matrix),
                        col.names = colnames(sms_matrix),
                        optional= FALSE
                        )
# rm(sms_df)

sms_df <- cbind(sms_df,sms_raw$type)

ncol(sms_df)


all_sum <- as.data.frame(colSums(sms_df[-7435]),
                         row.names=NULL,
                         col.names=colnames(sms_df[-7435])
                         )
colnames(all_sum) = c("all_sum")
# sub_colnames <- rownames(subset(all_sum, all_sum < 80))
# all_sum[!(rownames(all_sum) %in% sub_colnames),]
# df[, !(colnames(df) %in% c("x","bar","foo"))]
# all_sum[ , -grep("\\.B$", names(dfrm)) ]


sms_sub_df <- sms_df[,-which(all_sum < 80)]
all_sum_sub <- all_sum[-which(all_sum < 80),]


index_fin <- ncol(sms_sub_df)
count <- colSums(sms_sub_df[-index_fin])
spam_sum <- colSums(subset(sms_sub_df[-index_fin],sms_raw$type == "spam"))
ham_sum <- colSums(subset(sms_sub_df[-index_fin],sms_raw$type == "ham"))
spam_rate <- spam_sum/all_sum_sub
ham_rate <- ham_sum/all_sum_sub
diff <- spam_rate-ham_rate
distance <- abs(spam_rate-ham_rate)


classify_word <- cbind(distance,diff, spam_rate, ham_rate,count)


View(classify_word)







# dis_df <-rbind(as.data.frame(distance,
#                                col.names = colnames(sms_sub_df[-index_fin])
#                                ),"NULL")
# temp_df <- cbind(t(sms_sub_df),dis_df)
# sms_top_df <- t(temp_df)
# colnames(sms_sub_df[index_fin])
# colnames(dis_df[index_fin]) = "sms_raw$type"
# dim(sms_top_df)






# creating training and test datasets
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test  <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_corpus_train <- corpus_clean[1:4169]
sms_corpus_test  <- corpus_clean[4170:5559]

# check that the proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))


# word cloud visualization
library(wordcloud)

wordcloud(sms_corpus_train, min.freq = 30, random.order = FALSE)

# subset the training data into spam and ham groups
spam <- subset(sms_raw_train, type == "spam")
ham  <- subset(sms_raw_train, type == "ham")



wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

#-------------------------------------------------------

# indicator features for frequent words
findFreqTerms(sms_dtm_train, 5)
sms_dict <- Dictionary(findFreqTerms(sms_dtm_train, 5))
sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test  <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0, 1, 0)
  x <- factor(x, levels = c(0, 1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test  <- apply(sms_test, MARGIN = 2, convert_counts)

## Step 3: Training a model on the data ----
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

## Step 4: Evaluating model performance ----
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

## Step 5: Improving model performance ----
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
