### Spam/Ham 구분에 대한 중요단어 10개 찾기  

setwd("D:/Workspace/R_Workspace/Data/chapter 4")

# sms data 불러오기
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE, encoding="UTF-8")
# View(sms_raw)

# 오류데이터 수정 
sms_raw$text[1072] <- sms_raw$type[1072]
sms_raw$type[1072] <- "ham"

# convert spam/ham to factor. type 자료형을 character -> factor 로 변환
sms_raw$type <- factor(sms_raw$type)

# Cleaning 전처리 작업
replacePuctuation2space <- function(x){gsub("[[:punct:]]+", " ", x)} 
sms_raw$text <- replacePuctuation2space(sms_raw$text)
# ...이전 이후의 단어가 서로 붙는것을 방지
rm(replacePuctuation2space)

# build a corpus using the text mining (tm) package
library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))

# clean up the corpus using tm_map()
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)

sms_dtm <- DocumentTermMatrix(corpus_clean)
# sms_dtm


# library(tidytext)
# sms_df_miss <- tidy(sms_dtm)


# DocumentTermMatrix를 dataframe으로 변환
sms_matrix <- as.matrix(sms_dtm)
sms_df <- as.data.frame(sms_matrix)


# 각 단어개를 count 하여 80개 이상의 단어들만 선택
all_sum <- as.data.frame(colSums(sms_df))
sms_df <- cbind(sms_df, sms_raw$type)
sms_df_top <- sms_df[,-which(all_sum < 80)]


# 각 spamham rate 계산 후 단어별 ditance 도출
type_col <- ncol(sms_df_top)
count <- colSums(sms_df_top[-type_col])
spam_sum <- colSums(subset(sms_df_top[-type_col],sms_raw$type == "spam"))
ham_sum <- colSums(subset(sms_df_top[-type_col],sms_raw$type == "ham"))
spam_rate <- spam_sum/count
ham_rate <- ham_sum/count
distance <- abs(spam_rate-ham_rate)

classify_word <- cbind(distance, spam_rate, ham_rate,count)
View(classify_word)

important_word <- head(rownames(classify_word[order(-distance),]),10)
most_spam_word <- head(rownames(classify_word[order(-spam_rate),]),10)
most_ham_word  <- head(rownames(classify_word[order(-ham_rate),]),10)
most_cnt_word  <- head(rownames(classify_word[order(-count),]),10)

distance10  <- round(classify_word[order(-distance),][1:10,1], digits = 2)
spam_rate10 <- round(classify_word[order(-spam_rate),][1:10,2], digits = 2)
ham_rate10  <- round(classify_word[order(-ham_rate),][1:10,3], digits = 2)
count10     <- round(classify_word[order(-count),][1:10,4], digits = 2)

result <- cbind(important_word,distance10,most_spam_word,spam_rate10,
                most_ham_word,ham_rate10,most_cnt_word,count10)
colnames(result) <- c("important_word","dist","most_spam_word","rate",
                      "most_ham_word","rate","most_cnt_word","count")
View(result)
