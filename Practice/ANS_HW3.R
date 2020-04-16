
setwd("D:/Workspace/R_Workspace/Data/chapter 4")
sms_raw <- read.csv("sms_spam.csv", stringsAsFactors = FALSE, encoding="UTF-8")
sms_raw$text[1072] <- sms_raw$type[1072]
sms_raw$type[1072] <- "ham"
sms_raw$type <- factor(sms_raw$type)
replacePuctuation2space <- function(x){gsub("[[:punct:]]+", " ", x)} 
sms_raw$text <- replacePuctuation2space(sms_raw$text)
rm(replacePuctuation2space)

library(tm)
sms_corpus <- Corpus(VectorSource(sms_raw$text))
corpus_clean <- tm_map(sms_corpus, tolower)
corpus_clean <- tm_map(corpus_clean, removeNumbers)
corpus_clean <- tm_map(corpus_clean, removeWords, stopwords())
corpus_clean <- tm_map(corpus_clean, removePunctuation)
corpus_clean <- tm_map(corpus_clean, stripWhitespace)
sms_dtm <- DocumentTermMatrix(corpus_clean)

sms_labels <- sms_raw$type

sms_dtm_freq <- removeSparseTerms(sms_dtm, 0.999)
sms_freq_words <- findFreqTerms(sms_dtm,5)
sms_dtm_freq <- sms_dtm[,sms_freq_words]

convert_counts <- function(x){
  x<- ifelse(x>0,x,0)
}

sms <- apply(sms_dtm_freq, MARGIN = 2)#,  )
smsData <- as.data.frame(sms)
smsData$TYPE <- sms_raw$type

# 리눅스로 깔면 엄청빠름

smsDataName <- colnames(smsData)
which(smsDataName %in% "free") # where is free ?
temp <- t.test(smsData[,450]~smsData$TYPE)
# temp.statistic -> t-value

which(smsDataName %in% "TYPE")

impText <- function(){
  imp<-data.frame()
  for (i in 1:1406){
    temp <- t.test(sms_Data[,i]~smsData$TYPE)
    imp <- rbind(imp, cbind(i,temp$statistic[[1]],temp$p.value))
  }
}

imp <- impText()
impSort<-imp[order(imp$V3),]
impSort<-impSort[V2 < 0] 

# install.packages("sqldf")
library(sqldf)
impRes<-sqldf("select * from impSort where V2 < 0")
impRes[c(1:10),1]
impSort[c(1:10),1]
smsDataName[impSort[c(1:10),1]]
smsDataName[impRes[c(1:10),1]]