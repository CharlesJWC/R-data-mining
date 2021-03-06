library(twitteR)
library(base64enc)
library(ROAuth)
#library(RCurl)                   이전 버전

#request_token_URL= "https://api.twitter.com/oauth/request_token"   이전 버전
#authrize_URL ="https://api.twitter.com/oauth/authorize"            이전 버전
#access_token_URL= "https://api.twitter.com/oauth/access_token"     이전 버전

# 트위터 키확인: https://apps.twitter.com/app/15008019/keys

consumer_key <- ""
consumer_secret <- "" 
access_token <- ""
access_secret <- ""
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

search_string <- enc2utf8('아이코스')    # 아이코스라는 단어가 언급된 트윗 검색
num_tweets <- 1000                      # 가져올 숫자 지정 : 1000개
keyword <- "#sex"
tweets_ML <- searchTwitter(keyword, n=25, since='2016-01-01',
                            lang = "ko"
                            #geocode='35.874,128.246,400km'
)

# 트위터 데이터를 형태별로 분류하고 멘션 부분만 추출
tweets_ML.df <- twListToDF(tweets_ML)
tweets_ML.text <- tweets_ML.df$text
View(tweets_ML.text)

# 불필요한 문자를 필터링
tweets_ML.text <- gsub("RT @", "", tweets_ML.text)
tweets_ML.text <- gsub(".+: ", "", tweets_ML.text)
tweets_ML.text <- gsub('http.+','', tweets_ML.text)         # 페이지 링크 제거
tweets_ML.text <- gsub('youtu.+','', tweets_ML.text)        # 유투브 링크 제거
tweets_ML.text <- gsub("좋아요", "", tweets_ML.text)
tweets_ML.text <- gsub("리트윗", "", tweets_ML.text)
tweets_ML.text <- gsub("팔로우", "", tweets_ML.text)
tweets_ML.text <- gsub("\\<U\\+275D\\>", "", tweets_ML.text)

tweets_ML.text <- gsub('[~!@#$%&*()_+=?<>.,-]','', tweets_ML.text) # 특수문자 제거 


kakaotxt <- gsub("\\d+",'', kakaotxt)      # 숫자제거
kakaotxt <- gsub('[a-zA-z]','',kakaotxt)   # 영문 제거


library(KoNLP)        # 말뭉치 추출 라이브러리
library(wordcloud)    # 워드클라우드 라이브러리
library(wordcloud2)   # 워드클라우드 라이브러리2
library(RColorBrewer) # 워드클라우드 색상 라이브러리
useSejongDic()

# 문자 분리
tweets_ML_nouns <- Map(extractNoun, tweets_ML.text)
twitter_noun <- sapply(twitter_text, extractNoun, USE.NAMES = F)
View(tweets_ML_word)
# 쓸모없는 문자들을 제거한다. 특히 영문자의 경우 tm의 stopwords를 활용한다. 
tweets_ML_word <- unlist(tweets_ML_nouns, use.name=F)
tweets_ML_word <- tweets_ML_word[-which(tweets_ML_word %in% stopwords("english"))]
# 15 글자 이상 단어 제외
twitter_noun2 <- gsub("[ㄱ-ㅎ가-힣]{15,}", "", twitter_noun2)

tweets_ML_word <- Filter(function(x){nchar(x)>=2}, tweets_ML_word)
twitter_noun2 <- gsub("\\d+", "", twitter_noun2)
twitter_noun2 <- gsub("\\(", "", twitter_noun2)
twitter_noun2 <- gsub("\\)", "", twitter_noun2)   

# 단어별 카운팅
tweets_ML_count <- table(tweets_ML_word)

# 컬러 세팅
pal <- brewer.pal(11, "Spectral")
pal <- brewer.pal(12, "Set3")
pal <- brewer.pal(7, "Set2")
pal <- brewer.pal(12, "Paired")
pal <- brewer.pal(8, "Accent")


# 폰트 세팅
windowsFonts(font=windowsFont("배달의민족 한나는 열한살"))

# 그리기
wordcloud(names(tweets_ML_count),freq=tweets_ML_count,scale=c(7,0.5),min.freq=5,
          random.order=T,rot.per=.1,colors=pal,family="font")

wordcloud2(tweets_ML_count, fontFamily = '배달의민족 한나는 열한살', minSize = 5,
           size = 1.2, color = "random-light", backgroundColor = "black",
           rotateRatio = 0.25,# shape = 'pentagon',
           shuffle = TRUE, ellipticity = 1)

# ‘circle’ (default), ‘cardioid’, ‘diamond’ (alias of square), ‘triangle-forward’, ‘triangle’, ‘pentagon’, and ‘star’
