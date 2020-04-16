library(twitteR)
library(base64enc)
library(ROAuth)
#library(RCurl)                   ÀÌÀü ¹öÀü

#request_token_URL= "https://api.twitter.com/oauth/request_token"   ÀÌÀü ¹öÀü
#authrize_URL ="https://api.twitter.com/oauth/authorize"            ÀÌÀü ¹öÀü
#access_token_URL= "https://api.twitter.com/oauth/access_token"     ÀÌÀü ¹öÀü

# Æ®À§ÅÍ Å°È®ÀÎ: https://apps.twitter.com/app/15008019/keys

consumer_key <- ""
consumer_secret <- "" 
access_token <- ""
access_secret <- ""
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

search_string <- enc2utf8('¾ÆÀÌÄÚ½º')    # ¾ÆÀÌÄÚ½º¶ó´Â ´Ü¾î°¡ ¾ğ±ŞµÈ Æ®À­ °Ë»ö
num_tweets <- 1000                      # °¡Á®¿Ã ¼ıÀÚ ÁöÁ¤ : 1000°³
keyword <- "#sex"
tweets_ML <- searchTwitter(keyword, n=25, since='2016-01-01',
                            lang = "ko"
                            #geocode='35.874,128.246,400km'
)

# Æ®À§ÅÍ µ¥ÀÌÅÍ¸¦ ÇüÅÂº°·Î ºĞ·ùÇÏ°í ¸à¼Ç ºÎºĞ¸¸ ÃßÃâ
tweets_ML.df <- twListToDF(tweets_ML)
tweets_ML.text <- tweets_ML.df$text
View(tweets_ML.text)

# ºÒÇÊ¿äÇÑ ¹®ÀÚ¸¦ ÇÊÅÍ¸µ
tweets_ML.text <- gsub("RT @", "", tweets_ML.text)
tweets_ML.text <- gsub(".+: ", "", tweets_ML.text)
tweets_ML.text <- gsub('http.+','', tweets_ML.text)         # ÆäÀÌÁö ¸µÅ© Á¦°Å
tweets_ML.text <- gsub('youtu.+','', tweets_ML.text)        # À¯Åõºê ¸µÅ© Á¦°Å
tweets_ML.text <- gsub("ÁÁ¾Æ¿ä", "", tweets_ML.text)
tweets_ML.text <- gsub("¸®Æ®À­", "", tweets_ML.text)
tweets_ML.text <- gsub("ÆÈ·Î¿ì", "", tweets_ML.text)
tweets_ML.text <- gsub("\\<U\\+275D\\>", "", tweets_ML.text)

tweets_ML.text <- gsub('[~!@#$%&*()_+=?<>.,-]','', tweets_ML.text) # Æ¯¼ö¹®ÀÚ Á¦°Å 


kakaotxt <- gsub("\\d+",'', kakaotxt)      # ¼ıÀÚÁ¦°Å
kakaotxt <- gsub('[a-zA-z]','',kakaotxt)   # ¿µ¹® Á¦°Å


library(KoNLP)        # ¸»¹¶Ä¡ ÃßÃâ ¶óÀÌºê·¯¸®
library(wordcloud)    # ¿öµåÅ¬¶ó¿ìµå ¶óÀÌºê·¯¸®
library(wordcloud2)   # ¿öµåÅ¬¶ó¿ìµå ¶óÀÌºê·¯¸®2
library(RColorBrewer) # ¿öµåÅ¬¶ó¿ìµå »ö»ó ¶óÀÌºê·¯¸®
useSejongDic()

# ¹®ÀÚ ºĞ¸®
tweets_ML_nouns <- Map(extractNoun, tweets_ML.text)
twitter_noun <- sapply(twitter_text, extractNoun, USE.NAMES = F)
View(tweets_ML_word)
# ¾µ¸ğ¾ø´Â ¹®ÀÚµéÀ» Á¦°ÅÇÑ´Ù. Æ¯È÷ ¿µ¹®ÀÚÀÇ °æ¿ì tmÀÇ stopwords¸¦ È°¿ëÇÑ´Ù. 
tweets_ML_word <- unlist(tweets_ML_nouns, use.name=F)
tweets_ML_word <- tweets_ML_word[-which(tweets_ML_word %in% stopwords("english"))]
# 15 ±ÛÀÚ ÀÌ»ó ´Ü¾î Á¦¿Ü
twitter_noun2 <- gsub("[¤¡-¤¾°¡-ÆR]{15,}", "", twitter_noun2)

tweets_ML_word <- Filter(function(x){nchar(x)>=2}, tweets_ML_word)
twitter_noun2 <- gsub("\\d+", "", twitter_noun2)
twitter_noun2 <- gsub("\\(", "", twitter_noun2)
twitter_noun2 <- gsub("\\)", "", twitter_noun2)   

# ´Ü¾îº° Ä«¿îÆÃ
tweets_ML_count <- table(tweets_ML_word)

# ÄÃ·¯ ¼¼ÆÃ
pal <- brewer.pal(11, "Spectral")
pal <- brewer.pal(12, "Set3")
pal <- brewer.pal(7, "Set2")
pal <- brewer.pal(12, "Paired")
pal <- brewer.pal(8, "Accent")


# ÆùÆ® ¼¼ÆÃ
windowsFonts(font=windowsFont("¹è´ŞÀÇ¹ÎÁ· ÇÑ³ª´Â ¿­ÇÑ»ì"))

# ±×¸®±â
wordcloud(names(tweets_ML_count),freq=tweets_ML_count,scale=c(7,0.5),min.freq=5,
          random.order=T,rot.per=.1,colors=pal,family="font")

wordcloud2(tweets_ML_count, fontFamily = '¹è´ŞÀÇ¹ÎÁ· ÇÑ³ª´Â ¿­ÇÑ»ì', minSize = 5,
           size = 1.2, color = "random-light", backgroundColor = "black",
           rotateRatio = 0.25,# shape = 'pentagon',
           shuffle = TRUE, ellipticity = 1)

# ¡®circle¡¯ (default), ¡®cardioid¡¯, ¡®diamond¡¯ (alias of square), ¡®triangle-forward¡¯, ¡®triangle¡¯, ¡®pentagon¡¯, and ¡®star¡¯
