library(twitteR)
library(base64enc)
library(ROAuth)
#library(RCurl)                   ���� ����

#request_token_URL= "https://api.twitter.com/oauth/request_token"   ���� ����
#authrize_URL ="https://api.twitter.com/oauth/authorize"            ���� ����
#access_token_URL= "https://api.twitter.com/oauth/access_token"     ���� ����

# Ʈ���� ŰȮ��: https://apps.twitter.com/app/15008019/keys

consumer_key <- ""
consumer_secret <- "" 
access_token <- ""
access_secret <- ""
setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

search_string <- enc2utf8('�����ڽ�')    # �����ڽ���� �ܾ ��޵� Ʈ�� �˻�
num_tweets <- 1000                      # ������ ���� ���� : 1000��
keyword <- "#sex"
tweets_ML <- searchTwitter(keyword, n=25, since='2016-01-01',
                            lang = "ko"
                            #geocode='35.874,128.246,400km'
)

# Ʈ���� �����͸� ���º��� �з��ϰ� ��� �κи� ����
tweets_ML.df <- twListToDF(tweets_ML)
tweets_ML.text <- tweets_ML.df$text
View(tweets_ML.text)

# ���ʿ��� ���ڸ� ���͸�
tweets_ML.text <- gsub("RT @", "", tweets_ML.text)
tweets_ML.text <- gsub(".+: ", "", tweets_ML.text)
tweets_ML.text <- gsub('http.+','', tweets_ML.text)         # ������ ��ũ ����
tweets_ML.text <- gsub('youtu.+','', tweets_ML.text)        # ������ ��ũ ����
tweets_ML.text <- gsub("���ƿ�", "", tweets_ML.text)
tweets_ML.text <- gsub("��Ʈ��", "", tweets_ML.text)
tweets_ML.text <- gsub("�ȷο�", "", tweets_ML.text)
tweets_ML.text <- gsub("\\<U\\+275D\\>", "", tweets_ML.text)

tweets_ML.text <- gsub('[~!@#$%&*()_+=?<>.,-]','', tweets_ML.text) # Ư������ ���� 


kakaotxt <- gsub("\\d+",'', kakaotxt)      # ��������
kakaotxt <- gsub('[a-zA-z]','',kakaotxt)   # ���� ����


library(KoNLP)        # ����ġ ���� ���̺귯��
library(wordcloud)    # ����Ŭ���� ���̺귯��
library(wordcloud2)   # ����Ŭ���� ���̺귯��2
library(RColorBrewer) # ����Ŭ���� ���� ���̺귯��
useSejongDic()

# ���� �и�
tweets_ML_nouns <- Map(extractNoun, tweets_ML.text)
twitter_noun <- sapply(twitter_text, extractNoun, USE.NAMES = F)
View(tweets_ML_word)
# ������� ���ڵ��� �����Ѵ�. Ư�� �������� ��� tm�� stopwords�� Ȱ���Ѵ�. 
tweets_ML_word <- unlist(tweets_ML_nouns, use.name=F)
tweets_ML_word <- tweets_ML_word[-which(tweets_ML_word %in% stopwords("english"))]
# 15 ���� �̻� �ܾ� ����
twitter_noun2 <- gsub("[��-����-�R]{15,}", "", twitter_noun2)

tweets_ML_word <- Filter(function(x){nchar(x)>=2}, tweets_ML_word)
twitter_noun2 <- gsub("\\d+", "", twitter_noun2)
twitter_noun2 <- gsub("\\(", "", twitter_noun2)
twitter_noun2 <- gsub("\\)", "", twitter_noun2)   

# �ܾ ī����
tweets_ML_count <- table(tweets_ML_word)

# �÷� ����
pal <- brewer.pal(11, "Spectral")
pal <- brewer.pal(12, "Set3")
pal <- brewer.pal(7, "Set2")
pal <- brewer.pal(12, "Paired")
pal <- brewer.pal(8, "Accent")


# ��Ʈ ����
windowsFonts(font=windowsFont("����ǹ��� �ѳ��� ���ѻ�"))

# �׸���
wordcloud(names(tweets_ML_count),freq=tweets_ML_count,scale=c(7,0.5),min.freq=5,
          random.order=T,rot.per=.1,colors=pal,family="font")

wordcloud2(tweets_ML_count, fontFamily = '����ǹ��� �ѳ��� ���ѻ�', minSize = 5,
           size = 1.2, color = "random-light", backgroundColor = "black",
           rotateRatio = 0.25,# shape = 'pentagon',
           shuffle = TRUE, ellipticity = 1)

# ��circle�� (default), ��cardioid��, ��diamond�� (alias of square), ��triangle-forward��, ��triangle��, ��pentagon��, and ��star��