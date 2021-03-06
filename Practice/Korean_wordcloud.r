# install.packages("KoNLP")    
# install.packages("wordcloud")
# install.packages("wordcloud2")
# install.packages("RcolorBrewer") 
# install.packages("plyr")
# install.packages("dplyr")
# install.packages("extrafont"))
# install.packages("NIADic")

# install.packages(c("devtools","RJSONIO", "knitr", "shiny", "httpuv"))
# library(devtools)
# install_github("mages/googleVis")

# install.packages("googleVis")
# install.packages("igraph")
install.packages("qgraph",dep=TRUE)


library(KoNLP)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer) # 워드클라우드 색상 라이브러리
library(dplyr) 
# library(extrafont)

#차트용
# library(googleVis)

# 연관성 검사 결과값 
# library(tm) # 연관성 검사용
# library(qgraph) # 연관성 결과 시각화 그래프


useSejongDic()

setwd("D:/Workspace/R_Workspace/SNS")
mergeUserDic(data.frame(c(
                          
                          # "오케","대영","강남","나쁜남자","프사","추움","주안역",
                          # "일수","곱창","막창","대창","허각","변태","바이올린","비올라",
                          # "첼로","플룻","악장","기장","회장","부회장","총무","커플",
                          # "반주","긱사","자취","신입생","하드캐리","롤","개새끼","화실",
                          # "휴강","정석","도서관","군대","산쪼메","약사","재수강","세아랑",
                          # "찰칵","주모","멍충","컵밥","혼밥","핫식스","막걸리","소주",
                          # "술취함","오일일","기엠티","큐티","봉구스","밥집","후문","정문",
                          # "개꿀","셔틀","부들부들","도촬","존잘","울산","서울",
                          # "렌즈","두바이","정시","수능","멍청멍청","알바","으앙","주금",
                          # "노답","고찜","동방","양아치","조칼","존잼","졸환","신환","정연",
                          # "음캠","연돌","샤태톰","세빈니뮤","현정니뮤","인천","송도",
                          # "부천","워홀","어학연수","수강신청","돈참","성광교회","화이팅"
                          # "꿀잼","꾸르잼","민주화","일베","노잼","닥전",
                          # "닥후","메갈","보지화","개이득","김치녀","인정",
                          # "민주화","메갈리아","페미니스트","페미","맘충",
                          # "탈조선","헬반도","천조국","자야겠","굿나잇","우디"
                          # "잘자라","와이","왜인가","와진","의리남","비의리남",
                          # "자는가","조타","소울","미팅","소개팅","지하철","통금",
                          # "읽씹","휴가","레지","엠티","픽스","동아리","군기",
                          # "레지던트","여친","여자친구","핵고통","유급","춘천",
                          # "셀카봉","섹스","바보","이마트","입실렌티","라인업",
                          # "아재","피시방","pc방","방탈출","범계","쉑쉑버거",
                          # "오버워치","평촌","무한리필","댄스","고대","한림대",
                          # "어플","스키장","자취방","집들이","1박2일","선약",
                          # "예과","본과","씹노잼","퀴즈","과제","시험기간"
                          ), "ncn"))

f <- file("OCH.txt", blocking=F, encoding = "UTF-8")
kakaotxt <- iconv(readLines(f), "UTF-8", "EUC-KR")

kakaotxt <- gsub('KakaoTalk .+', '', kakaotxt)
kakaotxt <- gsub('Date Saved .+', '', kakaotxt)
kakaotxt <- gsub('.+( : )', '', kakaotxt)
kakaotxt <- gsub('.+(AM)', '', kakaotxt)
kakaotxt <- gsub('.+(PM)', '', kakaotxt)
kakaotxt <- gsub('\\(이모티콘\\)','', kakaotxt)
kakaotxt <- gsub('<Photo>','', kakaotxt)
kakaotxt <- gsub('Photo.+','', kakaotxt)
kakaotxt <- gsub('<Video>','', kakaotxt)
kakaotxt <- gsub('Video.+','', kakaotxt)
kakaotxt <- gsub('Calling.+','', kakaotxt)
kakaotxt <- gsub('Cancelled','', kakaotxt)
kakaotxt <- gsub('Missed.+','', kakaotxt)

kakaotxt <- gsub('http.+','', kakaotxt)
kakaotxt <- gsub('youtu.+','', kakaotxt)

kakaotxt <- gsub("\\[.+\\]", '', kakaotxt)
kakaotxt <- gsub('\\-.+\\-','', kakaotxt)
kakaotxt <- gsub('511','오일일',kakaotxt)
kakaotxt <- gsub("\\d+",'', kakaotxt)
kakaotxt <- gsub('[~!@#$%&*()_+=?<>.,]','', kakaotxt)
kakaotxt <- gsub('[a-zA-z]','',kakaotxt)
# kakaotxt <- gsub('[ㄱ-ㅎ]','',kakaotxt)
# kakaotxt <- gsub('(ㅜ|ㅠ)','',kakaotxt)

kakaotxt <- gsub("ㄱㄱ\\S*", "고고", kakaotxt)
kakaotxt <- gsub("ㄴㄴ\\S*", "노노", kakaotxt)
kakaotxt <- gsub("ㄷㄷ\\S*", "덜덜", kakaotxt)
kakaotxt <- gsub('ㅊㅋ\\S*','추카',kakaotxt)
kakaotxt <- gsub("ㄱㅇㄷ", "개이득", kakaotxt)
kakaotxt <- gsub("ㅇㅇ","응응",kakaotxt)
kakaotxt <- gsub("ㅇㅈ","인정",kakaotxt)
kakaotxt <- gsub('ㅂㅂ','바이바이', kakaotxt)
kakaotxt <- gsub('ㄹㅇ','레알',kakaotxt)
kakaotxt <- gsub('ㅈㄴ','존나',kakaotxt)
kakaotxt <- gsub('ㅅㅂ','씨바',kakaotxt)
kakaotxt <- gsub('ㅆㄴㅈ','씹노잼',kakaotxt)
kakaotxt <- gsub('ㄴㅈ','노잼',kakaotxt)
kakaotxt <- gsub('ㄵ','노잼',kakaotxt)
kakaotxt <- gsub('ㄷㅈ','닥전',kakaotxt)
kakaotxt <- gsub('ㄷㅎ','닥후',kakaotxt)
kakaotxt <- gsub('ㅂㄷㅂㄷ','부들부들',kakaotxt)


kakaotxt <- gsub('ㅇㅂ','일베',kakaotxt)
kakaotxt <- gsub('ㄱㄹㅎㄷ','그러하다',kakaotxt)
kakaotxt <- gsub('ㅂㅈㅎ','보지화',kakaotxt)
kakaotxt <- gsub('ㅁㅈㅎ','민주화',kakaotxt)
kakaotxt <- gsub('ㄱㅊ','괜찮',kakaotxt)
kakaotxt <- gsub('ㄱㄹㅎㅈ ㅇㄷ','그러하지않다',kakaotxt)
kakaotxt <- gsub('ㄱㄹㅎㅈㅇㄷ','그러하지않다',kakaotxt)
kakaotxt <- gsub('ㄱㄹㅎㅈ','그러하지',kakaotxt)
kakaotxt <- gsub('메갈리아','메갈',kakaotxt)
kakaotxt <- gsub('[ㄱ-ㅎ]','',kakaotxt)

kakaotxt <- na.omit(kakaotxt)

nouns <- sapply(kakaotxt, extractNoun, USE.NAMES = FALSE)
undata=unlist(nouns)

#특정 문자 제거

# undata = gsub("박열\\S*", "박열", undata)
# \S 라는 말은 박열 뒤에 붙은 공백,탭,개행을 제외한 모든 문자라는 말이고, 
# 뒤의 *는 뒤에 몇글자가 오든 상관없다는 뜻입니다.

undata <- gsub('모르겠','모르겠다',undata)
undata <- gsub('미치겠','미치겠다',undata)
undata <- gsub('보고싶','보고싶다',undata)
undata <- gsub('레알이','레알이다',undata)
undata <- gsub('레알인','레알인가',undata)
undata <- gsub('알고있','알고있다',undata)
undata <- gsub('있었$','있었다',undata)
undata <- gsub('하였$','하였다',undata)
undata <- gsub('않았$','않았다',undata)
undata <- gsub('않겠','않겠다',undata)
undata <- gsub('하겠$','하겠다',undata)
undata <- gsub('그렇','그렇다',undata)
undata <- gsub('다르','다르다',undata)
undata <- gsub('보았$','보았다',undata)
undata <- gsub('줘패야한','줘패야한다',undata)
undata <- gsub('줘패겠','줘패겠다',undata)
undata <- gsub('왜인','왜인가',undata) 
undata <- gsub('와진$','와진가',undata)
undata <- gsub('좋$','좋다',undata)
undata <- gsub('레지$','레지던트',undata)
undata <- gsub('민주$','민주화',undata)
undata <- gsub('부들$','부들부들',undata)
undata <- gsub('오일일$','511',undata)
undata <- gsub('바이바$','바이바이',undata)
undata <- gsub('조경$','조경수',undata)
undata <- gsub('이현$','이현정',undata)
undata <- gsub('남경$','남경록',undata)
undata <- gsub('밥먹$','밥먹자',undata)
undata <- gsub('좋겠$','좋겠다',undata)
undata <- gsub('부럽$','부럽다',undata)
undata <- gsub('뿌잉뿌$','뿌잉뿌잉',undata)
undata <- gsub('존나웃기$','존나웃기네',undata)
undata <- gsub('크리스$','크리스마스',undata)
undata <- gsub('왜그$','왜그래',undata)
undata <- gsub('왜이$','왜이래',undata)

#글자수 필터 
data2= Filter(function(x){nchar(x)>=2}, undata)
wordcount <- table(data2)
# head(wordcount2)
wordcount_top <-head(sort(wordcount, decreasing = TRUE),700)
head(wordcount_top, 700)
# View(wordcount_top)

# table 혹은 csv파일로 결과물을 보고 싶으시면 다음 코드 실행해주세요.
# write(wordcount, "2017 문화체육관광부 wordcloud.txt")  
# write.csv(wordcount, "2017 문화체육관광부 wordcloud.csv")


#색상 
display.brewer.all()
col <- brewer.pal(11, "Spectral")
col <- brewer.pal(12, "Set3")
col <- brewer.pal(7, "Set2")
col <- brewer.pal(12, "Paired")
col <- brewer.pal(12, "Accent")

#폰트 
windowsFonts(font=windowsFont("12롯데마트행복Bold"))
windowsFonts(font=windowsFont("Koverwatch"))
windowsFonts(font=windowsFont("나눔스퀘어OTF ExtraBold"))
windowsFonts(font=windowsFont("야놀자 야체 B"))
windowsFonts(font=windowsFont("배달의민족 한나는 열한살"))
windowsFonts(font=windowsFont("문체부 궁체 흘림체"))


wordcloud(names(wordcount_top), freq = wordcount_top, scale=c(5,1), rot.per=0.2, min.freq=5, random.order=FALSE, random.color=TRUE, colors=col, family = "font")


wordcloud2(wordcount_top, fontFamily = '배달의민족 한나는 열한살', minSize = 5,
           size = 1.2, color = "random-light", backgroundColor = "black",
           rotateRatio = 0.25, shape = 'pentagon',
           shuffle = TRUE, ellipticity = 1)
# ‘circle’ (default), ‘cardioid’, ‘diamond’ (alias of square), ‘triangle-forward’, ‘triangle’, ‘pentagon’, and ‘star’



library(stringr)
tt<-paste(unlist(SimplePos22(kakaotxt)))
head(tt,200)
#명사만 가져오기
alldta<-str_match_all(tt,"[가-힣]+/[N][C]|[가-힣]+/[N][Q]+")%>% unlist()

#형용사만 가져오기
alldta2<-str_match_all(tt,"[가-힣]+/[P][V]+|[가-힣]+/[P][X]+|[가-힣]+/[P][A]+|[가-힣]+/[M][A]+")%>%unlist()

N<-str_replace_all(alldta,"/[N][C]","") %>%
  str_replace_all("/[N][Q]","") %>%unlist()
#명사로 추출된 단어들의 분류표인 /NC, /NQ 등을 제거한다.

PNM<-str_replace_all(alldta2,"/[P][V]","") %>%
  str_replace_all("/[P][A]","") %>%
  str_replace_all("/[M][A]","") %>%
  str_replace_all("/[P][X]","") %>% unlist() 

#마찬가지로 감정어로 분류한 단어들의 분류표들을 제거한다.


DtaCorpusNC<-Corpus(VectorSource(N))
myTdmNC<-TermDocumentMatrix(DtaCorpusNC,control = list(wordLengths=c(4,10),
                                                       removePunctuation=T,removeNumbers=T,weighting=weightBin))
Encoding(myTdmNC$dimnames$Terms)="EUC-kr"
#tm패키지에서 제공하는 Corpus를 통해 분류된 단어들의 행렬을 만든다.
#Corpus의 원리는 현재 까먹었다.

findFreqTerms(myTdmNC, lowfreq=10)
#제대로 되었는지 확인 차원에서 입력했다. 

mtNC<-as.matrix(myTdmNC[0:30,0:30]) #행렬(matrix)로 변환하는 게 상관성 분석의 핵심이다.
mtrowNC<-rowSums(mtNC) 
mtNC.order<-mtrowNC[order(mtrowNC,decreasing=TRUE)]
freq.wordsNC<-sample(mtNC.order[mtNC.order>30],25)
freq.wordsNC<-as.matrix(freq.wordsNC)
freq.wordsNC 

co.matrix<-freq.wordsNC %*% t(freq.wordsNC)

qgraph(co.matrix,
       labels=rownames(co.matrix),
       diag=FALSE,
       layout='spring', 
       vsize=log(diag(co.matrix)*2))
#-----------
DtaCorpusPNM<-Corpus(VectorSource(PNM))
myTdmPNM<-TermDocumentMatrix(DtaCorpusPNM,control = list(wordLengths=c(4,10),weighting=weightBin))
Encoding(myTdmPNM$dimnames$Terms)="UTF-8"
findFreqTerms(myTdmPNM, lowfreq=10)
mtPNM<-as.matrix(myTdmPNM)
mtrowPNM<-rowSums(mtPNM) #
mtPNM.order<-mtrowPNM[order(mtrowPNM,decreasing=T)]
freq.wordsPNM<-sample(mtPNM.order[mtPNM.order>30],25)
freq.wordsPNM<-as.matrix(freq.wordsPNM) 
co.matrix2<-freq.wordsPNM %*% t(freq.wordsPNM)

qgraph(co.matrix2,
       labels=rownames(co.matrix2),
       diag=FALSE,
       layout='spring', 
       vsize=log(diag(co.matrix2)*2))










# require(devtools)
# install_github("lchiffon/wordcloud2")
fig = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(demoFreq, figPath = fig, size = 1,5,color = "skyblue")

wordcloud2(wordcount_top, fontFamily = '배달의민족 한나는 열한살',
           figPath = fig, size = 1, color = "skyblue" )

letterCloud(wordcount_top, word = "R",
            fontFamily = '배달의민족 한나는 열한살',size = 2)

letterCloud(wordcount_top, word = "WORDCLOUD2", 
            fontFamily = '배달의민족 한나는 열한살', wordSize = 1 )






#install webshot
library(webshot)
webshot::install_phantomjs()

# Make the graph
my_graph=wordcloud2(demoFreq, size=1.5)

# save it in html
library("htmlwidgets")
saveWidget(my_graph,"tmp.html",selfcontained = F)

# and in pdf
webshot("tmp.html","fig_1.pdf", delay =5, vwidth = 480, vheight=480)


# cps <- VCorpus(VectorSource(kko11))
# tdm <- TermDocumentMatrix(cps,
#                           control = list(tokenize=ko.words,
#                                          removePunctuation = T,
#                                          removeNumbers = T,
#                                          stopwords = c('네','넹','넴','으',
#                                                        '음','움','오','헐','앜')))
# tdm <- as.matrix(tdm)
# data <- data.frame(X=names(v),freq=v)



# 
# person1 <- length(data[grep(" you ",kakaotxt)])
# person2 <- length(data[grep(" 안정민 ",kakaotxt)])
# person3 <- length(data[grep(" 장정욱 ",kakaotxt)])
# person4 <- length(data[grep(" 백계삼 ",kakaotxt)])
# person5 <- length(data[grep(" 서영주 ",kakaotxt)])
# 
# talk_cnt <- c(person1,person2,person3,person4,person5)
# talk_name <- c("최중원", "안정민", "장정욱", "백계삼", "서영주")
# kakaopie <- data.frame(talk_name,talk_cnt)

# pie <- gvisPieChart(kakaopie,options=list(width=400),height=300)
# header <- pie$html$header
# header <- gsub("charset=utf-8", "charset=euc-kr",header)
# pie$header <- header
# plot(pie)



packageDescription('qgraph')$Version
options(
  qgraph = list(
    border.width = 2,
    asize = 8,
    unCol = "black",
    vsize = 10,
    esize = 3)
)
input <- matrix(c(
  0,1,1,
  0,0,1,
  0,0,0),3,3,byrow=TRUE)
print(input)
qgraph(input)


input <- matrix(0,8,8)
input[1,2] <- 1
input[2,3] <- 1
input[3,4] <- 1
input[4,5] <- 1
input[5,6] <- 1
input[6,7] <- 1
input[7,8] <- 1
input[8,1] <- 1
dput(input)


input<- matrix(1,3,3)
print(input)
qgraph(input,directed=TRUE)


input<-matrix(c(
  0,1,2,
  0,0,3,
  0,0,0),3,3,byrow=TRUE)
qgraph(input)


data(big5)
str(big5)
View(big5)
qgraph(cor(big5),minimum=0.25)
