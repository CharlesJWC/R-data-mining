library(KoNLP)        # 말뭉치 추출 라이브러리
library(wordcloud)    # 워드클라우드 라이브러리
library(wordcloud2)   # 워드클라우드 라이브러리2
library(RColorBrewer) # 워드클라우드 색상 라이브러리
library(dplyr)        # Cleaning 라이브러리

useSejongDic()  # 세종단어사전 사용
setwd("D:/Workspace/R_Workspace/SNS") # 작업 경로설정

# 텍스트 파일 로드
f <- file("OCH.txt", blocking=F, encoding = "UTF-8")
# 한글이 깨지지 않도록 인코딩
kakaotxt <- iconv(readLines(f), "UTF-8", "EUC-KR")   


mergeUserDic(data.frame(c(
                      "오케","대영","강남","나쁜남자","프사","추움","주안역",
                      "일수","곱창","막창","대창","허각","변태","바이올린","비올라",
                      "첼로","플룻","악장","기장","회장","부회장","총무","커플",
                      "반주","긱사","자취","신입생","하드캐리","롤","화실",
                      "휴강","정석","도서관","군대","산쪼메","약사","재수강",
                      "찰칵","주모","멍충","컵밥","혼밥","핫식스","막걸리","소주",
                      "오일일","기엠티","큐티","봉구스","밥집","후문","정문",
                      "개꿀","셔틀","부들부들","존잘","울산","서울",
                      "렌즈","두바이","정시","수능","알바",
                      "노답","고찜","동방","양아치","조칼","졸환","신환","정연",
                      "음캠","연돌","샤태톰","인천","송도",
                      "부천","워홀","어학연수","수강신청","돈참","성광교회","화이팅",
                      "꿀잼","꾸르잼","노잼","닥전", "개이득","인정",
                      "소울","미팅","소개팅","지하철","통금",
                      "읽씹","휴가","레지","엠티","픽스","동아리",
                      "레지던트","여친","남친","여자친구","핵고통","유급","춘천",
                      "셀카봉","바보","이마트","입실렌티","라인업",
                      "아재","피시방","pc방","방탈출","범계","쉑쉑버거",
                      "오버워치","평촌","무한리필","댄스","고대","한림대",
                      "어플","스키장","자취방","집들이","1박2일","선약",
                      "예과","본과","퀴즈","과제","시험기간"
                      ), "ncn"))



kakaotxt <- gsub('KakaoTalk .+', '', kakaotxt)  
kakaotxt <- gsub('Date Saved .+', '', kakaotxt) 
kakaotxt <- gsub('.+( : )', '', kakaotxt)       # 대화 발화자 제거
kakaotxt <- gsub('.+(AM)', '', kakaotxt)        # 시간 제거
kakaotxt <- gsub('.+(PM)', '', kakaotxt)        # 시간 제거
kakaotxt <- gsub('\\(이모티콘\\)','', kakaotxt) # 이모티콘 제거
kakaotxt <- gsub('<Photo>','', kakaotxt)        # <사진> 제거
kakaotxt <- gsub('Photo.+','', kakaotxt)        # 사진 제거
kakaotxt <- gsub('<Video>','', kakaotxt)        # <비디오> 제거
kakaotxt <- gsub('Video.+','', kakaotxt)        # 비디오 제거
kakaotxt <- gsub('Calling.+','', kakaotxt)      # 전화 걸기 제거
kakaotxt <- gsub('Cancelled','', kakaotxt)      
kakaotxt <- gsub('Missed.+','', kakaotxt)
kakaotxt <- gsub('http.+','', kakaotxt)         # 페이지 링크 제거
kakaotxt <- gsub('youtu.+','', kakaotxt)        # 유투브 링크 제거
kakaotxt <- gsub("\\[.+\\]", '', kakaotxt) # [내용] 형태 제거
kakaotxt <- gsub('\\-.+\\-','', kakaotxt)  # -내용- 형태 제거
kakaotxt <- gsub('511','오일일',kakaotxt)  # 숫자제거 이전에 511 변화
kakaotxt <- gsub("\\d+",'', kakaotxt)      # 숫자제거
kakaotxt <- gsub('[~!@#$%&*()_+=?<>.,]','', kakaotxt) # 특수문자 제거 
kakaotxt <- gsub('[a-zA-z]','',kakaotxt)   # 영문 제거

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
kakaotxt <- gsub('ㄴㅈ','노잼',kakaotxt)
kakaotxt <- gsub('ㄵ','노잼',kakaotxt)
kakaotxt <- gsub('ㅂㄷㅂㄷ','부들부들',kakaotxt)
kakaotxt <- gsub('[ㄱ-ㅎ]','',kakaotxt)

kakaotxt <- na.omit(kakaotxt) # 비어있는 라인 제거

# 의미가 있는 말물치 추출
nouns <- sapply(kakaotxt, extractNoun, USE.NAMES = FALSE) 
undata=unlist(nouns) # 리스트 벡터 -> 문자 벡터로 변환

View(nouns)
View(undata)
typeof(nouns)
typeof(undata)

#특정 단어 변환
undata <- gsub('모르겠','모르겠다',undata)
undata <- gsub('미치겠','미치겠다',undata)
undata <- gsub('보고싶','보고싶다',undata)
undata <- gsub('알고있','알고있다',undata)
undata <- gsub('있었$','있었다',undata)
undata <- gsub('하였$','하였다',undata)
undata <- gsub('않았$','않았다',undata)
undata <- gsub('하겠$','하겠다',undata)
undata <- gsub('그렇','그렇다',undata)
undata <- gsub('다르','다르다',undata)
undata <- gsub('보았$','보았다',undata)
undata <- gsub('좋$','좋다',undata)

undata <- gsub('레지$','레지던트',undata)
undata <- gsub('부들$','부들부들',undata)
undata <- gsub('오일일$','511',undata)
undata <- gsub('바이바$','바이바이',undata)
undata <- gsub('밥먹$','밥먹자',undata)
undata <- gsub('좋겠$','좋겠다',undata)
undata <- gsub('부럽$','부럽다',undata)
undata <- gsub('뿌잉뿌$','뿌잉뿌잉',undata)
undata <- gsub('크리스$','크리스마스',undata)
undata <- gsub('왜그$','왜그래',undata)

#글자수 필터 
data2= Filter(function(x){nchar(x)>=2}, undata)
wordcount <- table(data2)
wordcount_top <-head(sort(wordcount, decreasing = TRUE),700)


head(data2)
head(wordcount_top, 700)
View(wordcount_top)


#색상 
display.brewer.all()
col <- brewer.pal(11, "Spectral")
col <- brewer.pal(12, "Set3")
col <- brewer.pal(7, "Set2")
col <- brewer.pal(12, "Paired")
col <- brewer.pal(8, "Accent")

#폰트 
windowsFonts(font=windowsFont("12롯데마트행복Bold"))
windowsFonts(font=windowsFont("Koverwatch"))
windowsFonts(font=windowsFont("나눔스퀘어OTF ExtraBold"))
windowsFonts(font=windowsFont("야놀자 야체 B"))
windowsFonts(font=windowsFont("배달의민족 한나는 열한살"))
windowsFonts(font=windowsFont("문체부 궁체 흘림체"))


wordcloud(names(wordcount_top), freq = wordcount_top, scale=c(7,0.2), 
          rot.per=0.1, min.freq=20, random.order=FALSE, random.color=TRUE, 
          colors=col, family = "font")


wordcloud2(wordcount_top, fontFamily = '배달의민족 한나는 열한살', minSize = 5,
           size = 1.2, color = "random-light", backgroundColor = "black",
           rotateRatio = 0.25, shape = 'pentagon',
           shuffle = TRUE, ellipticity = 1)




# ‘circle’ (default), ‘cardioid’, ‘diamond’ (alias of square), ‘triangle-forward’, ‘triangle’, ‘pentagon’, and ‘star’
