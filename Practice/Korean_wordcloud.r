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
library(RColorBrewer) # ¿öµåÅ¬¶ó¿ìµå »ö»ó ¶óÀÌºê·¯¸®
library(dplyr) 
# library(extrafont)

#Â÷Æ®¿ë
# library(googleVis)

# ¿¬°ü¼º °Ë»ç °á°ú°ª 
# library(tm) # ¿¬°ü¼º °Ë»ç¿ë
# library(qgraph) # ¿¬°ü¼º °á°ú ½Ã°¢È­ ±×·¡ÇÁ


useSejongDic()

setwd("D:/Workspace/R_Workspace/SNS")
mergeUserDic(data.frame(c(
                          
                          # "¿ÀÄÉ","´ë¿µ","°­³²","³ª»Û³²ÀÚ","ÇÁ»ç","Ãß¿ò","ÁÖ¾È¿ª",
                          # "ÀÏ¼ö","°öÃ¢","¸·Ã¢","´ëÃ¢","Çã°¢","º¯ÅÂ","¹ÙÀÌ¿Ã¸°","ºñ¿Ã¶ó",
                          # "Ã¿·Î","ÇÃ·í","¾ÇÀå","±âÀå","È¸Àå","ºÎÈ¸Àå","ÃÑ¹«","Ä¿ÇÃ",
                          # "¹İÁÖ","±ã»ç","ÀÚÃë","½ÅÀÔ»ı","ÇÏµåÄ³¸®","·Ñ","°³»õ³¢","È­½Ç",
                          # "ÈŞ°­","Á¤¼®","µµ¼­°ü","±º´ë","»êÂÉ¸Ş","¾à»ç","Àç¼ö°­","¼¼¾Æ¶û",
                          # "ÂûÄ¬","ÁÖ¸ğ","¸ÛÃæ","ÄÅ¹ä","È¥¹ä","ÇÖ½Ä½º","¸·°É¸®","¼ÒÁÖ",
                          # "¼úÃëÇÔ","¿ÀÀÏÀÏ","±â¿¥Æ¼","Å¥Æ¼","ºÀ±¸½º","¹äÁı","ÈÄ¹®","Á¤¹®",
                          # "°³²Ü","¼ÅÆ²","ºÎµéºÎµé","µµÃÔ","Á¸Àß","¿ï»ê","¼­¿ï",
                          # "·»Áî","µÎ¹ÙÀÌ","Á¤½Ã","¼ö´É","¸ÛÃ»¸ÛÃ»","¾Ë¹Ù","À¸¾Ó","ÁÖ±İ",
                          # "³ë´ä","°íÂò","µ¿¹æ","¾ç¾ÆÄ¡","Á¶Ä®","Á¸Àë","Á¹È¯","½ÅÈ¯","Á¤¿¬",
                          # "À½Ä·","¿¬µ¹","»şÅÂÅè","¼¼ºó´Ï¹Â","ÇöÁ¤´Ï¹Â","ÀÎÃµ","¼Ûµµ",
                          # "ºÎÃµ","¿öÈ¦","¾îÇĞ¿¬¼ö","¼ö°­½ÅÃ»","µ·Âü","¼º±¤±³È¸","È­ÀÌÆÃ"
                          # "²ÜÀë","²Ù¸£Àë","¹ÎÁÖÈ­","ÀÏº£","³ëÀë","´ÚÀü",
                          # "´ÚÈÄ","¸Ş°¥","º¸ÁöÈ­","°³ÀÌµæ","±èÄ¡³à","ÀÎÁ¤",
                          # "¹ÎÁÖÈ­","¸Ş°¥¸®¾Æ","Æä¹Ì´Ï½ºÆ®","Æä¹Ì","¸¾Ãæ",
                          # "Å»Á¶¼±","Çï¹İµµ","ÃµÁ¶±¹","ÀÚ¾ß°Ú","±Â³ªÀÕ","¿ìµğ"
                          # "ÀßÀÚ¶ó","¿ÍÀÌ","¿ÖÀÎ°¡","¿ÍÁø","ÀÇ¸®³²","ºñÀÇ¸®³²",
                          # "ÀÚ´Â°¡","Á¶Å¸","¼Ò¿ï","¹ÌÆÃ","¼Ò°³ÆÃ","ÁöÇÏÃ¶","Åë±İ",
                          # "ÀĞ¾Ã","ÈŞ°¡","·¹Áö","¿¥Æ¼","ÇÈ½º","µ¿¾Æ¸®","±º±â",
                          # "·¹Áö´øÆ®","¿©Ä£","¿©ÀÚÄ£±¸","ÇÙ°íÅë","À¯±Ş","ÃáÃµ",
                          # "¼¿Ä«ºÀ","¼½½º","¹Ùº¸","ÀÌ¸¶Æ®","ÀÔ½Ç·»Æ¼","¶óÀÎ¾÷",
                          # "¾ÆÀç","ÇÇ½Ã¹æ","pc¹æ","¹æÅ»Ãâ","¹ü°è","½§½§¹ö°Å",
                          # "¿À¹ö¿öÄ¡","ÆòÃÌ","¹«ÇÑ¸®ÇÊ","´í½º","°í´ë","ÇÑ¸²´ë",
                          # "¾îÇÃ","½ºÅ°Àå","ÀÚÃë¹æ","ÁıµéÀÌ","1¹Ú2ÀÏ","¼±¾à",
                          # "¿¹°ú","º»°ú","¾Ã³ëÀë","ÄûÁî","°úÁ¦","½ÃÇè±â°£"
                          ), "ncn"))

f <- file("OCH.txt", blocking=F, encoding = "UTF-8")
kakaotxt <- iconv(readLines(f), "UTF-8", "EUC-KR")

kakaotxt <- gsub('KakaoTalk .+', '', kakaotxt)
kakaotxt <- gsub('Date Saved .+', '', kakaotxt)
kakaotxt <- gsub('.+( : )', '', kakaotxt)
kakaotxt <- gsub('.+(AM)', '', kakaotxt)
kakaotxt <- gsub('.+(PM)', '', kakaotxt)
kakaotxt <- gsub('\\(ÀÌ¸ğÆ¼ÄÜ\\)','', kakaotxt)
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
kakaotxt <- gsub('511','¿ÀÀÏÀÏ',kakaotxt)
kakaotxt <- gsub("\\d+",'', kakaotxt)
kakaotxt <- gsub('[~!@#$%&*()_+=?<>.,]','', kakaotxt)
kakaotxt <- gsub('[a-zA-z]','',kakaotxt)
# kakaotxt <- gsub('[¤¡-¤¾]','',kakaotxt)
# kakaotxt <- gsub('(¤Ì|¤Ğ)','',kakaotxt)

kakaotxt <- gsub("¤¡¤¡\\S*", "°í°í", kakaotxt)
kakaotxt <- gsub("¤¤¤¤\\S*", "³ë³ë", kakaotxt)
kakaotxt <- gsub("¤§¤§\\S*", "´ú´ú", kakaotxt)
kakaotxt <- gsub('¤º¤»\\S*','ÃßÄ«',kakaotxt)
kakaotxt <- gsub("¤¡¤·¤§", "°³ÀÌµæ", kakaotxt)
kakaotxt <- gsub("¤·¤·","ÀÀÀÀ",kakaotxt)
kakaotxt <- gsub("¤·¤¸","ÀÎÁ¤",kakaotxt)
kakaotxt <- gsub('¤²¤²','¹ÙÀÌ¹ÙÀÌ', kakaotxt)
kakaotxt <- gsub('¤©¤·','·¹¾Ë',kakaotxt)
kakaotxt <- gsub('¤¸¤¤','Á¸³ª',kakaotxt)
kakaotxt <- gsub('¤µ¤²','¾¾¹Ù',kakaotxt)
kakaotxt <- gsub('¤¶¤¤¤¸','¾Ã³ëÀë',kakaotxt)
kakaotxt <- gsub('¤¤¤¸','³ëÀë',kakaotxt)
kakaotxt <- gsub('¤¥','³ëÀë',kakaotxt)
kakaotxt <- gsub('¤§¤¸','´ÚÀü',kakaotxt)
kakaotxt <- gsub('¤§¤¾','´ÚÈÄ',kakaotxt)
kakaotxt <- gsub('¤²¤§¤²¤§','ºÎµéºÎµé',kakaotxt)


kakaotxt <- gsub('¤·¤²','ÀÏº£',kakaotxt)
kakaotxt <- gsub('¤¡¤©¤¾¤§','±×·¯ÇÏ´Ù',kakaotxt)
kakaotxt <- gsub('¤²¤¸¤¾','º¸ÁöÈ­',kakaotxt)
kakaotxt <- gsub('¤±¤¸¤¾','¹ÎÁÖÈ­',kakaotxt)
kakaotxt <- gsub('¤¡¤º','±¦Âú',kakaotxt)
kakaotxt <- gsub('¤¡¤©¤¾¤¸ ¤·¤§','±×·¯ÇÏÁö¾Ê´Ù',kakaotxt)
kakaotxt <- gsub('¤¡¤©¤¾¤¸¤·¤§','±×·¯ÇÏÁö¾Ê´Ù',kakaotxt)
kakaotxt <- gsub('¤¡¤©¤¾¤¸','±×·¯ÇÏÁö',kakaotxt)
kakaotxt <- gsub('¸Ş°¥¸®¾Æ','¸Ş°¥',kakaotxt)
kakaotxt <- gsub('[¤¡-¤¾]','',kakaotxt)

kakaotxt <- na.omit(kakaotxt)

nouns <- sapply(kakaotxt, extractNoun, USE.NAMES = FALSE)
undata=unlist(nouns)

#Æ¯Á¤ ¹®ÀÚ Á¦°Å

# undata = gsub("¹Ú¿­\\S*", "¹Ú¿­", undata)
# \S ¶ó´Â ¸»Àº ¹Ú¿­ µÚ¿¡ ºÙÀº °ø¹é,ÅÇ,°³ÇàÀ» Á¦¿ÜÇÑ ¸ğµç ¹®ÀÚ¶ó´Â ¸»ÀÌ°í, 
# µÚÀÇ *´Â µÚ¿¡ ¸î±ÛÀÚ°¡ ¿Àµç »ó°ü¾ø´Ù´Â ¶æÀÔ´Ï´Ù.

undata <- gsub('¸ğ¸£°Ú','¸ğ¸£°Ú´Ù',undata)
undata <- gsub('¹ÌÄ¡°Ú','¹ÌÄ¡°Ú´Ù',undata)
undata <- gsub('º¸°í½Í','º¸°í½Í´Ù',undata)
undata <- gsub('·¹¾ËÀÌ','·¹¾ËÀÌ´Ù',undata)
undata <- gsub('·¹¾ËÀÎ','·¹¾ËÀÎ°¡',undata)
undata <- gsub('¾Ë°íÀÖ','¾Ë°íÀÖ´Ù',undata)
undata <- gsub('ÀÖ¾ú$','ÀÖ¾ú´Ù',undata)
undata <- gsub('ÇÏ¿´$','ÇÏ¿´´Ù',undata)
undata <- gsub('¾Ê¾Ò$','¾Ê¾Ò´Ù',undata)
undata <- gsub('¾Ê°Ú','¾Ê°Ú´Ù',undata)
undata <- gsub('ÇÏ°Ú$','ÇÏ°Ú´Ù',undata)
undata <- gsub('±×·¸','±×·¸´Ù',undata)
undata <- gsub('´Ù¸£','´Ù¸£´Ù',undata)
undata <- gsub('º¸¾Ò$','º¸¾Ò´Ù',undata)
undata <- gsub('ÁàÆĞ¾ßÇÑ','ÁàÆĞ¾ßÇÑ´Ù',undata)
undata <- gsub('ÁàÆĞ°Ú','ÁàÆĞ°Ú´Ù',undata)
undata <- gsub('¿ÖÀÎ','¿ÖÀÎ°¡',undata) 
undata <- gsub('¿ÍÁø$','¿ÍÁø°¡',undata)
undata <- gsub('ÁÁ$','ÁÁ´Ù',undata)
undata <- gsub('·¹Áö$','·¹Áö´øÆ®',undata)
undata <- gsub('¹ÎÁÖ$','¹ÎÁÖÈ­',undata)
undata <- gsub('ºÎµé$','ºÎµéºÎµé',undata)
undata <- gsub('¿ÀÀÏÀÏ$','511',undata)
undata <- gsub('¹ÙÀÌ¹Ù$','¹ÙÀÌ¹ÙÀÌ',undata)
undata <- gsub('Á¶°æ$','Á¶°æ¼ö',undata)
undata <- gsub('ÀÌÇö$','ÀÌÇöÁ¤',undata)
undata <- gsub('³²°æ$','³²°æ·Ï',undata)
undata <- gsub('¹ä¸Ô$','¹ä¸ÔÀÚ',undata)
undata <- gsub('ÁÁ°Ú$','ÁÁ°Ú´Ù',undata)
undata <- gsub('ºÎ·´$','ºÎ·´´Ù',undata)
undata <- gsub('»ÑÀ×»Ñ$','»ÑÀ×»ÑÀ×',undata)
undata <- gsub('Á¸³ª¿ô±â$','Á¸³ª¿ô±â³×',undata)
undata <- gsub('Å©¸®½º$','Å©¸®½º¸¶½º',undata)
undata <- gsub('¿Ö±×$','¿Ö±×·¡',undata)
undata <- gsub('¿ÖÀÌ$','¿ÖÀÌ·¡',undata)

#±ÛÀÚ¼ö ÇÊÅÍ 
data2= Filter(function(x){nchar(x)>=2}, undata)
wordcount <- table(data2)
# head(wordcount2)
wordcount_top <-head(sort(wordcount, decreasing = TRUE),700)
head(wordcount_top, 700)
# View(wordcount_top)

# table È¤Àº csvÆÄÀÏ·Î °á°ú¹°À» º¸°í ½ÍÀ¸½Ã¸é ´ÙÀ½ ÄÚµå ½ÇÇàÇØÁÖ¼¼¿ä.
# write(wordcount, "2017 ¹®È­Ã¼À°°ü±¤ºÎ wordcloud.txt")  
# write.csv(wordcount, "2017 ¹®È­Ã¼À°°ü±¤ºÎ wordcloud.csv")


#»ö»ó 
display.brewer.all()
col <- brewer.pal(11, "Spectral")
col <- brewer.pal(12, "Set3")
col <- brewer.pal(7, "Set2")
col <- brewer.pal(12, "Paired")
col <- brewer.pal(12, "Accent")

#ÆùÆ® 
windowsFonts(font=windowsFont("12·Ôµ¥¸¶Æ®Çàº¹Bold"))
windowsFonts(font=windowsFont("Koverwatch"))
windowsFonts(font=windowsFont("³ª´®½ºÄù¾îOTF ExtraBold"))
windowsFonts(font=windowsFont("¾ß³îÀÚ ¾ßÃ¼ B"))
windowsFonts(font=windowsFont("¹è´ŞÀÇ¹ÎÁ· ÇÑ³ª´Â ¿­ÇÑ»ì"))
windowsFonts(font=windowsFont("¹®Ã¼ºÎ ±ÃÃ¼ Èê¸²Ã¼"))


wordcloud(names(wordcount_top), freq = wordcount_top, scale=c(5,1), rot.per=0.2, min.freq=5, random.order=FALSE, random.color=TRUE, colors=col, family = "font")


wordcloud2(wordcount_top, fontFamily = '¹è´ŞÀÇ¹ÎÁ· ÇÑ³ª´Â ¿­ÇÑ»ì', minSize = 5,
           size = 1.2, color = "random-light", backgroundColor = "black",
           rotateRatio = 0.25, shape = 'pentagon',
           shuffle = TRUE, ellipticity = 1)
# ¡®circle¡¯ (default), ¡®cardioid¡¯, ¡®diamond¡¯ (alias of square), ¡®triangle-forward¡¯, ¡®triangle¡¯, ¡®pentagon¡¯, and ¡®star¡¯



library(stringr)
tt<-paste(unlist(SimplePos22(kakaotxt)))
head(tt,200)
#¸í»ç¸¸ °¡Á®¿À±â
alldta<-str_match_all(tt,"[°¡-ÆR]+/[N][C]|[°¡-ÆR]+/[N][Q]+")%>% unlist()

#Çü¿ë»ç¸¸ °¡Á®¿À±â
alldta2<-str_match_all(tt,"[°¡-ÆR]+/[P][V]+|[°¡-ÆR]+/[P][X]+|[°¡-ÆR]+/[P][A]+|[°¡-ÆR]+/[M][A]+")%>%unlist()

N<-str_replace_all(alldta,"/[N][C]","") %>%
  str_replace_all("/[N][Q]","") %>%unlist()
#¸í»ç·Î ÃßÃâµÈ ´Ü¾îµéÀÇ ºĞ·ùÇ¥ÀÎ /NC, /NQ µîÀ» Á¦°ÅÇÑ´Ù.

PNM<-str_replace_all(alldta2,"/[P][V]","") %>%
  str_replace_all("/[P][A]","") %>%
  str_replace_all("/[M][A]","") %>%
  str_replace_all("/[P][X]","") %>% unlist() 

#¸¶Âù°¡Áö·Î °¨Á¤¾î·Î ºĞ·ùÇÑ ´Ü¾îµéÀÇ ºĞ·ùÇ¥µéÀ» Á¦°ÅÇÑ´Ù.


DtaCorpusNC<-Corpus(VectorSource(N))
myTdmNC<-TermDocumentMatrix(DtaCorpusNC,control = list(wordLengths=c(4,10),
                                                       removePunctuation=T,removeNumbers=T,weighting=weightBin))
Encoding(myTdmNC$dimnames$Terms)="EUC-kr"
#tmÆĞÅ°Áö¿¡¼­ Á¦°øÇÏ´Â Corpus¸¦ ÅëÇØ ºĞ·ùµÈ ´Ü¾îµéÀÇ Çà·ÄÀ» ¸¸µç´Ù.
#CorpusÀÇ ¿ø¸®´Â ÇöÀç ±î¸Ô¾ú´Ù.

findFreqTerms(myTdmNC, lowfreq=10)
#Á¦´ë·Î µÇ¾ú´ÂÁö È®ÀÎ Â÷¿ø¿¡¼­ ÀÔ·ÂÇß´Ù. 

mtNC<-as.matrix(myTdmNC[0:30,0:30]) #Çà·Ä(matrix)·Î º¯È¯ÇÏ´Â °Ô »ó°ü¼º ºĞ¼®ÀÇ ÇÙ½ÉÀÌ´Ù.
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

wordcloud2(wordcount_top, fontFamily = '¹è´ŞÀÇ¹ÎÁ· ÇÑ³ª´Â ¿­ÇÑ»ì',
           figPath = fig, size = 1, color = "skyblue" )

letterCloud(wordcount_top, word = "R",
            fontFamily = '¹è´ŞÀÇ¹ÎÁ· ÇÑ³ª´Â ¿­ÇÑ»ì',size = 2)

letterCloud(wordcount_top, word = "WORDCLOUD2", 
            fontFamily = '¹è´ŞÀÇ¹ÎÁ· ÇÑ³ª´Â ¿­ÇÑ»ì', wordSize = 1 )






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
#                                          stopwords = c('³×','³ß','³Û','À¸',
#                                                        'À½','¿ò','¿À','Çæ','Ú')))
# tdm <- as.matrix(tdm)
# data <- data.frame(X=names(v),freq=v)



# 
# person1 <- length(data[grep(" you ",kakaotxt)])
# person2 <- length(data[grep(" ¾ÈÁ¤¹Î ",kakaotxt)])
# person3 <- length(data[grep(" ÀåÁ¤¿í ",kakaotxt)])
# person4 <- length(data[grep(" ¹é°è»ï ",kakaotxt)])
# person5 <- length(data[grep(" ¼­¿µÁÖ ",kakaotxt)])
# 
# talk_cnt <- c(person1,person2,person3,person4,person5)
# talk_name <- c("ÃÖÁß¿ø", "¾ÈÁ¤¹Î", "ÀåÁ¤¿í", "¹é°è»ï", "¼­¿µÁÖ")
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
