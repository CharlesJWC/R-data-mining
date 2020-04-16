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
library(RColorBrewer) # ����Ŭ���� ���� ���̺귯��
library(dplyr) 
# library(extrafont)

#��Ʈ��
# library(googleVis)

# ������ �˻� ����� 
# library(tm) # ������ �˻��
# library(qgraph) # ������ ��� �ð�ȭ �׷���


useSejongDic()

setwd("D:/Workspace/R_Workspace/SNS")
mergeUserDic(data.frame(c(
                          
                          # "����","�뿵","����","���۳���","����","�߿�","�־ȿ�",
                          # "�ϼ�","��â","��â","��â","�㰢","����","���̿ø�","��ö�",
                          # "ÿ��","�÷�","����","����","ȸ��","��ȸ��","�ѹ�","Ŀ��",
                          # "����","���","����","���Ի�","�ϵ�ĳ��","��","������","ȭ��",
                          # "�ް�","����","������","����","���ɸ�","���","�����","���ƶ�",
                          # "��Ĭ","�ָ�","����","�Ź�","ȥ��","�ֽĽ�","���ɸ�","����",
                          # "������","������","�⿥Ƽ","ťƼ","������","����","�Ĺ�","����",
                          # "����","��Ʋ","�ε�ε�","����","����","���","����",
                          # "����","�ι���","����","����","��û��û","�˹�","����","�ֱ�",
                          # "���","����","����","���ġ","��Į","����","��ȯ","��ȯ","����",
                          # "��ķ","����","������","����Ϲ�","�����Ϲ�","��õ","�۵�",
                          # "��õ","��Ȧ","���п���","������û","����","������ȸ","ȭ����"
                          # "����","�ٸ���","����ȭ","�Ϻ�","����","����",
                          # "����","�ް�","����ȭ","���̵�","��ġ��","����",
                          # "����ȭ","�ް�����","��̴Ͻ�Ʈ","���","����",
                          # "Ż����","��ݵ�","õ����","�ھ߰�","�³���","���"
                          # "���ڶ�","����","���ΰ�","����","�Ǹ���","���Ǹ���",
                          # "�ڴ°�","��Ÿ","�ҿ�","����","�Ұ���","����ö","���",
                          # "�о�","�ް�","����","��Ƽ","�Ƚ�","���Ƹ�","����",
                          # "������Ʈ","��ģ","����ģ��","�ٰ���","����","��õ",
                          # "��ī��","����","�ٺ�","�̸�Ʈ","�ԽǷ�Ƽ","���ξ�",
                          # "����","�ǽù�","pc��","��Ż��","����","��������",
                          # "������ġ","����","���Ѹ���","��","���","�Ѹ���",
                          # "����","��Ű��","�����","������","1��2��","����",
                          # "����","����","�ó���","����","����","����Ⱓ"
                          ), "ncn"))

f <- file("OCH.txt", blocking=F, encoding = "UTF-8")
kakaotxt <- iconv(readLines(f), "UTF-8", "EUC-KR")

kakaotxt <- gsub('KakaoTalk .+', '', kakaotxt)
kakaotxt <- gsub('Date Saved .+', '', kakaotxt)
kakaotxt <- gsub('.+( : )', '', kakaotxt)
kakaotxt <- gsub('.+(AM)', '', kakaotxt)
kakaotxt <- gsub('.+(PM)', '', kakaotxt)
kakaotxt <- gsub('\\(�̸�Ƽ��\\)','', kakaotxt)
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
kakaotxt <- gsub('511','������',kakaotxt)
kakaotxt <- gsub("\\d+",'', kakaotxt)
kakaotxt <- gsub('[~!@#$%&*()_+=?<>.,]','', kakaotxt)
kakaotxt <- gsub('[a-zA-z]','',kakaotxt)
# kakaotxt <- gsub('[��-��]','',kakaotxt)
# kakaotxt <- gsub('(��|��)','',kakaotxt)

kakaotxt <- gsub("����\\S*", "���", kakaotxt)
kakaotxt <- gsub("����\\S*", "���", kakaotxt)
kakaotxt <- gsub("����\\S*", "����", kakaotxt)
kakaotxt <- gsub('����\\S*','��ī',kakaotxt)
kakaotxt <- gsub("������", "���̵�", kakaotxt)
kakaotxt <- gsub("����","����",kakaotxt)
kakaotxt <- gsub("����","����",kakaotxt)
kakaotxt <- gsub('����','���̹���', kakaotxt)
kakaotxt <- gsub('����','����',kakaotxt)
kakaotxt <- gsub('����','����',kakaotxt)
kakaotxt <- gsub('����','����',kakaotxt)
kakaotxt <- gsub('������','�ó���',kakaotxt)
kakaotxt <- gsub('����','����',kakaotxt)
kakaotxt <- gsub('��','����',kakaotxt)
kakaotxt <- gsub('����','����',kakaotxt)
kakaotxt <- gsub('����','����',kakaotxt)
kakaotxt <- gsub('��������','�ε�ε�',kakaotxt)


kakaotxt <- gsub('����','�Ϻ�',kakaotxt)
kakaotxt <- gsub('��������','�׷��ϴ�',kakaotxt)
kakaotxt <- gsub('������','����ȭ',kakaotxt)
kakaotxt <- gsub('������','����ȭ',kakaotxt)
kakaotxt <- gsub('����','����',kakaotxt)
kakaotxt <- gsub('�������� ����','�׷������ʴ�',kakaotxt)
kakaotxt <- gsub('������������','�׷������ʴ�',kakaotxt)
kakaotxt <- gsub('��������','�׷�����',kakaotxt)
kakaotxt <- gsub('�ް�����','�ް�',kakaotxt)
kakaotxt <- gsub('[��-��]','',kakaotxt)

kakaotxt <- na.omit(kakaotxt)

nouns <- sapply(kakaotxt, extractNoun, USE.NAMES = FALSE)
undata=unlist(nouns)

#Ư�� ���� ����

# undata = gsub("�ڿ�\\S*", "�ڿ�", undata)
# \S ��� ���� �ڿ� �ڿ� ���� ����,��,������ ������ ��� ���ڶ�� ���̰�, 
# ���� *�� �ڿ� ����ڰ� ���� ������ٴ� ���Դϴ�.

undata <- gsub('�𸣰�','�𸣰ڴ�',undata)
undata <- gsub('��ġ��','��ġ�ڴ�',undata)
undata <- gsub('�����','����ʹ�',undata)
undata <- gsub('������','�����̴�',undata)
undata <- gsub('������','�����ΰ�',undata)
undata <- gsub('�˰���','�˰��ִ�',undata)
undata <- gsub('�־�$','�־���',undata)
undata <- gsub('�Ͽ�$','�Ͽ���',undata)
undata <- gsub('�ʾ�$','�ʾҴ�',undata)
undata <- gsub('�ʰ�','�ʰڴ�',undata)
undata <- gsub('�ϰ�$','�ϰڴ�',undata)
undata <- gsub('�׷�','�׷���',undata)
undata <- gsub('�ٸ�','�ٸ���',undata)
undata <- gsub('����$','���Ҵ�',undata)
undata <- gsub('���о���','���о��Ѵ�',undata)
undata <- gsub('���а�','���аڴ�',undata)
undata <- gsub('����','���ΰ�',undata) 
undata <- gsub('����$','������',undata)
undata <- gsub('��$','����',undata)
undata <- gsub('����$','������Ʈ',undata)
undata <- gsub('����$','����ȭ',undata)
undata <- gsub('�ε�$','�ε�ε�',undata)
undata <- gsub('������$','511',undata)
undata <- gsub('���̹�$','���̹���',undata)
undata <- gsub('����$','�����',undata)
undata <- gsub('����$','������',undata)
undata <- gsub('����$','�����',undata)
undata <- gsub('���$','�����',undata)
undata <- gsub('����$','���ڴ�',undata)
undata <- gsub('�η�$','�η���',undata)
undata <- gsub('���׻�$','���׻���',undata)
undata <- gsub('��������$','���������',undata)
undata <- gsub('ũ����$','ũ��������',undata)
undata <- gsub('�ֱ�$','�ֱ׷�',undata)
undata <- gsub('����$','���̷�',undata)

#���ڼ� ���� 
data2= Filter(function(x){nchar(x)>=2}, undata)
wordcount <- table(data2)
# head(wordcount2)
wordcount_top <-head(sort(wordcount, decreasing = TRUE),700)
head(wordcount_top, 700)
# View(wordcount_top)

# table Ȥ�� csv���Ϸ� ������� ���� �����ø� ���� �ڵ� �������ּ���.
# write(wordcount, "2017 ��ȭü�������� wordcloud.txt")  
# write.csv(wordcount, "2017 ��ȭü�������� wordcloud.csv")


#���� 
display.brewer.all()
col <- brewer.pal(11, "Spectral")
col <- brewer.pal(12, "Set3")
col <- brewer.pal(7, "Set2")
col <- brewer.pal(12, "Paired")
col <- brewer.pal(12, "Accent")

#��Ʈ 
windowsFonts(font=windowsFont("12�Ե���Ʈ�ູBold"))
windowsFonts(font=windowsFont("Koverwatch"))
windowsFonts(font=windowsFont("����������OTF ExtraBold"))
windowsFonts(font=windowsFont("�߳��� ��ü B"))
windowsFonts(font=windowsFont("����ǹ��� �ѳ��� ���ѻ�"))
windowsFonts(font=windowsFont("��ü�� ��ü �긲ü"))


wordcloud(names(wordcount_top), freq = wordcount_top, scale=c(5,1), rot.per=0.2, min.freq=5, random.order=FALSE, random.color=TRUE, colors=col, family = "font")


wordcloud2(wordcount_top, fontFamily = '����ǹ��� �ѳ��� ���ѻ�', minSize = 5,
           size = 1.2, color = "random-light", backgroundColor = "black",
           rotateRatio = 0.25, shape = 'pentagon',
           shuffle = TRUE, ellipticity = 1)
# ��circle�� (default), ��cardioid��, ��diamond�� (alias of square), ��triangle-forward��, ��triangle��, ��pentagon��, and ��star��



library(stringr)
tt<-paste(unlist(SimplePos22(kakaotxt)))
head(tt,200)
#��縸 ��������
alldta<-str_match_all(tt,"[��-�R]+/[N][C]|[��-�R]+/[N][Q]+")%>% unlist()

#����縸 ��������
alldta2<-str_match_all(tt,"[��-�R]+/[P][V]+|[��-�R]+/[P][X]+|[��-�R]+/[P][A]+|[��-�R]+/[M][A]+")%>%unlist()

N<-str_replace_all(alldta,"/[N][C]","") %>%
  str_replace_all("/[N][Q]","") %>%unlist()
#���� ����� �ܾ���� �з�ǥ�� /NC, /NQ ���� �����Ѵ�.

PNM<-str_replace_all(alldta2,"/[P][V]","") %>%
  str_replace_all("/[P][A]","") %>%
  str_replace_all("/[M][A]","") %>%
  str_replace_all("/[P][X]","") %>% unlist() 

#���������� ������� �з��� �ܾ���� �з�ǥ���� �����Ѵ�.


DtaCorpusNC<-Corpus(VectorSource(N))
myTdmNC<-TermDocumentMatrix(DtaCorpusNC,control = list(wordLengths=c(4,10),
                                                       removePunctuation=T,removeNumbers=T,weighting=weightBin))
Encoding(myTdmNC$dimnames$Terms)="EUC-kr"
#tm��Ű������ �����ϴ� Corpus�� ���� �з��� �ܾ���� ����� �����.
#Corpus�� ������ ���� ��Ծ���.

findFreqTerms(myTdmNC, lowfreq=10)
#����� �Ǿ����� Ȯ�� �������� �Է��ߴ�. 

mtNC<-as.matrix(myTdmNC[0:30,0:30]) #���(matrix)�� ��ȯ�ϴ� �� ����� �м��� �ٽ��̴�.
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

wordcloud2(wordcount_top, fontFamily = '����ǹ��� �ѳ��� ���ѻ�',
           figPath = fig, size = 1, color = "skyblue" )

letterCloud(wordcount_top, word = "R",
            fontFamily = '����ǹ��� �ѳ��� ���ѻ�',size = 2)

letterCloud(wordcount_top, word = "WORDCLOUD2", 
            fontFamily = '����ǹ��� �ѳ��� ���ѻ�', wordSize = 1 )






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
#                                          stopwords = c('��','��','��','��',
#                                                        '��','��','��','��','��')))
# tdm <- as.matrix(tdm)
# data <- data.frame(X=names(v),freq=v)



# 
# person1 <- length(data[grep(" you ",kakaotxt)])
# person2 <- length(data[grep(" ������ ",kakaotxt)])
# person3 <- length(data[grep(" ������ ",kakaotxt)])
# person4 <- length(data[grep(" ���� ",kakaotxt)])
# person5 <- length(data[grep(" ������ ",kakaotxt)])
# 
# talk_cnt <- c(person1,person2,person3,person4,person5)
# talk_name <- c("���߿�", "������", "������", "����", "������")
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
