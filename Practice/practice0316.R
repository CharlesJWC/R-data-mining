x <- 1;
y <- sqrt(9)
4 * x -> z

#x2 <- [1, 2, 3];
#read.csv();

# ls()
# objects()
#rm(x)


bcData <- read.csv("D:/Workspace/R_Workspace/Data/chapter 3/wisc_bc_data.csv")
# 윈도우 c:\data -> 리눅스/유닉스 c:/Data or C:\\Data 
# 경로에 한글이 들어가 있을 경우 인식하지 못함
# 유저, 경로, 폴더명에 한칸 스페이스하지 말 것 
prop.table(table(bcData$diagnosis)) # 비율로 나타낼수 있음 62.7% 37.2%
library(varhandle)
# install.packages("varhandle")
temp <- unfactor(bcData$diagnosis)
temp2 <- bcData$diagnosis
class(temp)
class(temp2)
is.vector(temp)
is.vector(temp2)

mean(bcData$radius_mean)
median(bcData$radius_mean)

min(bcData$radius_mean)
which.min(bcData$radius_mean)
bcData$radius_mean[which.min(bcData$radius_mean)]

range(bcData$radius_mean)
summary(bcData$radius_mean)
var(bcData$radius_mean)
sd(bcData$radius_mean)
hist(bcData$radius_mean)

install.packages("fBasics")

install.packages("timeDate")
install.packages("timeSeries")
library(fBasics)
hist(mtcars$mpg)
skewness(mtcars$mpg)
kurtosis(mtcars$mpg)

sum(x)

# n차 차분 : diff(x, lag=n)
diff(x, lag=1)

length(x)

# 벡터에서 length()는 관측값 개수를 계산해서 보여줍니다.
# 데이터 프레임에서는 column 개수를 나타내주고요, 
# 데이터 프레임의 특정 변수를 지정하면 그 특정 변수의 관측값의 개수를 세서 보여줍니다.



# factorvariable 이면 에러 (그런 함수가 있음)
# character 이면 에러 없음
# 분석하는 사람이 제일 중요한게  domain knowledge가 중요하다 (데이터와 데이터 비지니스) 
# Feature expression / Feature Selection
# 평균이 다르면 표준화 해주어야함