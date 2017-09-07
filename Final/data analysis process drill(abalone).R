# 빅데이터 분석 시나리오1 실제 코딩방향(사용데이터 : Abalone)
# - 분석 목표(주제)를 설정하고 가설세우기
# - 데이터 불러오기
setwd("D:/R")
DF <- read.csv("D:/R/abalone/data/abalone.csv", header = T)
# DF <- fread("example_coffee.csv",header=T, stringsAsFactors=T, data.table=F)

# 데이터 정의하기
# - 데이터의 타이틀, 칼럼 수 / 로우 수 어떤것에 대한 데이터인지 전체적으로 정의
# Title : Abalone, 자라에 대한 데이터
str(DF)
# num of columns : 
ncol(DF)
# num of rows :
nrow(DF)
# 범주형 데이터인지 연속형 데이터인지 분류하기
# 범주형 데이터: Sex
# 연속형 데이터: 그 외
# 데이터의 사분면범위, 최소/최대값, 중앙값, 평균 등을 알아봄
summary(DF)
# 분석은 신체조건, 나이를 중심으로 분석하여 추정하는 모델로 한다
# 중앙값과 평균값 차이가 크지 않으므로 아웃라이어는 베제안함

# 분석을 시작하기전에 필요없는 데이터를 삭제
DF <- subset(DF,select=c(-Shell.weight, -Viscera.weight, -Shucked.weight, -Height))

library(doBy)
GP1 <- summaryBy(DF$Whole.weight ~ DF$Rings, DF)
GP2 <- summaryBy(DF$Length ~ DF$Rings, DF)
GP3 <- summaryBy(DF$Diameter ~ DF$Rings, DF)
plot(GP1)
plot(GP2)
plot(GP3)
# 남자 자라의 평균 나이: 남자(10), 여자(11), 유아(8)
# 나이에 따른 총 무게, 길이, 높이가 점점 증가하는 선을 그리고 있다

# 성별에 따라서도 평균 총무게를 계산했다
GDW <- split(DF$Whole.weight,DF$Sex)
sapply(GDW,mean)
barplot(sapply(GDW,mean))
GDWD <- as.data.frame(sapply(GDW,mean))
# barplot(tapply(DF$Whole.weight,DF$Sex,mean))
# 데이터프레임으로 나타내 보았다.
Freq.GDW <- prop.table(as.data.frame(sapply(GDW,mean)))
sum(Freq.GDW)
# 상대도수를 구하고나서 sum을 했을 때 1이 나오는 것을 확인
Freq.GDW <- t(Freq.GDW)
GDWD <- t(GDWD)
# 보기 쉽게 하기위해 행과 열을 바꿔주었다
GDWDT <- rbind(GDWD,Freq.GDW)
GDWDT <- addmargins(GDWDT,margin=2)
GDWDT
# 성별별 총 무게 평균과 상대도수분포표를 구했다
# 하나의 데이터로 묶어주어 도수분포표를 만들었다
# 평균적으로 남자보다는 여자가 더 무게가 많이 나가는 것을 확인했다.

# 자라의 총무게를 4등분을 하여 남녀 상대도수분포표를 만들것이다
Weight <- cut(DF$Whole.weight,breaks=4)
FreqOfWeight <- table(Weight)
FreqOfWeight <- rbind(FreqOfWeight,prop.table(FreqOfWeight))
rownames(FreqOfWeight)[2] <- "RelativeFreq"
# 상대도수를 구하고 2번째 행의 이름을 지어준다
FreqOfWeight <- rbind(FreqOfWeight,(CumuFreq <- cumsum(FreqOfWeight[2,])))
rownames(FreqOfWeight)[3] <- "CumuFreq"
# 누적상대도수를 구하고 3번째 행의 이름을 지어준다
rownames(FreqOfWeight) <- c("도수","상대도수","누적도수")
FreqOfWeight <- addmargins(FreqOfWeight,margin=2)
FreqOfWeight
# 총 무게에 따른 4구역으로 나눈 누적상대분포표까지 완성

# 그렇다면 시각화를 시작하여
# 총무게, 고기무게, 껍질무게에 따른 나이를 도표로 그린다
# 성별별 총무게 평균을 이용하여 barplot 나타내기
# barplot은 명목형 변수를 시각화하기 좋은 그래프다
VIZ1 <- GDWDT[-2,-4]
VIZ1 <- t(data.frame(VIZ1))
rownames(VIZ1)[1] <- "Sex"
VIZ1
barplot(VIZ1)

# 아까전의 총 무게를 4구역으로 나눈 FreqOfWeight를 그래프로
FreqOfWeight
# 상대도수와 누적도수를 제거
library(ggplot2)
library(ggthemes)
VIZ2 <- t(data.frame(FreqOfWeight[1,]))
rownames(VIZ2)[1] <- "Weight"
class(VIZ2)
VIZ2 <- as.data.frame(VIZ2)

##지금까지 정리##
# 자라는 나이가 많을수록 몸무게가 많이 나간다.
# 몸무게를 4구간으로 나눠서 보니 90%가량이 1, 2구간에 속해있었다.

#****작동안됨!!!****
# 구간을 10으로 쪼개서 ggplot2로 나타내보기로 하자
# CUT <- table(cut(DF$Length,breaks=10))
# CUTDF <- as.data.frame(CUT)
# library(ggplot2)
# library(ggthemes)
# ggplot(data=DF,aes(x=DF$Length))
# geom_freqpoly(binwidth=10,size=1.4 colour="orange")+
# theme_wsj()
#*******************

# 다른방향으로 다시 분석
range(DF$Rings,na.rm = T)
# 나이가 제일 적은 것과 많은 것을 출력, na.rm=T 인자를 넣어줘야 결측치를 제외
# 제일 나이가 어린것의 정보를 출력해보고 싶다
subset(DF,subset=(Rings==1))

# Length 칼럼을 2번째 자리까지 구해서 반올림한걸 숫자를 센 것
V <- table(round(DF$Length,digits=2))
library(ggplot2)
qplot(Length,data=DF,geom="bar")
# 막대그래프를 이용해 나타내보았다.

# 분할표를 이용하기, 나이대별 길이와 무게를 나타냄
# table(DF$Ringst,DF$Sex) ****계산안됨****

# 다시 몸무게를 20단계로 나눔
VV <- table(cut(DF$Length,breaks = 20))
library(ggplot2)
ggplot(data=DF, aes(x=DF$Length, y=DF$Whole.weight))+geom_point()


