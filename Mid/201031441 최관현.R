
DF <- read.csv("D:/R/myData.csv",header=T) #excel파일을 csv로 형식변환한 상태

ncol(DF) #148
nrow(DF) #6421

DF_my <- DF[,c(1,2,7,18,46,57,84)]
class(DF_my)
colnames(DF_my) <- c("Sex","Age","BMI","Height","Weight","Waist","Leg")

library(doBy)
Data1 <- summaryBy(DF_my$Weight ~ DF_my$Age, na.rm=T, DF_my)
Data2 <- summaryBy(DF_my$Waist ~ DF_my$Age, na.rm=T, DF_my)
Data3 <- summaryBy(DF_my$Weight ~ DF_my$Sex, na.rm=T, DF_my)

plot(Data1)
plot(Data2)
plot(Data3)
library(plyr)
Data4 <- split(DF_my$BMI,DF_my$Sex)
sapply(Data4.mean) #sapply를 이용하여 split으로 나이별로 BMI를 계산햇던 것을 평균내기
barplot(sapply(Data4,mean))
Data5 <- as.data.frame(sapply(Data4,mean))
Freq.Data4 <- prop.table(as.data.frame(sapply(Data4,mean))) # 상대분포 구하기
sum(Freq.Data4) # 모두 더해서 1이 나오는 것을 확인
Freq.Data4 <- t(Freq.Data4) #전치행렬 사용하기 보기 쉽게 만들어 준다

Data6 <- rbind(Data5,Freq.Data4)
Data6 <- addmargins(Data6, margin = 2)
# 성별별 BMI 평균과 상대도수분포표를 구했다.
# 하나의 데이터로 묶어주어 도수분포표를 만들었다.

Weight <- cut(DF_my$Weight,breaks=4)
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
VIZ1 <- Data5[-2,-4]
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

# 다른방향으로 다시 분석
range(DF$Sex,na.rm = T)
# 나이가 제일 적은 것과 많은 것을 출력, na.rm=T 인자를 넣어줘야 결측치를 제외
# 제일 나이가 어린것의 정보를 출력해보고 싶다
subset(DF,subset=(Sex==15))

# Hegiht 칼럼을 2번째 자리까지 구해서 반올림한걸 숫자를 센 것
V <- table(round(DF$Height,digits=2))
library(ggplot2)
qplot(Length,data=DF,geom="bar")
# 막대그래프를 이용해 나타내보았다.

# 분할표를 이용하기, 나이대별 길이와 무게를 나타냄
# table(DF$Sex,DF$Sex) ****계산안됨****

# 다시 몸무게를 20단계로 나눔
VV <- table(cut(DF$Height,breaks = 20))
library(ggplot2)
ggplot(data=DF, aes(x=DF$Height, y=DF$Weight))+geom_point()

