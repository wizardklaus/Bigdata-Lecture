install_github("saurfang/rCharts",ref="utf8-writelines")

# Page 1

library(Rcmdr)
library(plyr)
library(doBy)

getwd()
setwd("D:/Klaus Choi/KLAUS/2017_start/R/Instruction Elements/Final/Final exam")

size5th <- read.csv("5.csv",header=T)
size7th <- read.csv("7.csv",header=T)

str(size5th)
str(size7th)

cha <- rep("5",times=1391)
size5th <- cbind(size5th,cha)
cbind

cha <- rep("7",times=38)
size7th <- cbind(size7th,cha)

DF <- rbind(size5th,size7th)

DF01 <- summaryBy(height+weight~cha,DF)
DF01 <- data.frame(DF01$cha,DF01$height.mean/10,DF01$weight.mean)
names(DF01) <- c("cha","height","weight")
DF01

DF <- data.frame(DF$cha,DF$height/10,DF$weight)
names(DF) <- c("cha","height","weight")
DF

DFcha5 <- subset(DF,select=c(height,weight),subset=(cha=="5"))
DFcha7 <- subset(DF,select=c(height,weight),subset=(cha=="7"))
plot(lm(weight ~ height, DFcha5),which=c(1))
plot(lm(weight ~ height, DFcha7),which=c(1))
plot(lm(weight ~ height, DFcha7),which=c(4))
plot(DF$weight,DF$height, xlim=c(-30,120), ylim=c(-30,200))
plot(DFcha5, main="5차와 7차의 선형회기 직선 시각화")
abline(coef(lm(weight ~ height, DFcha5)))
abline(coef(lm(weight ~ height, DFcha7)))


library(Rcpp)
library("rCharts")
library(devtools)
V01 <- nPlot(height ~ weight, data=DF01, group="cha", type="multiBarHorizontalChart")
V01
with(DF,coplot(weight ~ height | cha))
title(main="2030세대 5차와 7차 인체치수자료 비교")
title(xlab="키")
title(ylab="몸무게")

# Page 2

library(KoNLP)
library(RColorBrewer)
library(wordcloud2)

library(XML)
searchUrl <- "http://apis.data.go.kr/1300000/jBGSSCJeongBo/list"
serviceKey <- "koy8yKa%2FNPxo51f44rnb9qsXSWbQTB6OVq5PaasqTsDqZbcKlKChSg3kocOXM2UNYHxksaOdHPawf1krmMDKrA%3D%3D"

url <- paste(searchUrl,"?ServiceKey=",serviceKey,"&numOfRows=10000","&pageNo=1",
             "&birth=1998",sep="")

xmlfile <- xmlParse(url)
xmltop <- xmlRoot(xmlfile)

DF021 <- xmlToDataFrame(getNodeSet(xmltop,"//item"),stringsAsFactors=F)
str(DF021)

DF02 <- DF021
DF02 <- DF02[,c(4,11)]
sapply(DF02,class)
DF02$height <- as.numeric(DF02$height)
DF02$weight <- as.numeric(DF02$weight)
plot(lm(weight ~ height, DF02),which=c(1))
plot(DF02, main="징병검사 신체정보 선형회기 그래프")
abline(coef(lm(weight ~ height, DF02)))
abline(coef(lm(weight ~ height, DFcha5)))

library(ggplot2)
library(ggthemes)
ggplot(data=DF02,aes(x=weight,y=height,colour='clarity')) + geom_point() + theme_wsj()
ggplot(data=DF02,aes(x=weight,y=height)) + geom_smooth(method="lm")


DF022 <- table(cut(DF02$height,breaks=4))
DF022 <- as.data.frame(DF022)
names(DF022) <- c("height","counts")
DF022
library(Rcpp)
library("rCharts")
library(devtools)
V02 <- nPlot(counts ~ height, data=DF022, group="height", type="multiBarHorizontalChart")
V02

DF023 <- table(cut(DF02$weight,breaks=4))
DF023 <- as.data.frame(DF022)
names(DF023) <- c("weight","counts")
DF023
library(Rcpp)
library("rCharts")
library(devtools)
V03 <- nPlot(counts ~ weight, data=DF023, group="weight", type="multiBarHorizontalChart")
V03


library(manipulate)
manipulate(
  plot( weight ~ height, data=DF02
        , axes=axes
        , cex=cex
        , pch=if(pch) 19 else 1)
  , axes=checkbox(TRUE, "Show axes")
  , cex=slider(0,5,initial=1,step=0.1,label="Point size")
  , pch=button("Fill points")
)

# page 3

library(RCurl)
library(XML)
library(KoNLP)
library(RColorBrewer)
library(wordcloud)

searchUrl2 <- "https://openapi.naver.com/v1/search/blog.xml"

client_id <- "2grDuh3XEqKvN6DFHhSD"
client_secret <- "QFEnl4fgJg"

query <- URLencode(iconv("남자빅사이즈","euc-kr","UTF-8"))
url2 <- paste(searchUrl2,"?query=",query,"&display=100",sep="")
doc <- getURL(url2,
              httpheader=c('Content-Type'="application/xml"
                               , 'X-Naver-Client-Id'=client_id
                               , 'X-Naver-Client-Secret'=client_secret))

doc2 <- htmlParse(doc, encoding="UTF-8")
text <- xpathApply(doc2, "//item/description", xmlValue)
text

useSejongDic()

noun <- sapply(text, extractNoun, USE.NAMES=F)
noun
noun2 <- unlist(noun)
noun2
noun2 <- Filter(function(x){nchar(x) >=2}, noun2)
noun2

noun2 <- gsub('\\d+','',noun2)
noun2 <- gsub('</b>','',noun2)
noun2 <- gsub('<b>','',noun2)
noun2 <- gsub('&amp','',noun2)
noun2 <- gsub('&lt','',noun2)
noun2 <- gsub('&quot','',noun2)
noun2 <- gsub("",'',noun2)
noun2 <- gsub('\'','',noun2)
noun2 <- gsub(' ','',noun2)
noun2 <- gsub('-','',noun2)
noun2

noun2 <- Filter(function(x){nchar(x) >=2}, noun2)
wordcount <- table(noun2)
head(sort(wordcount,decreasing=T),30)

palete <- brewer.pal(9,"Set1")
wordcloud(names(wordcount), freq=wordcount,
          scale=c(8,1), rot.per=0, min.freq=1,
          random.order=F, random.color=T, colors=palete)

# page 4

library(RCurl)
library(XML)
library(KoNLP)
library(RColorBrewer)
library(wordcloud)

searchUrl2 <- "https://openapi.naver.com/v1/search/blog.xml"

client_id <- "2grDuh3XEqKvN6DFHhSD"
client_secret <- "QFEnl4fgJg"

query <- URLencode(iconv("쇼핑스트리트","euc-kr","UTF-8"))
url2 <- paste(searchUrl2,"?query=",query,"&display=100",sep="")
doc <- getURL(url2,
              httpheader=c('Content-Type'="application/xml"
                           , 'X-Naver-Client-Id'=client_id
                           , 'X-Naver-Client-Secret'=client_secret))

doc2 <- htmlParse(doc, encoding="UTF-8")
text <- xpathApply(doc2, "//item/description", xmlValue)
text

useSejongDic()

noun <- sapply(text, extractNoun, USE.NAMES=F)
noun
noun2 <- unlist(noun)
noun2
noun2 <- Filter(function(x){nchar(x) >=2}, noun2)
noun2

noun2 <- gsub('\\d+','',noun2)
noun2 <- gsub('</b>','',noun2)
noun2 <- gsub('<b>','',noun2)
noun2 <- gsub('&amp','',noun2)
noun2 <- gsub('&lt','',noun2)
noun2 <- gsub('&quot','',noun2)
noun2 <- gsub("",'',noun2)
noun2 <- gsub('\'','',noun2)
noun2 <- gsub(' ','',noun2)
noun2 <- gsub('-','',noun2)
noun2

noun2 <- Filter(function(x){nchar(x) >=2}, noun2)
wordcount <- table(noun2)
head(sort(wordcount,decreasing=T),30)

palete <- brewer.pal(9,"Set1")
wordcloud(names(wordcount), freq=wordcount,
          scale=c(8,1), rot.per=0, min.freq=1,
          random.order=F, random.color=T, colors=palete)

# page 5

library(ggmap)
library(ggplot2)
addr <- c("서울특별시 중구 명동",
          "서울특별시 강남구 영동대로 513",
          "서울특별시 영등포구 국제금융로 10",
          "서울특별시 광진구 아차산로 200",
          "서울특별시 영등포구 영중로 15",
          "서울특별시 강남구 신사동",
          "서울특별시 서초구 반포동",
          "서울특별시 중구 을지로 281")
name <- c("명동쇼핑스트리트","코엑스몰","IFC몰","커먼그라운드","영등포타임스퀘어"
          , "가로수길", "파미에스테이션", "동대문디자인플라자")
gc <- geocode(enc2utf8(addr))
DF05 <- cbind(name,gc)
DF05
center <- c(mean(gc$lon),mean(gc$lat))
namecolor <- DF05[,1]
color <- as.integer(namecolor)*10

map <- get_googlemap(center=center,maptype="roadmap",zoom=12,marker=gc)
V05 <- ggmap(map,extent = "device") + geom_point(data=DF05, aes(x=DF05$lon, y=DF05$lat)
                                                 , size=2, alpha=0.7, color=color)
V051 <- V05 + geom_text(data=DF05, aes(x=DF05$lon, y=DF05$lat, label=DF05$name), size=5)
V051
ggsave("map.png")

# page 6

library(twitteR)
library(base64enc)
library(KoNLP)
library(RColorBrewer)
library(wordcloud)

options(httr_oauth_cache = T)

setup_twitter_oauth(
  consumer_key = "NHY572m33si1HywHV31TS2L4D",
  consumer_secret = "CVLbp6eEq7ta2Fy3ic6pJ5L5ELrTAaDDS6LthysAeyj988n7NW",
  access_token = "412876876-SWNM2OE3MiVlkhu6eY4fdpMq7R599aBUmXPLheUW",
  access_secret = "JY0ymGg3TpOORYQMIHZ6uLxW5Km4CsT86DDFWuiTaOjfk"
)
? searchTwitter()
keyword <- enc2utf8("커먼그라운드")
tweets <- searchTwitter(keyword,n=1000, resultType="recent", retryOnRateLimit=100)
tweets

text_ex <- sapply(tweets,function(t) t$getText())
text_ex

tweets_DF <- twListToDF(tweets)
tweets_DF$text

useSejongDic()
noun <- sapply(tweets_DF$text, extractNoun, USE.NAMES = F)
noun2 <- unlist(noun)
noun2 <- Filter(function(x){nchar(x)>=2},noun2)

noun2 <- gsub('\\d+', '', noun2)
noun2 <- gsub('</b>', '', noun2)
noun2 <- gsub('<b>', '', noun2)
noun2 <- gsub('&amp', '', noun2)
noun2 <- gsub('&lt', '', noun2)
noun2 <- gsub('&gt', '', noun2)
noun2 <- gsub('&quot', '', noun2)
noun2 <- gsub("", '', noun2)
noun2 <- gsub('\'', '', noun2)
noun2 <- gsub(' ', '', noun2)
noun2 <- gsub('-', '', noun2)

noun2 <- gsub('https', '', noun2)
noun2 <- gsub('RT', '', noun2)

wordcount <- table(noun2)
barplot(wordcount)

palete <- brewer.pal(9,"Set1")
wordcloud(names(wordcount), freq=wordcount,
          scale=c(8,1), rot.per=0.45, min.freq=1,
          random.order=F, random.color=T, colors=palete)
