

install.packages(c('Rfacebook','KoNLP','wordcloud2'))
# Rfacebook : facebook API 를 사용하기 위한 패키지
# KoNLP : Korea Natural Language Processing Package (한국어 자연어 처리 패키지)
library(Rfacebook)
library(KoNLP)
Sys.setenv(JAVA_HOME="C:/Program Files/Java/jre1.8.0_121")
# rjava 오류가 발생했을 시 해결 방법.
library(wordcloud2)

Token <- 'EAACEdEose0cBANzsUAV1qkzSGNxNomxvm6oHjdZBrTsgh1a2rj6hBLb0QdwfFWC9Ex4ZCjDQF7L7skgefwpC3D0xoMRjOgQvnjgILkRmZC73YOY3try9XFvZBZBRD68x4BGR2mXgt1l4krpqRYvUZA5cCQZAOjEtQiujA0DVnNYmsniEeo7UbkIdD3RFpkC924ZD'
# fackebook developer 에 들어가서 사용자 엑세스 토큰을 받아와야 함 
# https://developers.facebook.com/tools/explorer

fb_page <- getPage(page="moonbyun1",token=Token, n=2000, since='2016/01/01',until='2017/5/5')
# getPage에 내 정보를 가져온다는 것으로 me입력. moonbyun1은 문재
# n은 최대 가져올 post 정보의 개수, since와 until은 가져오려는 데이터의 기간.

fb_page$message
#작성한 글 확인

useNIADic()
#텍스트 분석을 위해 형태소 분석이 필요해 형태소 사전을 다운받아야 함.

all.reviews <- fb_page$message
all.reviews <- all.reviews[!is.na(all.reviews)]
#na값 제거

nouns <- extractNoun(all.reviews)
#명사 추출
nouns_norm <- Map(function(x){if(!is.na(x)&&is.ascii(x))toupper(x)else x},unlist(nouns))
#불필요한 데이터 삭제

cnts <- table(unlist(nouns_norm))
cnts_ <- cnts[cnts > 2 & nchar(names(cnts))>1]

wordcloud2(data.frame(word=names(cnts_),freq=as.numeric(cnts_)), color="random-light",backgroundColor = "black", shape = "cloud")
# 출력 결과는 빈도수에 따라 글씨 크기가 달라지고 색상은 random값으로 변홤. 