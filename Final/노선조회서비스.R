
# 버스 경로 ID 확인(getBusRouteList)

library("XML")

API_key <- "bmbBRsCJOA1EpIK8jgnm6LPMzcKqAyQyewr%2F7U9dTMepFlMRE%2FQttSNB7gD2p7r1sxuiCuybpJUfDFq1pQlnUA%3D%3D"

url <- paste("http://ws.bus.go.kr/api/rest/busRouteInfo/getBusRouteList?ServiceKey=",API_key,sep="")
# sep=""는 문자열 사이를 공백없이 붙여 연결하라는 의미.

xmlfile <- xmlParse(url)
# xml파일을 R에서 인식할 수 있게 구조 변경
xmltop <- xmlRoot(xmlfile)
# XML 문서 객체의 루트 노드에 접근, 필요한 컨텐츠만 가져오기 위함.
xmltop

# 노선의 지도상 경로를 리턴 (getRoutePathList)

busRouteId <- "204000041"
# 9004성남 버스의 ID번호

Route_url <- paste("http://ws.bus.go.kr/api/rest/busRouteInfo/getRoutePath?ServiceKey=",API_key, "&busRouteId=", busRouteId, sep="")

xmlfile2 <- xmlParse(Route_url)
xmltop2 <- xmlRoot(xmlfile2)
xmltop2



# 버스 실시간 위치를 출력

install.packages("ggmap")
#google map을 이용하기 위한 패키지

library("XML")
library("ggmap")

API_key <- "bmbBRsCJOA1EpIK8jgnm6LPMzcKqAyQyewr%2F7U9dTMepFlMRE%2FQttSNB7gD2p7r1sxuiCuybpJUfDFq1pQlnUA%3D%3D"

bus_no <- "462"
# 찾고자 하는 버스 번호

url <- paste("http://ws.bus.go.kr/api/rest/busRouteInfo/getBusRouteList?ServiceKey=",API_key,"&strSrch=",bus_no,sep="")
# sep=""는 문자열 사이를 공백없이 붙여 연결하라는 의미.

xmlfile <- xmlParse(url)
# xml파일을 R에서 인식할 수 있게 구조 변경
xmltop <- xmlRoot(xmlfile)
# XML 문서 객체의 루트 노드에 접근, 필요한 컨텐츠만 가져오기 위함.

DF <- xmlToDataFrame(getNodeSet(xmlfile,"//itemList"))
# xmlfile의 노드 이름이 ItemList인 모든 노드의 서브노드들을 항목명으로 하고, 그 데이터들로 구성되는 데이터프레임으로 변환
DF

busRouteId <- "100100458"
# 462번 버스 ID번호

final_url <- paste("http://ws.bus.go.kr/api/rest/busRouteInfo/getRoutePath?ServiceKey=",API_key, "&busRouteId=", busRouteId, sep="")

doc <- xmlParse(final_url)
top <- xmlRoot(doc)
top
# 운행 중인 462번 버스에 대한 위치정보들

DF2 <- xmlToDataFrame(getNodeSet(doc,"//itemList"))
DF2
str(DF2)

DF2$gpsX <- as.numeric(as.character(DF2$gpsX))
DF2$gpsY <- as.numeric(as.character(DF2$gpsY))
str(DF2)
# factor를 문자열로 변환 후 숫자로 변환.

gc <- data.frame(lon=DF2$gpsX,lat=DF2$gpsY)
gc

cent <- c(mean(gc$lon),mean(gc$lat))
# 지도상 중심을 표시하고 싶은 줌심좌표
gc <- gc[c(1:80),]
# 데이터가 너무 많아 메모리 초과를 방지 하기 위해 앞에 몇개만 표시함
map <- get_googlemap(center=cent,maptype="roadmap",zoom=14 ,marker=gc)
# 지도의 중심은 cent, type은 로드맵, 
ggmap(map,extent = "device")
