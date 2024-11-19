## R 코딩 플러스 

# 8.3

# 패키지 다운로드
install.packages("XML")
install.packages("ggplot2")
install.packages("RCurl")
library(RCurl)
library(XML)
library(ggplot2)

# API의 End Point URL
api <- "http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst"

# API 인증키 (공공데이터 포털에서 발급받은 키를 입력 -> 일반 인증키(Encoding) 추가하기! )
api_key <- "ctblSVIlq4G8POwFjJsKfIV%2F0lzVCruQ2CW0SRmSkIQhKVJGlhMoUhhMAqwWNwPnB%2FhJhfVubvDudXMil10tpg%3D%3D"

# 요청변수
numOfRows <- 10 # 한 페이지에 보여줄 데이터 개수
pageNo    <- 1 # 페이지 번호
itemCode  <- "PM10" # 조회할 항목 코드 (PM10: 미세먼지)
dataGubun <- "HOUR" # 데이터 구분 (HOUR: 시간별)
searchCondition <- "MONTH" # 조회 조건 (MONTH: 월간)

# 요청 URL 생성
url <- paste(api,
             "?serviceKey=", api_key, # 서비스 키 추가
             "&numOfRows=", numOfRows, # 한 페이지에 포함할 데이터 개수
             "&pageNo=", pageNo, # 페이지 번호 추가
             "&itemCode=", itemCode, # 조회 항목 코드 추가
             "&dataGubun=", dataGubun, # 데이터 구분 추가
             "&searchCondition=", searchCondition, # 조회 조건 추가
             sep="")

url

# XML 문서 다운로드 및 파싱
# xmlParse() 함수는 XML 데이터를 파싱하여 데이터를 구조적이고 계층적인 객체로 변환한다.
xmlFile <- xmlParse(url) # API로 호출한 XML 데이터를 파싱하여 XML 문서 객체 생성
xmlFile

# XML 데이터를 데이터프레임으로 변환
# # XML 문서에서 "//items/item" 경로의 노드를 추출하여 데이터프레임 형식으로 변환
df <- xmlToDataFrame(getNodeSet(xmlFile, "//items/item"))
df

# 시간대별 서울지역의 미세먼지 농도 변화를 막대그래프로 시각화
ggplot(data=df, aes(x=dataTime, y=seoul)) +
  geom_bar(stat="identity", fill="orange") +
  theme(axis.text.x=element_text(angle=90)) +
  labs(title="시간대별 서울지역의 미세먼지 농도 변화", x = "측정일시", y = "농도")


# 8.4 지역별 미세먼지 농도의 지도 분포포

# install.packages("ggplot2")
# install.packages("rgdal")
# install.packages("XML")
library(ggplot2)
library(rgdal)
library(XML)

api <- "http://apis.data.go.kr/B552584/ArpltnStatsSvc/getCtprvnMesureLIst"
api_key <- "RbxkM3C1miKISOb%2FYf28pvExZNQvE1KwHGw6SSIos9tHCdDG0brdBIvfyCXkpOhVTv3d8twnCSKhD%2FiHZ7PwWA%3D%3D"
numOfRows <- 10
pageNo    <- 1
itemCode  <- "PM10"
dataGubun <- "HOUR"
searchCondition <- "MONTH"
url <- paste(api,
             "?serviceKey=", api_key,
             "&numOfRows=", numOfRows,
             "&pageNo=", pageNo,
             "&itemCode=", itemCode,
             "&dataGubun=", dataGubun,
             "&searchCondition=", searchCondition,
             sep="")
url             
  
xmlFile <- xmlParse(url)
xmlFile           

df <- xmlToDataFrame(getNodeSet(xmlFile, "//items/item"))
df

#첫번째 시간대의 지역별 미세먼지 농도도
pm <- df[1, c(1:16, 19)]
pm

# 전치행렬을 이용해 행, 열을 바꿈꿈
pm.region <- t(pm) 
pm.region

df.region <- as.data.frame(pm.region)
df.region

# 열 이름을 'PM10"으로 수정정
colnames(df.region) <- "PM10"

df.region$NAME <- c("대구광역시", "충청남도", "인천광역시", "대전광역시", "경상북도", 
                    "세종특별자치시", "광주광역시", "전라북도", "강원도", "울산광역시", 
                    "전라남도", "서울특별시", "부산광역시", "제주특별자치도", "충청북도", 
                    "경상남도", "경기도")
df.region

setwd("c:/rdata")
install.packages("rgdal")
library(rgdal)


map <- readOGR('Z_NGII_N3A_G0010000.shp', encoding = "euc-kr")
crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
map <- spTransform(map, CRSobj = crs)

# 지역별 코드
df_map_info <- map@data
df_map_info

#지리좌표 정보를 데이터 프레임으로 변환환
df_map <- fortify(map)
head(df_map)

# id(시도)별로 경도와 위도의 평균값 계산
longlat<-aggregate(df_map[, 1:2], list(df_map$id), mean)
longlat

# 첫 번째 항목명을 'id'로 수정
colnames(longlat)[1] <- "id"

#문자를 숫자로 변환
longlat$id <- as.numeric(longlat$id)

#id 값의 오름차순 정렬렬
longlat <- longlat[order(longlat$id),]
longlat

# id 순에 따른 지역명 통합(열추가)
longlat <- cbind(longlat, NAME=df_map_info[,3])
longlat

df.PM10 <- merge(x = df.region, y = longlat, by = "NAME", all = TRUE)
df.PM10

ggplot() +
  geom_polygon(data = df_map, 
               aes(x = long, y = lat, group = group), 
               fill = "white", alpha=0.5, 
               color="black") +
  geom_point(data=df.PM10, 
             aes(x=long, y=lat, size=PM10),
             shape=21, color='black', 
             fill='red', alpha=0.3) +
  theme(legend.position = "none") + 
  labs(title="지진분포", x="경도", y="위도")
