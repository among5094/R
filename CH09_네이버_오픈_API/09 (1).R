## R 코딩 플러스

# 9.4

install.packages("RCurl")
install.packages("RmecabKo")
install.packages("XML")
install.packages("wordcloud2")

library(RCurl)
library(XML)
library(wordcloud2)
library(RmecabKo)

# 한글 형태소 분석 기능 설치
install_mecab("C:/RmecabKo")
#다시 로딩하기
library(RmecabKo)

# 네이버 뉴스 검색 API의 기본 URL을 설정
searchUrl     <- "https://openapi.naver.com/v1/search/news.xml"

# 네이버 오픈 API를 사용하기 위해 발급받은 클라이언트 ID와 시크릿 키를 변수에 저장
Client_ID     <- "7Hg0xrmTP4G76BqsBhR8" # 발급받은 클라이언트 ID
Client_Secret <- "3iaBMpDfy6" # 발급받은 클라이언트 비밀번호(시크릿 키)

# 검색할 키워드를 설정하고 URL 인코딩
# "제임스 웹"이라는 검색어를 UTF-8로 변환한 뒤, URL에서 사용 가능하도록 인코딩
query <- URLencode(iconv("제임스 웹","UTF-8"))

# API 요청에 필요한 URL을 구성
# 검색 URL에 키워드, 출력 개수 등 파라미터를 추가하여 최종 요청 URL 생성
url <- paste(searchUrl, "?query=", query, "&display=20", sep="")

# API 호출
doc <- getURL(url, 
              httpheader = c('Content-Type' = "application/xml",  # 요청 및 응답 형식 지정
              'X-Naver-Client-Id' = Client_ID, # 클라이언트 ID
              'X-Naver-Client-Secret' = Client_Secret)) # 클라이언트 시크릿 키
doc

# xml구조로 변환
xmlFile <- xmlParse(doc) # 구조화된 xml 객체로 변환
xmlFile

#  XML 파일에서 특정 태그(<item>)에 해당하는 데이터를 추출하여 데이터 프레임으로 변환
df <- xmlToDataFrame(getNodeSet(xmlFile, "//item"))
# 생성된 데이터 프레임의 구조를 확인
str(df)



# 뉴스 내용 추출(원하는 내용 추출)
description <- df[,4] #  데이터 프레임 df의 네 번째 열(column)을 추출
description

# 텍스트 전처리
description2 <- gsub("\\d|<b>|</b>", "", description)
description2

# 뉴스 내용으로부터 명사 추출
nouns <- nouns(iconv(description2, "utf-8"))
nouns

# 리스트 내의 단어들을 하나의 벡터로 통합, F는 뉴스 원분이 보이지 않음
nouns.all <- unlist(nouns, use.names = F)
nouns.all

# 글자수가 2이상인 단어만 추출
nouns.all.2 <- nouns.all[nchar(nouns.all) >= 2]
nouns.all.2  

# 단어들의 빈도 
nouns.freq <- table(nouns.all.2)
nouns.freq

# table 객체였던 nouns.freq를 데이터 프레임으로 변환
nouns.df <- data.frame(nouns.freq)

# Freg 값의 역순으로 정렬
nouns.df.sort <- nouns.df[order(-nouns.df$Freq), ] 
nouns.df.sort

#워드 클라우드
# 데이터프레임으로 빈도수가 저장된 데이터, size는 글자크기,
# 단어의 회전 비율( 0이면 모든 단어가 수평으로 표시되고, 1이면 모든 단어가 수직으로 표시)
wordcloud2(nouns.df.sort,
           size = 2,
           rotateRatio=0.5)

wordcloud2(nouns.df.sort,
           size = 0.3,
           shape = 'star')

wordcloud3(nouns.df.sort,
           size =2,
           shape = 'star',
           rotateRatio=1)

