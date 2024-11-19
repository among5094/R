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

install_mecab("C:/RmecabKo/mecab")
library(RmecabKo)

searchUrl     <- "https://openapi.naver.com/v1/search/news.xml"
Client_ID     <- "7Hg0xrmTP4G76BqsBhR8"
Client_Secret <- "3iaBMpDfy6"

query <- URLencode(iconv("제임스 웹","UTF-8"))

url <- paste(searchUrl, "?query=", query, "&display=20", sep="")

doc <- getURL(url, 
              httpheader = c('Content-Type' = "application/xml",
              'X-Naver-Client-Id' = Client_ID,
              'X-Naver-Client-Secret' = Client_Secret))
doc

xmlFile <- xmlParse(doc)
xmlFile

# XML 파일의 <item1> 태그 내의 데이터 구조를 데이터 프레임으로 변환
df <- xmlToDataFrame(getNodeSet(xmlFile, "//item"))
str(df)

# 뉴스 내용 추출
description <- df[,4]
description

description2 <- gsub("\\d|<b>|</b>", "", description)
description2

# 뉴스 내용으로부터 명사 추출
nouns <- nouns(iconv(description2, "utf-8"))
nouns

# 리스트 내의 단어들을 하나의 벡터로 통합, F는 뉴스 원분이 보이지 않음음
nouns.all <- unlist(nouns, use.names = F)
nouns.all

# 글자수가 2이상인 단어만 추출
nouns.all.2 <- nouns.all[nchar(nouns.all) >= 2]
nouns.all.2  

# 단어들의 빈도 
nouns.freq <- table(nouns.all.2)
nouns.freq

# 데이터 프레임으로 변환환
nouns.df <- data.frame(nouns.freq)

# Freg 값의 역순으로 정렬렬
nouns.df.sort <- nouns.df[order(-nouns.df$Freq), ] 
nouns.df.sort

# size=1은 가장큰 글자의 크기기
wordcloud2(nouns.df.sort,
           size = 1,
           rotateRatio=0.5)

wordcloud2(nouns.df.sort,
           size = 0.3,
           shape = 'star')
