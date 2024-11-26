## R 코딩 플러스 - 11월 26일 화

# 12.3

install.packages("igraph")
library(igraph)

# 노드가 6개인 스타형 네트워크, 방향성 없음, 1번 노드를 중앙에 배치
G.star <- make_star(6, mode="undirected", center=1) %>%
    # 앞의 결과에서 노드 이름을 'A'~'F'로 설정
    set_vertex_attr("name", value = c("A", "B", "C", "D", "E", "F"))
# 네트워크 출력(색과 노드 크기 지정)
plot(G.star, vertex.color="orange", vertex.size=50)

# 인터렉티브 그래프 출력
tkplot(G.star, vertex.color="orange", vertex.size=40)

# 그래프 초기화(무방향)
G.ring <- make_ring(6, directed = FALSE, circular = TRUE) %>%
    set_vertex_attr("name", value = c("A", "B", "C", "D", "E", "F"))
tkplot(G.ring, vertex.color="lightgreen", vertex.size=40)

G.Y <- make_graph(edges=NULL, n=0, directed=FALSE) # 그리프 초기화(무방향)
G.Y <- G.Y + vertices("A", "B", "C", "D", "E", "F") # 노드 추가
# 연결 추가(노드 순서대로 2개씩 한쌍으로 연결)
G.Y <- G.Y + edges("A", "B",
                   "A", "C",
                   "A", "D",
                   "D", "E",
                   "E", "F")
tkplot(G.Y, vertex.color="lightblue", vertex.size=40)


# 연결정도 중심성(비정규형형)
degree(G.star, normalized = FALSE)

# 연결정도 중심성(정규형형)
degree(G.star, normalized = TRUE)

# 연결정도 중심성(비정규형형), 연결정도 중심성:비정규형, 연결정도 중심성 비정규형, 이론적인 연결정도 중심화
CD <- centralization.degree(G.star, normalized = FALSE)
CD

# 이론적인 연결정도 중심화화
Tmax <- centralization.degree.tmax(G.ring)
Tmax

CD$centralization / Tmax

# 밀도 g_strar 그래프에 대한 네트워크 밀도도
graph.density(G.star)

graph.density(G.Y)

graph.density(G.ring)

# 최단 경로, 각 노드간 경로에 대한 거리(연결정도, B에서c는두 단계를 거치면 도달)
shortest.paths(G.Y)

# A노드에서 E노로 연결된 거리(단계)
distances(G.Y, v = "A", to="E")

# A노드에서 E노로 연결된 경로
get.shortest.paths(G.Y, "A", "E")$vpath[[1]]

# 네트워크 경로들에 대한 평균거리, 임의의 두 사용자는 평균적으로 2.13 단계를 거치면 연결됨됨
average.path.length(G.Y)


# 12.4, 페이스북 사용자의 네트워크 분석

# install.packages("igraph")
library(igraph)

setwd("c:/rdata")
#df.fb <- read.table("c:/rdata/facebook_combined", header=F)
df.fb <- read.table(file.choose(), header=F) #facebook_combined.txt 파일 선택택
head(df.fb)

tail(df.fb)

# 그래프 형식의 데이터 프레임으로 변환(directed FALSE는 방향성 없음)
G.fb <-graph.data.frame(df.fb, directed=FALSE)


# 그래프 출력 환경 설정(그래프 영역의 마진을 하,좌,상,우 순으로 지정정)
par(mar=c(0,0,0,0))

# 연결망 출력력
plot(G.fb,
     vertex.label = NA,
     vertex.size = 10,
     vertex.color = rgb(0,1,0,0.5))


# 노드(사용자) 이름름
V(G.fb)$name

#1~50번째 노드 추출
v.set <- V(G.fb)$name[1:50]

#서브 네트워크 생성성
G.fb.part <- induced_subgraph(G.fb, v=v.set)

# 노드 라벨의 크기, 노드 크기:연결정도의 1.5배, vertex.frame.color=인터랙티브 네트워크
tkplot(G.fb.part,
       vertex.label.cex = 1.2,
       vertex.size = degree(G.fb.part)*1.5,
       vertex.color = "yellow",
       vertex.frame.color = "gray")

# 노드 이름 '1'의 위치
v2 <- which(V(G.fb)$name == "1")
v2

# v2ㅣ 노드와 연결된 이웃 노드 추출
v.set <- neighbors(G.fb, v=v2)
v.set

# 노드'1'과 그 이웃 노드의 통합
v3 <- c(v2, v.set)

# G.fb에서 V3에 해당하는 서브 네트워크 추출출
G.fb.id <- induced_subgraph(G.fb, v=v3)

# 노드색 노드가 '1'이면 적색, 그 외는 노란색
V(G.fb.id)$color <- ifelse(V(G.fb.id)$name == "1", "red", "yellow")
tkplot(G.fb.id,
       vertex.label.cex = 1.2,
       vertex.size = degree(G.fb.id)*1.5,
       vertex.frame.color = "gray")

# 연결정도 중심성(비정규형)
degree(G.fb, normalized=FALSE)

# 연결정도 중심성(정규형)
degree(G.fb, normalized=TRUE)

# 연결정도 중심성(비정규형)
CD <- centralization.degree(G.fb, normalized = FALSE)

# 연결정도 중심화화
CD$centralization

# 이론적인 최대 연결정도 중심화화
Tmax <- centralization.degree.tmax(G.fb)
Tmax

#연결정도 중심성(정규형형)
CD$centralization / Tmax

#연결이 가장 많은 노드드
v.max <- V(G.fb)$name[degree(G.fb)==max(degree(G.fb))]
v.max

# 노드 이름이'107'인 노드의 연결 정도
degree(G.fb, v.max)

# 연결 정도가 가장 큰 노드의 위치
v.max.idx <- which(V(G.fb)$name == v.max)
v.max.idx

# 연결정도가 가장 틈 노드의 이웃노드
v.set <- neighbors(G.fb, v=v.max.idx)

#연결정도가 가장 큰 노드와 그 이웃 노드 통합
v3    <- c(v.max.idx, v.set)

#G.fb 그래프에서 V3에 해당하는 서브 네트워크 추천
G.fb_2 <- induced_subgraph(G.fb, v=v3)

#노드색, 라벨, 사이즈 지정
V(G.fb_2)$color <- ifelse(V(G.fb_2)$name == v.max, "red", "yellow")
V(G.fb_2)$label <- ifelse(V(G.fb_2)$name == v.max, v.max, NA)
V(G.fb_2)$size  <- ifelse(V(G.fb_2)$name == v.max, 50, 5)
plot(G.fb_2)

# 1~20번째 사용자명 추출
v.set <- V(G.fb)$name[1:20]

# 1~20번째 사용자의 네트워크 추출
G.fb.part <- induced_subgraph(G.fb, v=v.set)

#네트워크 내의 사용자 간 연결 행열, 대부분의 값인 0인 성긴 행렬
G.fb.part.adj <- get.adjacency(G.fb.part)

#행렬로의 변환, 연결되지 않은 노드들은 0으로 설정
G.fb.part.mtx <- as.matrix(G.fb.part.adj) 


# 연결된 부분은 1, 연결되지 않은 부분은 '.'으로 표현
G.fb.part.adj
G.fb.part.mtx

#개체들이 행 또는 열 단위에서 단계적으로 결합되는 트리 형태의 덴드로그램(dendrogram)을 나타나지 않게 함
heatmap(G.fb.part.mtx,
        Rowv=NA, Colv=NA,
        margin = c(3, 3),
        col=c('steelblue', 'gold'),
        main="사용간 간 연결",
        xlab="사용자명", ylab="사용자명")

# 밀도
graph.density(G.fb)

# 1~10번째 로드 간의 경로
shortest.paths(G.fb)[1:10, 1:10]

#노드 '3'인 노드와 '7'노드 간의 거리
distances(G.fb, v = "3", to="7")

#노드 '3'인 노드와 '7'노드 간의 경로
get.shortest.paths(G.fb, "3", "7")$vpath[[1]]

#임의 두 노드간의 평균거리
average.path.length(G.fb)

# 확률밀도 분포
#사용자(노드)별 연결정도 분포
plot(degree(G.fb),
     xlab="사용자 ID", ylab="연결 정도",
     main="사용자별 연결정도",
     type='h')

# 사용자별 연결 정도
x <- degree(G.fb, normalized=F)

# 연결 정도에 대한 요약
summary(x)

# 연결 정도의 히스토그램
hist(x, 
     xlab="연결정도", ylab="빈도",
     main="연결정도 분포",
     breaks=seq(0, max(x), by=1))

# 연결 정도에 대한 확률밀도 분포포
G.fb.dist <- degree.distribution(G.fb)
plot(G.fb.dist, 
     type="h",
     xlab="연결정도", ylab="확률밀도",
     main="연결정도 분포")

