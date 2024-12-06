## R 코딩 플러스 

# 11.2 표본추출과 난수

# (1) 표분 추출
# 복원추출: 추출된 샘플을 다시 모집단에 돌려놓고 다음 샘플을 뽑는 방식
sample(1:10, 5, replace = T)
sample(c("앞면", "뒷면"), 5, replace=T)

# 비복원 추출: 추출된 샘플을 다시 모집단에 돌려놓지 않고 다음 샘플을 뽑는 방식
sample(1:10, 5, replace = F)
sample(c("앞면", "뒷면"), 2, replace=F)

# (2) 난수
# 0~1사이의 균등분포에서 10개 난수 추출출
runif(10, min=0, max=1)

# 11.3 동전 던지기

# install.packages("ggplot2")
library(ggplot2)

# (1) 변수 초기화 부분
iteration <- 1000 # 최대 반복수
prob <- NULL # 누적 비율
count <- 0 # 동전 앞면이 나온 횟수수

# 동전 던지기 반복
for(x in 1:iteration) {
  
  # (2) 동전 던지기
  coin <- sample(c("앞면", "뒷면"), 1, replace=T)
  
  # (3) 동전이 앞면인 경우, 횟수 1 증가
  if (coin == "앞면")
    count = count + 1
  
  # (4) 누적 비율 증가
  prob <- c(prob, round(count / x, 2))
}

# (5) 동전 던지기의 반복에 따른 누적비율의 데이터 프레임
df.coin<- data.frame("반복수"=1:iteration, "누적확률"=prob)

# 데이터 확인
head(df.coin)
tail(df.coin)


# (2) 실험 결과 그래프로 그리는 부분
ggplot(data=df.coin, aes(x=반복수, y=누적확률, group=1)) +
  geom_line(color="blue", size=1) +
  geom_point() +
  geom_hline(yintercept=0.5, color="red") +
  labs(title="동전 던진 횟수에 따른 누적확률의 변화")

# 11.4 원주율 구하기

# install.packages("ggplot2")
# install.packages("ggforce")
library(ggplot2)
library(ggforce)

# 원주율 계산

# (1) 변수 쵸기화
iteration <- 100 # 반복 횟수(샘플 점의 수)
N.circle  <- 0 # 원 내부에 위치한 점의 수
PI <- NULL # 원주율을 저장할 벡터
pts <- NULL #점의 좌표를 저장할 행렬

for(k in 1: iteration) {
  
  # (2) 사각형 내의 점의 위치
  x <- runif(1, min=0, max=1) # x좌표: 0에서 1 사이의 난수
  y <- runif(1, min=0, max=1) # y좌표: 0에서 1 사이의 난수
  
  pts <- rbind(pts, c(x, y)) # 생성된 점의 좌표를 행렬에 추가

  # (3) 거리 계산
  dist <- sqrt(x ^2 + y ^2) # 점 (x, y)에서 원점 (0, 0)까지의 거리
  
  # (4) 거리가 1이하이면, 원 내의 점의 수 증가
  if (dist <= 1)
    N.circle <- N.circle + 1
  
  # (5) 시행 횟수까지의 원주율 계산
  pi.sim <- 4 * N.circle / k # 원주율 추정치
  PI <- c(PI, pi.sim) # 추정치를 벡터에 추가
}

# 시행횟수에 따른 원주율 데이터 세트
PI.df <- data.frame("반복수"=1:iteration, "원주율"=PI) 

# 데이터 확인
head(PI.df)
tail(PI.df)

# (2) 실험결과 그래프
ggplot(data=PI.df, aes(x=반복수, y=원주율, group=1)) +
  geom_line(color="blue", size=1) +
  geom_point() +
  geom_hline(yintercept=pi, color="red") +
  labs(title="시행횟수에 따른 원주율의 변화") 

# 시행횟수에 따른 점의 분포

pts.df <- as.data.frame(pts) #행렬을 데이터 프레임으로 변환
colnames(pts.df) <- c("X", "Y")# 데이터 프레임의 열 이름 설정
head(pts.df)

ggplot() +
  geom_point(data=pts.df, aes(x=X, y=Y)) +
  labs(title="시행횟수에 따른 점의 분포") + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 1)) +
  geom_circle(aes(x0 = 0, y0 = 0, r = 1), col="red", inherit.aes = FALSE)
  # 원점을 중심으로 반경 1인 적생 원을 그린다
  # geom_circle(aes(x0 = 0, y0 = 0, r = 1), col="red")

# 11.5 회귀선 구하기기

# install.packages("ggplot2")
# install.packages("plotly")
library(ggplot2)
library(plotly)

X <- c(10, 22, 28, 40, 48, 60, 67, 82, 92, 98)
Y <- c(12, 18, 27, 33, 38, 40, 43, 52, 55, 62)
df <- data.frame(X=X, Y=Y)

ggplot(df, aes(X, Y)) +
  geom_point() +
  labs(title="산포도", x="X", y="Y") +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 70)) 

# y 절편 범위 설정  
A <- seq(-100, 100, by=0.1)

# 기울기 범위 설정
B <- seq(-3, 3, by=0.1)

# 비용함수값 행렬 초기화화
cost.mtx <- matrix(NA, nrow=length(A), ncol=length(B))

for(i in 1:length(A)) {
  for(j in 1:length(B)) {
    
    err.sum <- 0
    
    for(k in 1:length(X)) {
      y_hat <- B[j]*X[k] + A[i]
      err   <- (y_hat - Y[k])^2
      err.sum <- err.sum + err
    }
    
    cost <- err.sum/length(X)
    cost.mtx[i,j] <- cost
  }
}

# 비용함수값 일부 출력력
cost.mtx[1:5, 1:5]

# 비용함수 값의 범위
range(cost.mtx) 

# 최소 비용용함수값
min(cost.mtx) 

#비용함수값이 최;소가 되는 행과 열의 위치 
idx <- which(cost.mtx == min(cost.mtx), arr.ind = TRUE)
idx

# y 절편편
Amin <- A[idx[1,1]]
Amin

# 기울기기
Bmin <- B[idx[1,2]]
Bmin

# 회귀선 추가가
ggplot(df, aes(X, Y)) +
  geom_point() +
  labs(title="산포도", x="X", y="Y") +
  coord_cartesian(xlim = c(0, 100), ylim = c(0, 70)) +
  geom_abline(intercept=Amin, slope=Bmin, 
              color='red', linetype = "dashed")

# 등고선 그래프를 통한 비용함수값의 분포 파악악
install.packages("plotly")
library(plotly)

# X, Y 평면에 Z값의 등고선 그래프
fig <- plot_ly(x=B, y=A, z = ~cost.mtx, type = "contour")
fig <- fig %>% layout(title = list(text = '기울기와 Y절편에 따른 비용함수값', 
                                   font = list(size=15)), 
                      # 레이아웃의 순서:제목, X축 라벨, Y 축 라벨벨
                      xaxis = list(title = list(text = '기울기')), 
                      yaxis = list(title = list(text = 'Y절편')))

# 색조에 따른 등고선 값을 표시하는 범례례
fig <- fig %>% colorbar(title = "비용함수값")
fig

# 등고선의 범위와 간격, 등고선의 범위(start~end), 간격(size), 라벨 표시
fig <- plot_ly(x=B, y=A, z = ~cost.mtx, 
               type = "contour", 
               contours = list(
                 start = 0,
                 end = 100e3,
                 size = 2e3,
                 showlabels = TRUE))
fig <- fig %>% layout(title = list(text = '기울기와 Y절편에 따른 비용함수값', 
                                   font = list(size=15)), 
                      xaxis = list(title = list(text = '기울기')), 
                      yaxis = list(title = list(text = 'Y절편')))
fig <- fig %>% colorbar(title = "비용함수값")
fig

# 3차원 등고선 그래프프
fig <- plot_ly(x=B, y=A, z=~cost.mtx)

# Z축의 값에 따른 곡면 추가(3차원 그래프프)
fig <- fig %>% add_surface()
fig <- fig %>% layout(
  title = list(text = '기울기와 Y절편에 따른 비용함수값', font = list(size=15)),
  scene = list(
    xaxis = list(title = '기울기'),
    yaxis = list(title = 'Y절편'),
    zaxis = list(title = '비용함수값')
  )
)
fig <- fig %>% colorbar(title = "비용함수값")
fig

library(plotly)

# z 축의 값에 따른 곡면에 등고선 추가가
fig <- plot_ly(x=B, y=A, z=~cost.mtx)
fig <- fig %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      project=list(z=TRUE)
    )
  )
)

fig <- fig %>% layout(
  title = list(text = '기울기와 Y절편에 따른 비용함수값', font = list(size=15)),
  scene = list(
    xaxis = list(title = '기울기'),
    yaxis = list(title = 'Y절편'),
    zaxis = list(title = '비용함수값')
  )
)
fig <- fig %>% colorbar(title = "비용함수값")
fig

# Z축 범위 조정
fig <- plot_ly(x=B, y=A, z=~cost.mtx)
fig <- fig %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      project=list(z=TRUE)
    )
  )
)


# Z축 범위 설정정
fig <- fig %>% layout(
  title = list(text = '기울기와 Y절편에 따른 비용함수값', font = list(size=15)),
  scene = list(
    xaxis = list(title = '기울기'),
    yaxis = list(title = 'Y절편'),
    zaxis = list(title = '비용함수값', range= c(0, 2e3))
  )
)
fig <- fig %>% colorbar(title = "비용함수값")
fig

