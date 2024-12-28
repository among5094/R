# 11장 5강 회귀선 구하기

# (1) 데이터 분포 파악

# 데이터 작성 및 분포 파악
install.packages("ggplot2")
install.packages("plotly") #3차원 그래프 
library(ggplot2)
library(plotly)

# 데이터 작성
X<-c(10,22,28,40,48,60,67,82,92,98)
Y<-c(12,18,27,33,38,40,43,52,55,62)
df <- data.frame(X=X, Y=Y)

# 산포도 출력
ggplot(df, aes(X,Y))+
  geom_point()+
  labs(title="산포도", x="X", y="Y")+
  coord_cartesian(xlim=c(0,100), ylim=c(0,70))# 그래프의 X축과 Y축의 범위를 설정

# 비용함수값 계산

# (1) 변수 초기화
# y절편 범위 설정 
A<-seq(-100,100,by=0.1)

# 기울기 범위 설정
B<-seq(-3,3,by=0.1)

# 비용함수값 행렬 초기화
cost.mtx<-matrix(NA, nrow=length(A), ncol=length(B))

# (2) 최소 비용함수값과 회귀선

# A,B 구간에서의 각 비용함수값 계산
for(i in 1:length(A)){
  for(j in 1:length(B)){
  
    # (2) 잔차 제곱합
    err.sum<-0
    for(k in 1:length(x)){ # 모든 데이터 포인트에 대해 반복
      y_hat <- B[j]*X[k]+A[i] # 예측값 계산 (y = B[j] * X[k] + A[i])
      err <- (y_hat-Y[k])^2 # 실제값과 예측값의 차이를 제곱
    err.sum<-err.sum+err # 잔차 제곱합에 추가
    }
  
  # (3)비용함수값
  cost <- mean(err.sum)
  cost.mtx[i,j]<-cost
  }
}

# 비용함수값 일부 출력(1~5행, 1~5열)
cost.mtx[1:5, 1:5]

# 최소 비용함수값의 파라미터 찾기
# 비용함수값의 범위
range(cost.mtx) # 비용 함수값의 최소값과 최대값 출력

# 최소 비용함수값
min(cost.mtx) # 비용 함수값 중 가장 작은 값(최소값)을 출력

# 비용함수값이 최소가 되는 행과 열의 위치
idx <- which(cost.mtx == min(cost.mtx), arr.ind=TRUE)
idx # 최소 비용 함수값의 위치(행, 열)를 출력

# y절편
Amin <- A[idx[1,1]] # 최소 비용 함수값에 대응하는 y절편(A) 값
Amin # y절편 출력

# 기울기
Bmin <- B[idx[1,2]] # 최소 비용 함수값에 해당하는 기울기(B)의 값
Bmin # 기울기 출력

# 회귀선 추가
ggplot(df,aes(x,y))+
  geom_point()+
  labs(title="산포도", x="X", y="Y")+
  coord_cartesian(xlim=c(0,100), ylim=c(0,70))+
  geom_abline(intercept=Amin, slope=Bmin,
              color='red', linetype="dashed") # 회귀선 색상은 빨간색, 선 종류는 점선


# (3) 등고선 그래프를 통한 비용함수값의 분포 파악
# 등고선 그래프 생성
fig <- plot_ly(x=B, y=A, z=~cost.mtx, type="contour") #X,Y 평면에 Z값이 등고선 그래프

# 그래프 레이아웃 설정
fig <- fig %>% layout(title=list(text='기울기와 Y절편에 따른 비용함수값',
                                 font=list(size=15)),
                      xaxis=list(title=list(text='기울기')),
                      yaxis=list(title=list(text='Y절편편')))

# 색상 막대 추가
fig <- fig %>% colorbar(title="비용함수값")

# 그래프 출력
fig










