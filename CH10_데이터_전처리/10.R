## R 코딩 플러스 

# 10.2

W <- c("가", "나", "", "라", "마", "바", "사", "아", "아", "차") 
X <- c(1, 2, 3, NA, 5, 6, 7, 8, 9, 0)
Y <- c(10, 20, NULL, 40, 50, NaN, 70, 80, 90, 100, 110)
Z <- c(5, 10/0, Inf, -20/0, -Inf, 0/0, NaN, 40, 45, 50)
df <- data.frame(W, X, Y, Z)
df

# 데이터 구조 보기 
str(df)

# 요약보기
summary(df)

# W열에 대한 빈문자열 확인인
nzchar(df$W)

# 가 열별 빈문자열 확인인(1은 행, 2는 열)
empty <- apply(df, 2, nzchar)
empty

# 각 열별 합계(빈문자열의 수수)
colSums(!empty)

# 각 열별 합계(빈문자열의 수수)
rowSums(!empty)

# 결측치 여부
is.na(df)

# 결측치의 총수, 열별 결측치의 수, 행별 결측치의 수수
sum(is.na(df))
colSums(is.na(df))
rowSums(is.na(df))

# Inf(무한대) 여부
is.infinite(df$Z)

# Inf의 수
sum(is.infinite(df$Z))

# na(결측치)가 있는 행 삭제
df2 <- na.omit(df)
df2

# --- 강의자료에 있는 부분 추가 ---

# 조건에 맞는 행 추출
install.packages("dplyr")
library(dplyr)

# 무한값을 제거해서 만든 것것
df2 <- filter(df, is.finite(Z)) # df에서 Z열이 유한한 값을 갖는 행 추출
df2

# 조건에 맞는 행과 열 추출
df2 <- subset(df, nzchar(W)&is.finite(X)&is.finite(Y)&is.finite(Z),
              select=c(W,Z))
df2

# 조건에 해당하는 데이터의 임의 수정 (값 대체하기)
df3 <- df
df3&X[df3&X==0] <- 10
df3&X[is.na(df3&X)] <- 4
df3&X[is.na(df3&Y)] <- 60
df3&X[is.na(df3&Z)] <- 30
df3&X[df3&Z >= Inf] <- 10

# 조건에 해당하는 데이터의 수정


iris
str(iris)
summary(iris)

install.packages("GGally")
library(GGally)

# 두 변수간 분포, 비교할 변수(1열~4열), wrap(상관계수의 글자크기)
ggpairs(data = iris, 
        columns = 1:4,
        upper = list(continuous = wrap("cor", size = 2.5)))

# 두 변수간 종별 분포포
ggpairs(data = iris,                 
        columns = 1:4,        
        aes(color = Species, alpha = 0.5),
        upper = list(continuous = wrap("cor", size = 2.5)))

# 10.3

# 결측치가 있는 행 삭제, 각 열에서 NA와  NaN이 있는 4~7행이 삭제됨됨
df2 <- na.omit(df)
df2 

install.packages("dplyr")
library(dplyr)   

# df에서 Z열이 유한한 값을 갖는 행 추출출
df2 <- filter(df, is.finite(Z))
df2

# df에서 W열이 빈 문자열이 아니며, X,Y,Z열이 모두 유한한 값인 행 추출출
df2 <- filter(df, nzchar(W)    & is.finite(X) &
                  is.finite(Y) & is.finite(Z))
df2

df2 <- subset(df, nzchar(W)    & is.finite(X) &
                  is.finite(Y) & is.finite(Z),
              select = c(W, X))
df2

df3 <- df
df3$X[df3$X == 0]    <- 10

# df에서 X열의 값이 NA인 경우 4로 수정정
df3$X[is.na(df3$X)]  <- 4
df3$Y[is.nan(df3$Y)] <- 60
df3$Z[is.nan(df3$Z)] <- 30
df3$Z[df3$Z >= Inf]  <- 10
df3$Z[df3$Z <= -Inf] <- 20
df3$W[df3$W == ""]   <- "Unknown"
df3

df3 <- df
df3$X[df3$X == 0]    <- 10
df3$X[is.na(df3$X)]  <- mean(df3$X[is.finite(df3$X)])
df3$Y[is.nan(df3$Y)] <- median(df3$Y[is.finite(df3$Y)])
df3$Z[is.nan(df3$Z)] <- mean(df3$Z[is.finite(df3$Z)])
df3$Z[df3$Z >= Inf]  <- max(df3$Z[is.finite(df3$Z)])
df3$Z[df3$Z <= -Inf] <- min(df3$Z[is.finite(df3$Z)])
df3$W[df3$W == ""]   <- "다"
df3

# 10.4

install.packages("dplyr")  
library("dplyr") 

df1 <- data.frame(ID=1:3, 성명=c('장발장', '팡틴', '자베르'))
df1
df2 <- data.frame(ID=2:4, 경력=c(7, 5, 10))
df2

# 내부 조인
inner_join(df1, df2, by = "ID") 

# 왼쪽 외부 조인 <- ?
left_join(df1,df2,   by = "ID") 

# 오른쪽 외부 조인 
right_join(df1,df2,  by = "ID") 

# 전체 조인
full_join(df1,df2,   by = "ID") 

