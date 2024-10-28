# R 코딩 플러스

# set Workind directory
setwd("C:\\Users\\marlrero\\Documents\\카카오톡 받은 파일\\06")

# R 4.3.3에서 해야 leaflet이 정상으로 보여짐 (Bug)
# package_version(R.version) 

# 6.2 
install.packages("htmltools")
install.packages("leaflet")
library(leaflet)



m <- leaflet() %>% 
  setView(lng=126.976882, lat=37.574187, zoom = 1) %>%
  addTiles()
m

m <- leaflet() %>%
  addTiles() %>% 
  addMarkers(lng=126.976882, lat=37.574187, 
             label="광화문광장",
             popup="행사 안내")
m 

m <- leaflet() %>%
  setView(lng=126.976882, lat=37.574187, zoom = 14) %>%
  addTiles() %>%  
  addMarkers(lng=126.986882, lat=37.574187, 
             label="광화문광장",
             popup="행사 안내")
m

# 6.3

# install.packages("leaflet")
# install.packages("ggplot2")
library(leaflet)
library(ggplot2)

quakes

leaflet(data = quakes) %>% 
  addTiles() %>% 
  addCircleMarkers(
    ~long, ~lat,
    radius = ~ mag,
    stroke = TRUE, weight = 1, color = "black",
    fillColor = "red", fillOpacity = 0.3)

ggplot(quakes, aes(x=mag)) + 
  geom_histogram()

leaflet(data = quakes) %>% 
  addTiles() %>% 
  addCircleMarkers(
    ~long, ~lat,
    radius = ~ifelse(mag >= 6, 10, 1),
    stroke = TRUE, weight = 1, color = "black",
    fillColor = "red", fillOpacity = 0.3)

leaflet(data = quakes) %>% 
  addTiles() %>% 
  addCircleMarkers(
    ~long, ~lat,
    radius = ~ifelse(mag >= 5.5, 10, 0),
    stroke = TRUE,
    weight = ~ifelse(mag >= 5.5, 1, 0),
    color = "black",
    fillColor = "red",
    fillOpacity = ~ifelse(mag >= 5.5, 0.3, 0))

leaflet(data = quakes) %>% 
  addTiles() %>% 
  addCircleMarkers(
    ~long, ~lat,
    radius = ~ifelse(mag >= 6, 10, ifelse(mag >= 5.5, 5, 0)),
    stroke = TRUE,
    weight = ~ifelse(mag >= 5.5, 1, 0),
    color = ~ifelse(mag >= 5.5, 'black', NA),
    fillColor  = ~ifelse(mag >= 6, 'red', ifelse(mag >= 5.5, 'green', NA)),
    fillOpacity = ~ifelse(mag >= 5.5, 0.3, 0))

# 6.4

# install.packages("ggplot2")

# rgdal이 CRAN에서 제외되었음. Posit repository 추가해서 설치. (R Studio에서 CRAN외에 Posit 추가)
# https://docs.posit.co/ide/user/ide/guide/environments/r/packages.html
# Posit 이름으로 추가 : https://packagemanager.posit.co/cran/2023-10-03
install.packages("rgdal")

# rgdal로 ggplot 호출 시 조금 늦게 뜸. 기다려야 함. (한국 지도 좌표 값이기 때문.)

library(ggplot2)
library(rgdal)

setwd("d:/rdata")

map <- readOGR('Z_NGII_N3A_G0010000.shp')

df_map <- fortify(map)
head(df_map)

ggplot(data = df_map, 
       aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", color = "black")

ggplot(data = df_map, 
       aes(x = long, y = lat, group = group, fill = id)) +
  geom_polygon(alpha = 0.3, color = "black") + 
  theme(legend.position = "none")


# install.packages("ggplot2")
# install.packages("rgdal")
library(ggplot2)
library(rgdal)

map <- readOGR('Z_NGII_N3A_G0010000.shp')

crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
map <- spTransform(map, CRSobj = crs)
df_map <- fortify(map)

ggplot(data = df_map, 
       aes(x = long, y = lat, group = group, fill = id)) +
  geom_polygon(alpha = 0.3, color = "black") + 
  theme(legend.position = "none") +
  labs(x="경도", y="위도")

# 6.5

# install.packages("ggplot2")
install.packages("openxlsx")
# install.packages("rgdal")
library(ggplot2)
library(openxlsx)
library(rgdal)

df <- read.xlsx(file.choose(), sheet = 1, startRow = 4,  colNames = FALSE) # 국내지진목록.xlsx 선택 
head(df)

idx <- grep("^북한", df$X8)
df[idx, 'X8']
df <- df[-idx, ]

df[,6] <- gsub("N", "", df[,6])
df[,7] <- gsub("E", "", df[,7])

df[,6] <- as.numeric(df[,6])
df[,7] <- as.numeric(df[,7])

map <- readOGR('Z_NGII_N3A_G0010000.shp')
crs <- CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
map <- spTransform(map, CRSobj = crs)

df_map <- fortify(map)

ggplot() +
  geom_polygon(data = df_map, 
               aes(x = long, y = lat, group = group), 
               fill = "white", alpha=0.5, 
               color="black") +
  geom_point(data=df, 
             aes(x=X7, y=X6, size=X3),
             shape=21, color='black', 
             fill='red', alpha=0.3) +
  theme(legend.position = "none") + 
  labs(title="지진분포", x="경도", y="위도") 
