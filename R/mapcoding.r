library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(rgeos)
library(maptools)
library(raster)
library(readxl)

A1 <- readxl::read_xlsx("data/KIKmix.20191021.xlsx")
B1 <- read.csv("data/LOCAL_PEOPLE_DONG_201909.csv")
C1 <- read.csv("data/법정동코드 조회자료.csv")
D1 <- readxl::read_xlsx("data/행정동코드_매핑정보_2018.xlsx")

B2 <- ddply(B1,~시간대구분~행정동코드,summarize,mean = mean(총생활인구수))

B3 <- B2[B2$시간대구분==7|B2$시간대구분==8|
           B2$시간대구분==9|B2$시간대구분==10|
           B2$시간대구분==17|B2$시간대구분==18|
           B2$시간대구분==19|B2$시간대구분==20,]
B4 <- ddply(B3,~행정동코드,summarize,mean=mean(mean))


A1$행정동코드 <- substr(A1$행정동코드,1,8)
A1$법정동코드 <- substr(A1$법정동코드,1,8)

n2 <- data.frame()
n1 <- NULL

for (i in 1:424) {
  n1 <- D1[which(B4$행정동코드[i] == D1$행자부행정동코드),5] %>%
    unlist() %>% unname()
  n2 <- rbind(n2,data.frame(name = A1[which(A1$읍면동명==n1),5],
                            mean = rep(B4[i,2],length(which(A1$읍면동명==n1)))))
  cat(i,"/424","\n",sep = "")
}

D2 <- merge(x=C1[1:467,],y=n2,by="법정동코드",all.x = T)

D3 <- ddply(D2,~id,summarize,sum = sum(mean))

seoul <- shapefile("data/TL_SCCO_EMD.shp")
seoul <- spTransform(seoul,CRS("+proj=longlat"))
seoul_map <- fortify(seoul)
merge_result <- merge(D3,seoul_map,by="id")

D3[468:5047,1]<-467:5046


ggplot() +
  geom_polygon(data = merge_result,
               aes(x=long,y=lat,group=group,fill = sum),colour = "white",size=0.8,
               show.legend = F) +
  coord_cartesian(xlim=c(126.75,127.22),
                  ylim=c(37.42,37.705)) +
  scale_fill_gradient(low = '#D9E5FF',high='#5587ED',na.value = "white") +
  theme_void() +
  ggtitle("서울시 출퇴근시간대 생활인구") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 15, color = "#4375DB"))
