
##HW01

#변수 2개 지정하여 기술통계량과 간단한 그림

#PACKAGES
library(plyr)
library(dplyr)
library(ggplot2)


#PRE DATA
A01 <- readxl::read_xlsx("data/임상데이터_분석용.xlsx")

#01-1 수술후 재발까지 걸린시간
B01 <- A01$재발일자1 - A01$Opdate

B05 <- B01[B01>0] %>% 
  na.omit() %>%
  as.integer()

#01-1 기술통계
sum(A01$재발여부,na.rm=T)/length(A01$재발여부) #재발율

mean(B05)

E03 <- B01 %>%
  na.omit()
#이상치제거 (1.5IQRANGE -381.25 ~ 1030.75)
#간단한그림
quantile(E03,0.25) - IQR(E03) * 1.5 #-381.25
quantile(E03,0.75) + IQR(E03) * 1.5 #1030.75
length(E03[E03>1030.75]) #이상치 31개

boxplot(B03[B03<1030.75])

B06 <- as.data.frame(B03[B03>0&B03<1030.75]) %>%
  `colnames<-`("수술후재발")

ggplot() +
  geom_density(aes(x=수술후재발),B06)

kableExtra::kable(summary(B04))


#01-2 pStage
C01 <- A01$cStage
C01 <- as.factor(C01)

A02 <- cbind(A01$pStage,B01) %>%
  as.data.frame()
A02$V1 <- toupper(A02$V1) %>%
  as.factor()
A02$B01 <- as.integer(A02$B01)

D01 <- ddply(A02,~V1,summarise,mean = mean(B01,na.rm=T)) %>%
  na.omit() %>%
  `[`(-1:-2,)

ggplot()

