library(plyr)
library(dplyr)
library(ggplot2)

readxl::read_xlsx("임상데이터_분석용.xlsx")

A1 <- origin_data

#재발일자 전처리(이상치 제거)
B01 <- c(A1$재발일자1 - A1$Opdate) %>%
  as.data.frame()
A1 <- cbind(A1,B01)

A2 <- A1[-!A1$.<1030.75&A1$.>0,]

## 특정 유전자 활성도와 수술후 재발까지 걸린시간에 대하여 with t-test

D1 <- ddply(A1,~`Cell type`,summarise,
            mean_p53a = mean(`P 53a`,na.rm = T),mean_p53b = mean(`P 53b`,na.rm = T),
            mean_ki67a = mean(`ki 67a`,na.rm = T),mean_ki67b = mean(`ki 67b`,na.rm = T))

D2_mlist <- lapply(A2[,29:32], summary)
D2_colname <- colnames(A2[,29:32])

D2_anova <- aov(~`P 53a`+`P 53b`+`ki 67a`+`ki 67b`,A2)



t.test(D2[,1])

summary(aov(재발여부~`P 53b`+`ki 67a`,A2))
summary(aov(.~`P 53a`+`P 53b`+`ki 67a`+`ki 67b`,A2))
summary(aov(.~H1,A2))
table(A2$H1)


summary(lm(.~`P 53a`+`P 53b`+`ki 67a`+`ki 67b`,A3))

levene
summary(aov(.~H1,A3))

A3 <- A2 #A3사용

ki67a <- ifelse(A2$`ki 67a`>25,"25초과","25이하")
A4 <- cbind(A3,ki67a)
A4 <- A4[!is.na(A4$ki67a),]
A4[is.na(A4$.),49]<-100000


ggplot(A4,aes(x=.)) +
  stat_ecdf(aes(group = ki67a,colour = ki67a),size=1.2) +
  ylab("누적 재발률") +
  xlab("수술후 경과시간(일)") +
  coord_cartesian(xlim = c(0,1030.75),
                  ylim = c(0,0.40))

summary(aov(.~high_ki67a,A4))

t.test(.~high_ki67a,A3)




##3. 폐암의 조직학적 세포형태와 수술후 재발까지 걸린시간에 대하여
##3-1 문자열 필터링
D2 <- ddply(A2,~H1,summarise,mean = mean())

G1 <- A1$`Cell type`

H1 <- data.frame(0) %>%
  `colnames<-`("cell")
H1[grep("ADC",G1$G1),1] <- "ADC"
H1[grep("SQC",G1$G1),1] <- "SQC"
#H1[grep("LAC",G1$G1),1] <- "LAC"

H1 <- as.factor(H1$cell)

A2 <- cbind(A1,H1)

G2 <- ddply(A2,~H1,summarise,mean = mean(.,na.rm=T))
G2


