
##모델링

#library
library(plyr)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)
##1.전처리과정 

A5$재발여부 <- ifelse(A5$재발여부==T,1,0)

#A5를 공식지정


#1-1더미변수
A5$ADC <- ifelse(A5$H1 == "ADC",1,0)
A5$SQC <- ifelse(A5$H1 == "SQC",1,0)

#1-2림프노드 침범 여부
regmatches(A5$pTNM,regexpr("N1",A5$pTNM),invert = F)
table(regexpr("N1",A5$pTNM))

cLN <- ifelse(regexpr("N1",A5$pTNM)>0,1,0)
A5$cLN <- cLN

A5$times <- ifelse(A5$times<0,-1*A5$times,A5$times)

#2.모형만들기 [9]가 adj. r_sqared
p <- lm(times~`ki 67a`+성별+Age+width+`P 53b`,A5[-c(651,264),])
summary(p)
plot(p)
pm <- gvlma(p)
summary(pm)

attach(Z1)
p1 <- lm(times~`ki 67a`+성별+Age+width,Z1)
summary(p1)[9]

p2 <- lm(times~.-Age,Z2)
summary(p2)

#3.Cox
B1 <- A5[,c(1,2,7,9,11,12,15:18,23)] %>%
  na.omit()

B2 <- B1[!B1$time<0,]

names(B2)[8] <- "ki67a발현율"

B2$`ki67a 발현률` <- ifelse(B2$`ki 67a`>=20,"20이상","20미만")
B2$size<-ifelse(B2$width>=3.7,"3.7cm이상","3.7cm미만")
  B2$ki67a발현율
pcox_fine <- coxph(Surv(time,재발여부==1)~ki67a발현율+조직학적분류+width+`림프절 전이 유무`+Age,data=B2)
pcox_step <-step(pcox_fine,direction = "both",trace = 0)
pcox_step
ggforest(pcox_step,data = B2)

cox.zph(pcox_step)

ggsurvplot(survfit(Surv(time,재발여부==1)~1,data=B2[,c(4,11)]),
           palette = "#2E9FDF",
           risk.table = "nrisk_cumevents",
           xlab = "Time(days)",
           cumevents = TRUE,
           linetype = 1,
           tables.height = 0.2,
           surv.median.line = "hv",
           legend = "none")

ggsurvplot(survfit(pcox_step),data=B2, palette = "#2E9FDF",
           ggtheme = theme_minimal(),
           xlab = "Time(days)")



fine <- coxph(Surv(times,재발여부==1)~`ki67a 발현률`+width+조직학적분류+`림프절 전이 유무`,Z1)
step(fine,direction = "both",trace = 0)




