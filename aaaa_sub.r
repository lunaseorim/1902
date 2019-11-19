library(plyr)
library(dplyr)
library(survival)
library(survminer)

dat<-lung %>%
  na.omit() %>%
  data.frame()

pcox_fine <- coxph(Surv(time,status==2)~.-inst,data = dat)
summary(pcox_fine)
pcox_step <-step(pcox_fine,direction = "forward",trace = 0)
pcox_step
ggforest(pcox_step,data = dat)

cox.zph(pcox_step)

ggsurvplot(survfit(Surv(time,status==2)~.,data=dat),
           palette = "#2E9FDF",
           risk.table = "nrisk_cumevents",
           xlab = "Time(days)",
           cumevents = TRUE,
           linetype = 1,
           tables.height = 0.2,
           surv.median.line = "hv",
           legend = "none")

ggsurvplot(survfit(pcox_step),data=dat, palette = "#2E9FDF",
           ggtheme = theme_minimal(),
           xlab = "Time(days)")