regexpr("\\d",A2$Size)
m <- regmatches(A2$Size,regexpr("\\d\\.\\d|\\d",A2$Size),invert = F) %>%
  as.numeric()
R1 <- as.numeric(R1) %>%
  as.data.frame()
R1 <- regexpr("\\d\\.\\d|\\d",A2$Size) %>%
  as.data.frame()
R1$R2 <- ifelse(R1$.=="NA"|R1$.==-1,NA,0)

plot(A5$`ki 67a`,A5$`R1$V4`)

A5$재발여부
cor.test(A5$.,(A5$`ki 67a`^-0.5)) #-0.3017

sum(!is.na(A5$.)&!is.na(A5$`ki 67a`))

AA01 <- A5[!is.na(A5$.)&!is.na(A5$`ki 67a`),c(49,31)] %>%
  `colnames<-`(c("times","ki67a"))

cor.test(AA01$times,AA01$ki67a^-0.5)


AA01$high <- ifelse(AA01$ki67a>25,1,0)


ggplot(AA01,aes(x=AA01$time,y=ki67a^-0.5)) +
  geom_point(colour="skyblue")+
  ylim(0,1)+
  xlim(0,1030)+
  geom_abline(intercept = 1.936e-01,slope = 1.100e-04,color = "orange",size=1.3) 



ggplot(A4,aes(x=.)) +
  stat_ecdf(aes(group = high_ki67a,colour = high_ki67a),size=1.2) +
  ylab("누적 재발률") +
  xlab("수술후 경과시간(일)") +
  coord_cartesian(xlim = c(0,1030.75),
                  ylim = c(0,1))



t_test<-ddply(AA01,~high,summarise,mean=mean(times))

t.test(AA01$times~AA01$high)

bartlett.test(AA01$times~AA01$high)

shapiro.test(AA01$times)

wilcox.test(AA01[AA01$high==0,1],
            AA01[AA01$high==1,1],paired=T)



