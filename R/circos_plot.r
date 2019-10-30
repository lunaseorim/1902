
library(miges)
library(circlize)
library(plyr)

A1 <- read.csv("data/PJT001_sk_emd_od1.csv")

A3$dst<-as.factor(A3$dst)

C1 <- matrix(NA,8,8)
rownames(C1)<-c("동탄1동","동탄2동","동탄4동","동탄6동","반월동","향남읍","팔탄면","봉담읍")

grid.col = c(동탄1동 = "#FFD1A3", 동탄2동 = "#EB99D2", 동탄4동 = "#788CFF",
             동탄6동 = "#98EBC5", 반월동 = "#FFFFA3", 향남읍 = "#B782FF", 봉담읍 = "#FF564D", 팔탄면 = "#EBC671")

jpeg("test.jpg",width = 8.1, height = 4.9,units = "in",res = 1200)
par(mfrow = c(1, 3))
chordDiagram(am05, link.visible = am05$pop > 2000 , grid.col = grid.col,link.sort = TRUE, link.decreasing = TRUE)
text(0.8,1.2, cex = 2.3, labels = "6-11시 이동인구", pos = 2.3)
chordDiagram(pm12, link.visible = pm12$pop > 2000 , grid.col = grid.col,link.sort = TRUE, link.decreasing = TRUE)
text(0.8,1.2, cex = 2.3, labels = "12-17시 이동인구", pos = 2.3)
chordDiagram(pm18, link.visible = pm18$pop > 2000 , grid.col = grid.col,link.sort = TRUE, link.decreasing = TRUE)
text(0.8,1.2, cex = 2.3, labels = "18-23시 이동인구", pos = 2.3)
dev.off()

pm12 <- A1[A1$tm_trm=="12_17",]
pm12 <- pm12[pm12$dst=="동탄1동"|pm12$dst=="동탄2동"|pm12$dst=="동탄4동"|pm12$dst=="동탄6동"|
                 pm12$dst=="반월동"|pm12$dst=="향남읍"|pm12$dst=="팔탄면"|pm12$dst=="봉담읍",]
pm12 <- ddply(pm12,~org~dst,summarize,pop=mean(pop))

