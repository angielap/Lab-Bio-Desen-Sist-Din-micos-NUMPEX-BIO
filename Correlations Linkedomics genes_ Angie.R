
install.packages("ggpubr")
install.packages("ggplot2")

library("ggplot2")
library("ggpubr")


#***********Abrindo dados*******************
alldata <- read.delim(file.choose("~/TCC"))

#alldata$STUDY_ID <- NULL
#rownames(alldata)<-alldata$SAMPLE_ID
#alldata$SAMPLE_ID <- NULL

dim(alldata)  #conta o numero de linhas e colunas


#***********Log dos dados********************

log2(alldata)
alldata_log <-log2(alldata)

#**********Graficos dos dados*****************
reg_alldata_TWIST1_PRRX1= lm(alldata$TWIST1 ~ alldata$PRRX1)
reg_alldata_TWIST1_PRRX1
summary(reg_alldata_TWIST1_PRRX1)

ggscatter(alldata, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title = "Correlação TWIST1 X PRRX1: Pearson",
          xlab = "TWIST1", ylab = "PRRX1")

ggscatter(alldata, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", title = "Correlação TWIST1 X PRRX1: Spearman",
          xlab = "TWIST1", ylab = "PRRX1")


reg_alldata_PRRX1_TNC= lm(alldata$TNC ~ alldata$PRRX1)
reg_alldata_PRRX1_TNC
summary(reg_alldata_PRRX1_TNC)

ggscatter(alldata, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title = "Correlação PRRX1 X TNC: Pearson",
          xlab = "PRRX1", ylab = "TNC")

ggscatter(alldata, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", title = "Correlação PRRX1 X TNC: Spearman",
          xlab = "PRRX1", ylab = "TNC")


reg_alldata_TWIST1_TNC= lm(alldata$TWIST1 ~ alldata$TNC)
reg_alldata_TWIST1_TNC
summary(reg_alldata_TWIST1_TNC)

ggscatter(alldata, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",  title = "Correlação TWIST1 X TNC: Pearson",
          xlab = "TWIST1", ylab = "TNC")


ggscatter(alldata, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",  title = "Correlação TWIST1 X TNC: Spearman",
          xlab = "TWIST1", ylab = "TNC")


#**********grafico de dispersÃ£o******************
ggplot(alldata,aes(x=TWIST1, y=PRRX1))+
  geom_point(col="blue")+
  labs(title='mRNA Expression')+
  theme_bw()

ggplot(alldata,aes(x=PRRX1, y=TNC))+
  geom_point(col="blue")+
  labs(title='mRNA Expression')+
  theme_bw()

ggplot(alldata,aes(x=TWIST1, y=TNC))+
  geom_point(col="blue")+
  labs(title='mRNA Expression')+
  theme_bw()

#**********teste estatistico****************
#normalidade(Shapiro-wilk)
shapiro.test(alldata$TWIST1)
shapiro.test(alldata$PRRX1)
shapiro.test(alldata$TNC)

res1p <- cor.test(alldata_log$TWIST1, alldata_log$PRRX1,method = "pearson")
res1p

res1s <- cor.test(alldata_log$TWIST1, alldata_log$PRRX1,method = "spearman")
res1s

res2p <- cor.test(alldata_log$PRRX1, alldata_log$TNC,method = "pearson")
res2p

res2s <- cor.test(alldata_log$PRRX1, alldata_log$TNC,method = "spearman")
res2s

res3p <- cor.test(alldata_log$TWIST1, alldata_log$TNC,method = "pearson")
res3p

res3s <- cor.test(alldata_log$TWIST1, alldata_log$TNC,method = "spearman")
res3s

