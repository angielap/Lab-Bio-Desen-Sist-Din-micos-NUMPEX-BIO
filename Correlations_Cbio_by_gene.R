
# Lab-Bio-Desen-Sist-Din-micos-NUMPEX-BIO
# Para calculo de correlação de Pearson e Spearman para dados de expressão gênica provenientes do Portal CBIO por gene. 

#versão 2.0
#Data: Dezembro 2021


library("ggplot2")
library("ggpubr")
library(grid)
library(corrplot)
library("ggcorrplot")


#dados usados com o Cbio

#Chamada de dados
#***********Abrindo dados*******************
data_NFKB1 <- read.delim(file.choose("~/TCC/Resultados BRUNO/BASAL"), header=T)
data_PRRX1 <- read.delim(file.choose("~/TCC/Resultados BRUNO/BASAL"), header=T)
data_RELA <- read.delim(file.choose("~/TCC/Resultados BRUNO/BASAL"), header=T)
data_SNAI2 <- read.delim(file.choose("~/TCC/Resultados BRUNO/BASAL"), header=T)
data_TWIST1 <- read.delim(file.choose("~/TCC/Resultados BRUNO/BASAL"), header=T)
data_ZEB2 <-read.delim(file.choose("~/TCC/Resultados BRUNO/BASAL"), header=T)


#***********NFKB1*******************
data_NFKB1$Study.ID <- NULL
data_NFKB1$Patient.ID <- NULL
data_NFKB1$Sample.ID <- NULL

colnames(data_NFKB1)[1] <- "NFKB1"

log_data_NFKB1 <-cbind(data_NFKB1, log2(data_NFKB1$NFKB1))
log_data_NFKB1$NFKB1 <- NULL
colnames(log_data_NFKB1)[1] <- "NFKB1"


dim(data_NFKB1)#conta o numero de linhas e colunas
str(data_NFKB1)

#************PRRX1******************
data_PRRX1$Study.ID <- NULL
data_PRRX1$Patient.ID <- NULL
data_PRRX1$Sample.ID <- NULL

colnames(data_PRRX1)[1] <- "PRRX1"

log_data_PRRX1 <-cbind(data_PRRX1, log2(data_PRRX1$PRRX1))
log_data_PRRX1$PRRX1 <- NULL
colnames(log_data_PRRX1)[1] <- "PRRX1"


dim(log_data_PRRX1)#conta o numero de linhas e colunas
str(log_data_PRRX1)

#************RELA******************
data_RELA$Study.ID <- NULL
data_RELA$Patient.ID <- NULL
data_RELA$Sample.ID <- NULL

colnames(data_RELA)[1] <- "RELA"

log_data_RELA <-cbind(data_RELA, log2(data_RELA$RELA))
log_data_RELA$RELA <- NULL
colnames(log_data_RELA)[1] <- "RELA"


dim(log_data_RELA)#conta o numero de linhas e colunas
str(log_data_RELA)

#************SNAI2******************
data_SNAI2$Study.ID <- NULL
data_SNAI2$Patient.ID <- NULL
data_SNAI2$Sample.ID <- NULL

colnames(data_SNAI2)[1] <- "SNAI2"

log_data_SNAI2 <-cbind(data_SNAI2, log2(data_SNAI2$SNAI2))
log_data_SNAI2$SNAI2 <- NULL
colnames(log_data_SNAI2)[1] <- "SNAI2"


dim(log_data_SNAI2)#conta o numero de linhas e colunas
str(log_data_SNAI2)

#************TWIST1******************
data_TWIST1$Study.ID <- NULL
data_TWIST1$Patient.ID <- NULL
data_TWIST1$Sample.ID <- NULL

colnames(data_TWIST1)[1] <- "TWIST1"

log_data_TWIST1 <-cbind(data_TWIST1, log2(data_TWIST1$TWIST1))
log_data_TWIST1$TWIST1 <- NULL
colnames(log_data_TWIST1)[1] <- "TWIST1"


dim(log_data_TWIST1)#conta o numero de linhas e colunas
str(log_data_TWIST1)

#************ZEB2******************

data_ZEB2$Study.ID <- NULL
data_ZEB2$Patient.ID <- NULL
data_ZEB2$Sample.ID <- NULL

colnames(data_ZEB2)[1] <- "ZEB2"

log_data_ZEB2 <-cbind(data_ZEB2, log2(data_ZEB2$ZEB2))
log_data_ZEB2$ZEB2 <- NULL
colnames(log_data_ZEB2)[1] <- "ZEB2"


dim(log_data_ZEB2)#conta o numero de linhas e colunas
str(log_data_ZEB2)



#**********Graficos dos dados*****************

#*** PRRX1 X NFKB1

reg_PRRX1_NFKB1= lm(log_data_PRRX1$PRRX1 ~ log_data_NFKB1$NFKB1)
reg_PRRX1_NFKB1

summary(reg_PRRX1_NFKB1)
cor(log_data_PRRX1$PRRX1,log_data_NFKB1$NFKB1 )

graph_PRRX1_NFBK1<- data.frame(log_data_PRRX1,log_data_NFKB1)

ggscatter(graph_PRRX1_NFBK1, x = "PRRX1", y = "NFKB1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo BASAL - PRRX1 X NFKB1: Correlação Pearson",
          xlab = "PRRX1", ylab = "NFKB1")

ggscatter(graph_PRRX1_NFBK1, x = "PRRX1", y = "NFKB1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo BASAL - PRRX1 X NFKB1: Correlação Sperman",
          xlab = "PRRX1", ylab = "NFKB1")


#*** PRRX1 X RELA

reg_PRRX1_RELA= lm(log_data_PRRX1$PRRX1 ~ log_data_RELA$RELA)
reg_PRRX1_RELA

summary(reg_PRRX1_RELA)
cor(log_data_PRRX1$PRRX1,log_data_RELA$RELA)

graph_PRRX1_RELA<- data.frame(log_data_PRRX1,log_data_RELA)

ggscatter(graph_PRRX1_RELA, x = "PRRX1", y = "RELA",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo BASAL - PRRX1 X RELA: Correlação Pearson",
          xlab = "PRRX1", ylab = "RELA")

ggscatter(graph_PRRX1_RELA, x = "PRRX1", y = "RELA",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo BASAL - PRRX1 X RELA: Correlação Sperman",
          xlab = "PRRX1", ylab = "RELA")

#*** PRRX1 X TWIST1

reg_PRRX1_TWIST1= lm(log_data_PRRX1$PRRX1 ~ log_data_TWIST1$TWIST1)
reg_PRRX1_TWIST1

summary(reg_PRRX1_TWIST1)
cor(log_data_PRRX1$PRRX1,log_data_TWIST1$TWIST1)

graph_PRRX1_TWIST1<- data.frame(log_data_PRRX1,log_data_TWIST1)

ggscatter(graph_PRRX1_TWIST1, x = "PRRX1", y = "TWIST1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo Basal - PRRX1 X TWIST1: Correlação Pearson",
          xlab = "PRRX1", ylab = "TWIST1")

ggscatter(graph_PRRX1_TWIST1, x = "PRRX1", y = "TWIST1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo Basal - PRRX1 X TWIST1: Correlação Sperman",
          xlab = "PRRX1", ylab = "TWIST1")


#*** PRRX1 X SNAI2

reg_PRRX1_SNAI2= lm(log_data_PRRX1$PRRX1 ~ log_data_SNAI2$SNAI2)
reg_PRRX1_SNAI2

summary(reg_PRRX1_SNAI2)
cor(log_data_PRRX1$PRRX1,log_data_SNAI2$SNAI2)

graph_PRRX1_SNAI2<- data.frame(log_data_PRRX1,log_data_SNAI2)

ggscatter(graph_PRRX1_SNAI2, x = "PRRX1", y = "SNAI2",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo BASAL - PRRX1 X SNAI2: Correlação Pearson",
          xlab = "PRRX1", ylab = "SNAI2")

ggscatter(graph_PRRX1_SNAI2, x = "PRRX1", y = "SNAI2",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo BASAL - PRRX1 X SNAI2: Correlação Sperman",
          xlab = "PRRX1", ylab = "SNAI2")


#*** PRRX1 X ZEB2
#*
reg_PRRX1_ZEB2= lm(log_data_PRRX1$PRRX1 ~ log_data_ZEB2$ZEB2)
reg_PRRX1_ZEB2

summary(reg_PRRX1_ZEB2)
cor(log_data_PRRX1$PRRX1,log_data_ZEB2$ZEB2)

graph_PRRX1_ZEB2<- data.frame(log_data_PRRX1,log_data_ZEB2)

ggscatter(graph_PRRX1_ZEB2, x = "PRRX1", y = "ZEB2",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo BASAL - PRRX1 X ZEB2: Correlação Pearson",
          xlab = "PRRX1", ylab = "ZEB2")

ggscatter(graph_PRRX1_ZEB2, x = "PRRX1", y = "ZEB2",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo BASAL - PRRX1 X ZEB2: Correlação Sperman",
          xlab = "PRRX1", ylab = "ZEB2")



#*********************SEGUNDA FORMA********************************

data2_PRRX1_NFKB1 <- read.delim(file.choose("~/TCC/Resultados BRUNO/BASAL"), header=T)
data2_PRRX1_RELA <- read.delim(file.choose("~/TCC/Resultados BRUNO/BASAL"), header=T)
data2_PRRX1_SNAI2 <- read.delim(file.choose("~/TCC/Resultados BRUNO/BASAL"), header=T)
data2_PRRX1_TWIST1 <- read.delim(file.choose("~/TCC/Resultados BRUNO/BASAL"), header=T)
data2_PRRX1_ZEB2 <-read.delim(file.choose("~/TCC/Resultados BRUNO/BASAL"), header=T)


#**********Graficos dos dados*****************

#*** PRRX1 X NFKB1

cor(data2_PRRX1_NFKB1$PRRX1,data2_PRRX1_NFKB1$NFKB1 )

ggscatter(data2_PRRX1_NFKB1, x = "PRRX1", y = "NFKB1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo HER2 - PRRX1 X NFKB1: Correlação Pearson",
          xlab = "PRRX1", ylab = "NFKB1")

ggscatter(data2_PRRX1_NFKB1, x = "PRRX1", y = "NFKB1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo HER2 - PRRX1 X NFKB1: Correlação Sperman",
          xlab = "PRRX1", ylab = "NFKB1")



#*** PRRX1 X RELA

cor(data2_PRRX1_RELA$PRRX1,data2_PRRX1_RELA$RELA )

ggscatter(data2_PRRX1_RELA, x = "PRRX1", y = "RELA",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo HER2 - PRRX1 X RELA: Correlação Pearson",
          xlab = "PRRX1", ylab = "RELA")

ggscatter(data2_PRRX1_RELA, x = "PRRX1", y = "RELA",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo HER2 - PRRX1 X RELA: Correlação Sperman",
          xlab = "PRRX1", ylab = "RELA")


#*** PRRX1 X TWIST1

cor(data2_PRRX1_TWIST1$PRRX1,data2_PRRX1_TWIST1$TWIST1 )

ggscatter(data2_PRRX1_TWIST1, x = "PRRX1", y = "TWIST1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo HER2 - PRRX1 X TWIST1: Correlação Pearson",
          xlab = "PRRX1", ylab = "TWIST1")

ggscatter(data2_PRRX1_TWIST1, x = "PRRX1", y = "TWIST1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo HER2 - PRRX1 X TWIST1: Correlação Sperman",
          xlab = "PRRX1", ylab = "TWIST1")



#*** PRRX1 X SNAI2

cor(data2_PRRX1_SNAI2$PRRX1,data2_PRRX1_SNAI2$SNAI2 )

ggscatter(data2_PRRX1_SNAI2, x = "PRRX1", y = "SNAI2",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo HER2 - PRRX1 X SNAI2: Correlação Pearson",
          xlab = "PRRX1", ylab = "SNAI2")

ggscatter(data2_PRRX1_SNAI2, x = "PRRX1", y = "SNAI2",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo HER2 - PRRX1 X SNAI2: Correlação Sperman",
          xlab = "PRRX1", ylab = "SNAI2")



#*** PRRX1 X ZEB2
cor(data2_PRRX1_ZEB2$PRRX1,data2_PRRX1_ZEB2$ZEB2 )

ggscatter(data2_PRRX1_ZEB2, x = "PRRX1", y = "ZEB2",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo HER2 - PRRX1 X ZEB2: Correlação Pearson",
          xlab = "PRRX1", ylab = "ZEB2")

ggscatter(data2_PRRX1_ZEB2, x = "PRRX1", y = "ZEB2",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo HER2 - PRRX1 X ZEB2: Correlação Sperman",
          xlab = "PRRX1", ylab = "ZEB2")
