library("tidyverse")
library("ggplot2")
library("ggpubr")
library("plotly")
library("reshape2")
library("pheatmap")
require(ggplot2)
require(colorspace)
library(grid)
library(corrplot)
library("ggcorrplot")
library(ComplexHeatmap)


#Rodar para cada subtipo

#*********************PARA OS DADOS DO LINKEDOMICS********************************

data_TWIST1_PRRX1 <- read.delim(file.choose("~Documentos/TCC/Main data/Linkedomics"), header=T)
data_TWIST1_TNC <- read.delim(file.choose("~Documentos/TCC/Main data/Linkedomics"), header=T)
data_PRRX1_TNC <- read.delim(file.choose("~Documentos/TCC/Main data/Linkedomics"), header=T)



#**********Graficos dos dados*****************
#*PARA CADA SUBTIPO*************************************************

#*** TWIST1 X PRRX1

cor(data_TWIST1_PRRX1$TWIST1,data_TWIST1_PRRX1$PRRX1 )

ggscatter(data_TWIST1_PRRX1, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo BASAL - TWIST1 X PRRX1: Correlação Pearson",
          xlab = "TWIST1", ylab = "PRRX1")

ggscatter(data_TWIST1_PRRX1, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo BASAL  - TWIST1 X PRRX1: Correlação Spearman",
          xlab = "TWIST1", ylab = "PRRX1")


#*** TWIST1X TNC

cor(data_TWIST1_TNC$TWIST1,data_TWIST1_TNC$TNC )

ggscatter(data_TWIST1_TNC, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo BASAL - TWIST1 X TNC: Correlação Pearson",
          xlab = "TWIST1", ylab = "TNC")

ggscatter(data_TWIST1_TNC, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo BASAL  - TWIST1 X TNC: Correlação Spearman",
          xlab = "TWIST1", ylab = "TNC")



#*** PRRX1 X TNC

cor(data_PRRX1_TNC$PRRX1,data_PRRX1_TNC$TNC)

ggscatter(data_PRRX1_TNC, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo BASAL - PRRX1 X TNC: Correlação Pearson",
          xlab = "PRRX1", ylab = "TNC")

ggscatter(data_PRRX1_TNC, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo BASAL - PRRX1 X TNC: Correlação Spearman",
          xlab = "PRRX1", ylab = "TNC")




#FIM DO PROGRAMA


















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
