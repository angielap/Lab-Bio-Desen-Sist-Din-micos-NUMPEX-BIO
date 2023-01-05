
install.packages("ggpubr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("reshape2")
install.packages("pheatmap")
install.packages("colorspace")
install.packages("ggcorrplot")


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


#***********Abrindo dados*******************
alldata <- read.delim(file.choose("~/TCC"))

alldata$STUDY_ID <- NULL
rownames(alldata)<-alldata$SAMPLE_ID
alldata$SAMPLE_ID <- NULL

dim(alldata)  #conta o numero de linhas e colunas



#***********Log dos dados********************


log2(alldata)
alldata_log <-log2(alldata)

#**********Graficos dos dados*****************
reg_alldata_TWIST1_PRRX1= lm(alldata_log$TWIST1 ~ alldata_log$PRRX1)
reg_alldata_TWIST1_PRRX1
summary(reg_alldata_TWIST1_PRRX1)

ggscatter(alldata_log, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title = "Correlação TWIST1 X PRRX1 Pearson",
          xlab = "TWIST1", ylab = "PRRX1")

ggscatter(alldata_log, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", title = "Correlação TWIST1 X PRRX1 Spearman",
          xlab = "TWIST1", ylab = "PRRX1")



reg_alldata_PRRX1_TNC= lm(alldata_log$TNC ~ alldata_log$PRRX1)
reg_alldata_PRRX1_TNC
summary(reg_alldata_PRRX1_TNC)

ggscatter(alldata_log, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title = "Correlação PRXX1 X TNC Pearson",
          xlab = "PRRX1", ylab = "TNC")

ggscatter(alldata_log, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", title = "Correlação PRXX1 X TNC Spearman",
          xlab = "PRRX1", ylab = "TNC")



reg_alldata_TWIST1_TNC= lm(alldata_log$TWIST1 ~ alldata_log$TNC)
reg_alldata_TWIST1_TNC
summary(reg_alldata_TWIST1_TNC)

ggscatter(alldata_log, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",  title = "Correlação TWIST1 X TNC Pearson",
          xlab = "TWIST1", ylab = "TNC")


ggscatter(alldata_log, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",  title = "Correlação TWIST1 X TNC Spearman",
          xlab = "TWIST1", ylab = "TNC")




#**********grafico de dispersão******************
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


#**********Matrizes de correlaçao entre variaveis**********
cor(alldata, use ="complete.obs")
#cor(alldata$TNC,alldata$PRRX1)
heatmap(cor(alldata, use ="complete.obs"))

ggcorrplot(cor(alldata,use ="complete.obs"))
#ggcorrplot(cor(alldata,use ="complete.obs", hc.=TRUE, method="circle"))


'''
heatmap(as.matrix(alldata))
col <-colorRampPalette(c("blue","white","red"))(20)
cormat<-
  heatmap(cormat,col=col, symm = TRUE)
'''

#********Correlação entre variaveis e amostras*************
#TCGA-C8-A9FZ-01 e TCGA-AC-A5EI-01

heatmap(as.matrix(alldata))

mat_alldata<-data.matrix(alldata)  # [ ,c(4:100)])

class(alldata)
class(mat_alldata)

head(mat_alldata)
Heatmap(mat_alldata)


pheatmap(mat_alldata,
         color= colorRampPalette(c("deepskyblue4","goldenrod","firebrick"))(100),
         fontsize_col=2,
         show_rownames=F,
         cluster_cols=F,
         cluster_rows=T,
         main="Heatmap",
)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")


library(ComplexHeatmap)

#Heatmap(alldata) #dados completos em dataframe
#Heatmap(mat_alldata) #dados em matrizes

#Normalizando os dados
heatmap(mat_alldata, scale="column")

Heatmap(mat_alldata,
        use ="complete.obs",
        name = "Heatmap", #title of legend
        column_title = "Variables", row_title = "Samples",
        row_names_gp = gpar(fontsize = 7) # Text size for row names
)







#cor.res <- round(cor(alldata),1)
#ggplot(alldata)


'''
summary(alldata)
cor(alldata)
m<-cor(alldata)
view(m)
'''

library(corrplot)

?corrplot

corrplot(m)









cor(alldata)




library(corrplot)

plot(alldata$PRRX1, alldata$TWIST1)

cor(alldata) # Corr matrix
round(corel, 3)

?round




data <- data.frame(
  x=seq(10,100),
  y=seq(10,100)/2+rnorm(90)
)

# Make the plot
ggplot(data, aes(x=x, y=y)) +
  geom_line() +
  scale_y_log10( breaks=c(1,5,10,15,20,50,100), limits=c(1,100) )




'''
?ggscatter

ggscatter(alldata,x="PRRX1", y="TWIST1")
          add = "reg.line", conf.int = TRUE, #Plot Regression Line  #Confidence Intervals for Model Parameters
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "PRRX1", ylab = "TWIST1")


shapiro.test(alldata$SAMPLE_ID)
shapiro.test(data1$TWIST1)

library("ggpubr")
ggqqplot(data1$PRRX1, Xlab = "PRRX1") #<<<< PRRX1
ggqqplot(data2$TWIST1, ylab = "TWIST1")  #<<<<<<<<<< TWIST1

alldata <-read.csv2("~/TCC/Exemplo de Rna Seq analisis/Correlations/all_data.csv",header = TRUE, sep = ";", dec=".")

data1 <- read.delim(file.choose("~/TCC/Exemplo de Rna Seq analisis/Correlations"))

#data1 <- read.csv(file.choose("~/TCC/Exemplo de Rna Seq analisis/Correlations"))
data1

data2 <- read.delim(file.choose("~/TCC/Exemplo de Rna Seq analisis/Correlations"))
#data2<-read.csv(file.choose("~/TCC/Exemplo de Rna Seq analisis/Correlations"))
data2



d <- diamonds[sample(nrow(diamonds), 1000), ]
# without log scales
fig <- plot_ly(d, x = ~carat, y = ~price) %>% add_markers()

fig

a <-alldata[sample(nrow(alldata), 1000), ]
fig <- plot_ly(a, x = ~PRRX1, y = ~TWIST1) %>% add_markers()

fig


'''
