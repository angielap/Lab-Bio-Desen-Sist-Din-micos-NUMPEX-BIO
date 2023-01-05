
library("tidyverse")
library("ggplot2")
library("ggpubr")
library("plotly")
library("reshape2")
require(ggplot2)
require(colorspace)
library(grid)
library(corrplot)
library("ggcorrplot")

#Chamada de dados

subtype_TNC <- read.delim(file.choose("~/TCC/Main data"))
subtype_PRRX1 <- read.delim(file.choose("~/TCC/Main data"))
subtype_TWIST1 <- read.delim(file.choose("~/TCC/Main data"))


#********************GENE TNC*****************************
rownames(subtype_TNC)<-subtype_TNC$SAMPLE_ID
subtype_TNC$Sample.Id <- NULL

log_subtype_TNC<-cbind(subtype_TNC, log2(subtype_TNC$TNC))
log_subtype_TNC$TNC <- NULL

colnames(log_subtype_TNC)[2] <- "TNC"

dim(log_subtype_TNC)
str(log_subtype_TNC)

#********************GENE PRRX1*****************************
rownames(subtype_PRRX1)<-subtype_PRRX1$Sample.Id
subtype_PRRX1$Sample.Id <- NULL

log_subtype_PRRX1<-cbind(subtype_PRRX1, log2(subtype_PRRX1$PRRX1))
log_subtype_PRRX1$PRRX1 <- NULL

colnames(log_subtype_PRRX1)[2] <- "PRRX1"

dim(log_subtype_PRRX1)

#********************GENE TWIST1*****************************
rownames(subtype_TWIST1)<-subtype_TWIST1$Sample.Id
subtype_TWIST1$Sample.Id <- NULL

log_subtype_TWIST1<-cbind(subtype_TWIST1, log2(subtype_TWIST1$TWIST))
log_subtype_TWIST1$TWIST <- NULL

colnames(log_subtype_TWIST1)[2] <- "TWIST1"

dim(log_subtype_TWIST1)

#*********************Subtipo A****************************
#busca por subtipo

a_PRRX1<-log_subtype_PRRX1[log_subtype_PRRX1$Group=="(A) BRCA_Basal",]
a_TNC<- log_subtype_TNC[log_subtype_TNC$Group=="(A) BRCA_Basal",]
a_TWIST1<-log_subtype_TWIST1[log_subtype_TWIST1$Group=="(A) BRCA_Basal",]

#PRRX1 & TNC
cor(a_PRRX1$PRRX1,a_TNC$TNC )

#Anova com dados sem log
anova_A_PRRX1_TNC <- aov(subtype_PRRX1$PRRX1 ~ subtype_TNC$TNC)
summary(anova_A_PRRX1_TNC)


#grafico de dispersão e reta de regressão
plot(a_PRRX1[,2],a_TNC[,2], main="Correlação",xlab="PRRX1",ylab="TNC",pch=16)
grid()
regressao= lm(a_PRRX1$PRRX1 ~ a_TNC$TNC)
regressao
summary(regressao)
abline(a = 2.6343, b=0.2037) #abline(lm(a_PRRX1$PRRX1 ~ a_TNC$TNC))

a_TNC_PRRX1<- data.frame(a_PRRX1,a_TNC)
ggscatter(a_TNC_PRRX1, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title= " Subtipo A - TNC X PRRX1 : Correlação Pearson",
          xlab = "PRRX1", ylab = "TNC")

a_TNC_PRRX1<- data.frame(a_PRRX1,a_TNC)
ggscatter(a_TNC_PRRX1, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.meth = "spearman", title= " Subtipo A - TNC X PRRX1 : Correlação Spearman",
          xlab = "PRRX1", ylab = "TNC")


#TWIST x PRRX1
cor(a_TWIST1$TWIST1,a_PRRX1$PRRX1)

Reg_A_TWIST1_PRRX1= lm(a_TWIST1$TWIST1 ~ a_PRRX1$PRRX1)
Reg_A_TWIST1_PRRX1
summary(Reg_A_TWIST1_PRRX1)

anova_A_TWIST1_PRRX1 <- aov(a_TWIST1$TWIST1 ~ a_PRRX1$PRRX1)
summary(anova_A_TWIST1_PRRX1)

a_TWIST1_PRRX1<- data.frame(a_TWIST1,a_PRRX1)
ggscatter(a_TWIST1_PRRX1, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo A - TWIST1 X PRRX1 : Correlação Pearson",
          xlab = "TWIST1", ylab = "PRRX1")

a_TWIST1_PRRX1<- data.frame(a_TWIST1,a_PRRX1)
ggscatter(a_TWIST1_PRRX1, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", title= " Subtipo A - TWIST1 X PRRX1 : Correlação Spearman",
          xlab = "PRRX1", ylab = "PRRX1")


#TWIST1 x TNC
cor(a_TWIST1$TWIST1,a_TNC$TNC)

Reg_A_TWIST1_TNC= lm(a_TWIST1$TWIST1 ~ a_TNC$TNC)
Reg_A_TWIST1_TNC
summary(Reg_A_TWIST1_TNC)

anova_A_TWIST1_TNC <- aov(a_TWIST1$TWIST1 ~ a_TNC$TNC)
summary(anova_A_TWIST1_TNC)

a_TWIST1_TNC <-data.frame(a_TWIST1,a_TNC)
ggscatter(a_TWIST1_TNC, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title= " Subtipo A - TWIST1 X TNC : Correlação Pearson",
          xlab = "TWIST1", ylab = "TNC")

a_TWIST1_TNC <-data.frame(a_TWIST1,a_TNC)
ggscatter(a_TWIST1_TNC, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE,  cor.method = "spearman", title= " Subtipo A - TWIST1 X TNC : Correlação Spearman",
          xlab = "TWIST1", ylab = "TNC")


#*********************Subtipo B****************************

b_TNC<- log_subtype_TNC[log_subtype_TNC$Group=="(B) BRCA_Her2",]
b_PRRX1<-log_subtype_PRRX1[log_subtype_PRRX1$Group=="(B) BRCA_Her2",]
b_TWIST1<-log_subtype_TWIST1[log_subtype_TWIST1$Group=="(B) BRCA_Her2",]



#PRRX1 & TNC
cor(b_PRRX1$PRRX1,b_TNC$TNC)

Reg_B_PRRX1_TNC= lm(b_PRRX1$PRRX1 ~ b_TNC$TNC)
Reg_B_PRRX1_TNC
summary(Reg_B_PRRX1_TNC)

anova_B_PRRX1_TNC <- aov(b_PRRX1$PRRX1 ~ b_TNC$TNC)
summary(anova_B_PRRX1_TNC)


b_PRRX1_TNC<- data.frame(b_PRRX1,b_TNC)
ggscatter(b_PRRX1_TNC, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo B - TNC X PRRX1 : Correlação Pearson",
          xlab = "PRRX1", ylab = "TNC")

b_PRRX1_TNC<- data.frame(b_PRRX1,b_TNC)
ggscatter(b_PRRX1_TNC, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", title= " Subtipo B - TNC X PRRX1 : Correlação Spearman",
          xlab = "PRRX1", ylab = "TNC")


#Twist1 x PRRX1
cor(b_TWIST1$TWIST1,b_PRRX1$PRRX1)

Reg_B_TWIST1_PRRX1= lm(b_TWIST1$TWIST1 ~ b_PRRX1$PRRX1)
Reg_B_TWIST1_PRRX1
summary(Reg_B_TWIST1_PRRX1)

anova_B_TWIST1_PRRX1 <- aov(b_TWIST1$TWIST1 ~ b_PRRX1$PRRX1)
summary(anova_B_TWIST1_PRRX1)

b_TWIST1_PRRX1<- data.frame(b_TWIST1,b_PRRX1)
ggscatter(b_TWIST1_PRRX1, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title= " Subtipo B - TWIST1 X PRRX1 : Correlação Pearson",
          xlab = "TWIST1", ylab = "PRRX1")

b_TWIST1_PRRX1<- data.frame(b_TWIST1,b_PRRX1)
ggscatter(b_TWIST1_PRRX1, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", title= " Subtipo B - TWIST1 X PRRX1 : Correlação Spearman",
          xlab = "TWIST1", ylab = "PRRX1")



#TWIST1 x TNC
cor(b_TWIST1$TWIST1,b_TNC$TNC)

Reg_B_TWIST1_TNC= lm(b_TWIST1$TWIST1 ~ b_TNC$TNC)
Reg_B_TWIST1_TNC
summary(Reg_B_TWIST1_TNC)

anova_B_TWIST1_TNC <- aov(b_TWIST1$TWIST1 ~ b_TNC$TNC)
summary(anova_B_TWIST1_TNC)

b_TWIST1_TNC <-data.frame(b_TWIST1,b_TNC)
ggscatter(b_TWIST1_TNC, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title= " Subtipo B - TWIST1 X TNC : Correlação Pearson",
          xlab = "TWIST1", ylab = "TNC")

b_TWIST1_TNC <-data.frame(b_TWIST1,b_TNC)
ggscatter(b_TWIST1_TNC, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", title= " Subtipo B - TWIST1 X TNC : Correlação Spearman",
          xlab = "TWIST1", ylab = "TNC")




#*********************Subtipo C****************************
c_TNC<- log_subtype_TNC[log_subtype_TNC$Group=="(C) BRCA_LumA",]
c_PRRX1<- log_subtype_PRRX1[log_subtype_PRRX1$Group=="(C) BRCA_LumA",]
c_TWIST1<-log_subtype_TWIST1[log_subtype_TWIST1$Group=="(C) BRCA_LumA",]


#PRRX1 & TNC
cor(c_PRRX1$PRRX1,c_TNC$TNC)

Reg_C_PRRX1_TNC= lm(c_PRRX1$PRRX1 ~ c_TNC$TNC)
Reg_C_PRRX1_TNC
summary(Reg_C_PRRX1_TNC)

anova_C_PRRX1_TNC <- aov(c_PRRX1$PRRX1 ~ c_TNC$TNC)
summary(anova_C_PRRX1_TNC)

c_PRRX1_TNC<- data.frame(c_PRRX1,c_TNC)
ggscatter(c_PRRX1_TNC, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title= " Subtipo C - TNC X PRRX1 : Correlação Pearson",
          xlab = "PRRX1", ylab = "TNC")

c_PRRX1_TNC<- data.frame(c_PRRX1,c_TNC)
ggscatter(c_PRRX1_TNC, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", title= " Subtipo C - TNC X PRRX1 : Correlação Spearman",
          xlab = "PRRX1", ylab = "TNC")


#TWIST1 x PRRX1
cor(c_TWIST1$TWIST1,c_PRRX1$PRRX1)

Reg_C_TWIST1_PRRX1= lm(c_TWIST1$TWIST1 ~ c_PRRX1$PRRX1)
Reg_C_TWIST1_PRRX1
summary(Reg_C_TWIST1_PRRX1)


anova_C_TWIST1_PRRX1 <- aov(c_TWIST1$TWIST1 ~ c_PRRX1$PRRX1)
summary(anova_C_TWIST1_PRRX1)

c_TWIST1_PRRX1<- data.frame(c_TWIST1,c_PRRX1)
ggscatter(c_TWIST1_PRRX1, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title= " Subtipo C - TWIST1 X PRRX1 : Correlação Pearson",
          xlab = "TWIST1", ylab = "PRRX1")

c_TWIST1_PRRX1<- data.frame(c_TWIST1,c_PRRX1)
ggscatter(c_TWIST1_PRRX1, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title= " Subtipo C - TWIST1 X PRRX1 : Correlação Spearman",
          xlab = "TWIST1", ylab = "PRRX1")




#TWIST1 x TNC
cor(c_TWIST1$TWIST1,c_TNC$TNC)

Reg_C_TWIST1_TNC= lm(c_TWIST1$TWIST1 ~ c_TNC$TNC)
Reg_C_TWIST1_TNC
summary(Reg_C_TWIST1_TNC)


anova_C_TWIST1_TNC <- aov(c_TWIST1$TWIST1 ~ c_TNC$TNC)
summary(anova_C_TWIST1_TNC)

c_TWIST1_TNC <-data.frame(c_TWIST1,c_TNC)
ggscatter(c_TWIST1_TNC, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",  title="Subtipo C - TWIST1 X TNC : Correlação Pearson",
          xlab = "TWIST1", ylab = "TNC")

c_TWIST1_TNC <-data.frame(c_TWIST1,c_TNC)
ggscatter(c_TWIST1_TNC, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",  title="Subtipo C - TWIST1 X TNC : Correlação Spearman",
          xlab = "TWIST1", ylab = "TNC")



#*********************Subtipo D****************************

d_TNC<- log_subtype_TNC[log_subtype_TNC$Group=="(D) BRCA_LumB",]
d_PRRX1<- log_subtype_PRRX1[log_subtype_PRRX1$Group=="(D) BRCA_LumB",]
d_TWIST1<-log_subtype_TWIST1[log_subtype_TWIST1$Group=="(D) BRCA_LumB",]


#PRRX1 & TNC
cor(d_PRRX1$PRRX1,d_TNC$TNC)

Reg_D_PRRX1_TNC= lm(d_PRRX1$PRRX1 ~ d_TNC$TNC)
Reg_D_PRRX1_TNC
summary(Reg_D_PRRX1_TNC)

anova_D_PRRX1_TNC <- aov(d_PRRX1$PRRX1 ~ d_TNC$TNC)
summary(anova_D_PRRX1_TNC)


d_PRRX1_TNC<- data.frame(d_PRRX1,d_TNC)
ggscatter(d_PRRX1_TNC, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title="Subtipo D - TNC X PRRX1 : Correlação Pearson",
          xlab = "PRRX1", ylab = "TNC")

d_PRRX1_TNC<- data.frame(d_PRRX1,d_TNC)
ggscatter(d_PRRX1_TNC, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", title="Subtipo D - TNC X PRRX1 : Correlação Spearman",
          xlab = "PRRX1", ylab = "TNC")


#TWIST1 x PRRX1
cor(d_TWIST1$TWIST1,d_PRRX1$PRRX1)

Reg_D_TWIST1_PRRX1= lm(d_TWIST1$TWIST1 ~ d_PRRX1$PRRX1)
Reg_D_TWIST1_PRRX1
summary(Reg_D_TWIST1_PRRX1)

anova_D_TWIST1_PRRX1 <- aov(d_TWIST1$TWIST1 ~ d_PRRX1$PRRX1)
summary(anova_D_TWIST1_PRRX1)

d_TWIST1_PRRX1<- data.frame(d_TWIST1,d_PRRX1)
ggscatter(d_TWIST1_PRRX1, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title="Subtipo D - TWIST1 X PRRX1 : Correlação Pearson",
          xlab = "TWIST1", ylab = "PRRX1")

d_TWIST1_PRRX1<- data.frame(d_TWIST1,d_PRRX1)
ggscatter(d_TWIST1_PRRX1, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title="Subtipo D - TWIST1 X PRRX1 : Correlação Spearman",
          xlab = "TWIST1", ylab = "PRRX1")



#TWIST1 x TNC
cor(d_TWIST1$TWIST1,d_TNC$TNC)

Reg_D_TWIST1_TNC= lm(d_TWIST1$TWIST1 ~ d_TNC$TNC)
Reg_D_TWIST1_TNC
summary(Reg_D_TWIST1_TNC)

anova_D_TWIST1_TNC <- aov(d_TWIST1$TWIST1 ~ d_TNC$TNC)
summary(anova_D_TWIST1_TNC)

d_TWIST1_TNC <-data.frame(d_TWIST1,d_TNC)
ggscatter(d_TWIST1_TNC, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title="Subtipo D - TWIST1 X TNC : Correlação Pearson",
          xlab = "TWIST1", ylab = "TNC")


d_TWIST1_TNC <-data.frame(d_TWIST1,d_TNC)
ggscatter(d_TWIST1_TNC, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", title="Subtipo D - TWIST1 X TNC : Correlação Spearman",
          xlab = "TWIST1", ylab = "TNC")


#*********************Subtipo E****************************

e_TNC<- log_subtype_TNC[log_subtype_TNC$Group=="(E) BRCA_Normal",]
e_PRRX1<- log_subtype_PRRX1[log_subtype_PRRX1$Group=="(E) BRCA_Normal",]
e_TWIST1<-log_subtype_TWIST1[log_subtype_TWIST1$Group=="(E) BRCA_Normal",]

#PRRX1 & TNC
cor(e_PRRX1$PRRX1,e_TNC$TNC)

Reg_E_PRRX1_TNC= lm(e_PRRX1$PRRX1 ~ e_TNC$TNC)
Reg_E_PRRX1_TNC
summary(Reg_E_PRRX1_TNC)

anova_E_PRRX1_TNC <- aov(e_PRRX1$PRRX1 ~ e_TNC$TNC)
summary(anova_E_PRRX1_TNC)

e_PRRX1_TNC<- data.frame(e_PRRX1,e_TNC)
ggscatter(e_PRRX1_TNC, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title="Subtipo E - TNC X PRRX1 : Correlação Pearson",
          xlab = "PRRX1", ylab = "TNC")

e_PRRX1_TNC<- data.frame(e_PRRX1,e_TNC)
ggscatter(e_PRRX1_TNC, x = "PRRX1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman", title="Subtipo E - TNC X PRRX1 : Correlação Spearman",
          xlab = "PRRX1", ylab = "TNC")


#TWIST1 x PRRX1
cor(e_TWIST1$TWIST1,e_PRRX1$PRRX1)

Reg_E_TWIST1_PRRX1= lm(e_TWIST1$TWIST1 ~ e_PRRX1$PRRX1)
Reg_E_TWIST1_PRRX1
summary(Reg_E_TWIST1_PRRX1)

anova_E_TWIST1_PRRX1 <- aov(e_TWIST1$TWIST1 ~ e_PRRX1$PRRX1)
summary(anova_E_TWIST1_PRRX1)

e_TWIST1_PRRX1<- data.frame(e_TWIST1,e_PRRX1)
ggscatter(e_TWIST1_PRRX1, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", title="Subtipo E - TWIST1 X PRRX1 : Correlação Pearson",
          xlab = "TWIST1", ylab = "PRRX1")

e_TWIST1_PRRX1<- data.frame(e_TWIST1,e_PRRX1)
ggscatter(e_TWIST1_PRRX1, x = "TWIST1", y = "PRRX1",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman" , title="Subtipo E - TWIST1 X PRRX1 : Correlação Spearman",
          xlab = "TWIST1", ylab = "PRRX1")



#TWIST1 x TNC
cor(e_TWIST1$TWIST1,e_TNC$TNC)

Reg_E_TWIST1_TNC= lm(e_TWIST1$TWIST1 ~ e_TNC$TNC)
Reg_E_TWIST1_TNC
summary(Reg_E_TWIST1_TNC)

anova_E_TWIST1_TNC <- aov(e_TWIST1$TWIST1 ~ e_TNC$TNC)
summary(anova_E_TWIST1_TNC)

e_TWIST1_TNC <-data.frame(e_TWIST1,e_TNC)
ggscatter(e_TWIST1_TNC, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson",title="Subtipo E - TWIST1 X TNC : Correlação Pearson",
          xlab = "TWIST1", ylab = "TNC")

e_TWIST1_TNC <-data.frame(e_TWIST1,e_TNC)
ggscatter(e_TWIST1_TNC, x = "TWIST1", y = "TNC",
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "spearman",title="Subtipo E - TWIST1 X TNC : Correlação Spearman",
          xlab = "TWIST1", ylab = "TNC")







log_subtype_TNC=="(A) BRCA_Basal"
log_subtype_TNC[log_subtype_TNC=="(A) BRCA_Basal"]

#log_subtype_TNC[,c("Group","TNC")]
#log_subtype_TNC[1,1]
#df[linha,coluna]

