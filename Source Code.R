###Setting Working Directory
setwd("")

###Calling Packages
library("randomForest"); library(corrplot); library(neuralnet); library(ggplot2)

###Reading Data
X <- read.csv("University_Dropout_Data.csv")

###Data Pre-treatment
X <- X[-1, ]
X$FECHA.DE.NACIMIENTO <- as.POSIXct(X$FECHA.DE.NACIMIENTO,format="%m/%d/%Y",tz="EST")
X$Edad<-as.numeric(abs(X$FECHA.DE.NACIMIENTO-as.POSIXct("11/27/2023",
                                                        format="%m/%d/%Y",tz="EST")))
for (i in 1:nrow(X)) {
  if(X$Status.20222[i] !=1){
  if(X$Status.20222[i] == 0 && X$Status.20221[i] == 1){
    X$Edad[i] <- as.numeric(abs(X$FECHA.DE.NACIMIENTO[i] -
                                as.POSIXct("08/30/2022",format="%m/%d/%Y",tz="EST")))
  } else if(X$Status.20221[i] == 0 && X$Status.20212[i] == 1){
    X$Edad[i] <- as.numeric(abs(X$FECHA.DE.NACIMIENTO[i] -
                                as.POSIXct("11/30/2021",format="%m/%d/%Y",tz="EST")))
  } else if(X$Status.20212[i] == 0 && X$Status.20211[i] == 1){
    X$Edad[i] <- as.numeric(abs(X$FECHA.DE.NACIMIENTO[i] -
                                as.POSIXct("08/30/2021",format="%m/%d/%Y",tz="EST")))
  } else if(X$Status.20211[i] == 0 && X$Status.20202[i] == 1){
    X$Edad[i] <- as.numeric(abs(X$FECHA.DE.NACIMIENTO[i] -
                                as.POSIXct("11/30/2020",format="%m/%d/%Y",tz="EST")))
  } else if(X$Status.20202[i] == 0 && X$Status.20201[i] == 1){
    X$Edad[i] <- as.numeric(abs(X$FECHA.DE.NACIMIENTO[i] -
                                as.POSIXct("08/30/2020",format="%m/%d/%Y",tz="EST")))
  } else if(X$Status.20201[i] == 0 && X$Status.20192[i] == 1){
    X$Edad[i] <- as.numeric(abs(X$FECHA.DE.NACIMIENTO[i] -
                                as.POSIXct("11/30/2019",format="%m/%d/%Y",tz="EST")))
  } else if(X$Status.20192[i] == 0 && X$Status.20191[i] == 1){
    X$Edad[i]<-as.numeric(abs(X$FECHA.DE.NACIMIENTO-
                                as.POSIXct("08/30/2019",format="%m/%d/%Y",tz="EST")))
  } else if(X$Status.20191[i] == 0 && X$Status.20182[i] == 1){
    X$Edad[i] <- as.numeric(abs(X$FECHA.DE.NACIMIENTO[i] -
                                as.POSIXct("11/30/2018",format="%m/%d/%Y",tz="EST")))
  } else if(X$Status.20182[i] == 0 && X$Status.20181[i] == 1){
    X$Edad[i] <- as.numeric(abs(X$FECHA.DE.NACIMIENTO[i] -
                                as.POSIXct("08/30/2018",format="%m/%d/%Y",tz="EST")))
  } else if(X$Status.20181[i] == 0 && X$Status.20172[i] == 1){
    X$Edad[i] <- as.numeric(abs(X$FECHA.DE.NACIMIENTO[i] -
                                as.POSIXct("11/30/2017",format="%m/%d/%Y",tz="EST")))
  } else if(X$Status.20172[i] == 0 && X$Status.20171[i] == 1){
    X$Edad[i] <- as.numeric(abs(X$FECHA.DE.NACIMIENTO[i] -
                                as.POSIXct("08/30/2017",format="%m/%d/%Y",tz="EST")))
  }
 }
}
X$Edad <- round(X$Edad/365, digits = 0)

X$Promedio.20171 <- as.numeric(X$Promedio.20171) 
X$Promedio.20172 <- as.numeric(X$Promedio.20172)
X$Promedio.20181 <- as.numeric(X$Promedio.20181)
X$Promedio.20182 <- as.numeric(X$Promedio.20182)
X$Promedio.20191 <- as.numeric(X$Promedio.20191)
X$Promedio.20192 <- as.numeric(X$Promedio.20192)
X$Promedio.20201 <- as.numeric(X$Promedio.20201)
X$Promedio.20202 <- as.numeric(X$Promedio.20202)
X$Promedio.20211 <- as.numeric(X$Promedio.20211)
X$Promedio.20212 <- as.numeric(X$Promedio.20212)
X$Promedio.20221 <- as.numeric(X$Promedio.20221)
X$Promedio.20222 <- as.numeric(X$Promedio.20222)

###Creating binary variables Desercion and Sexo
X$Desercion <- X$ESTADO.A.2022.2 == "DESERTOR"
X$Desertado <- 0
X$Desertado[X$Desercion == TRUE] <- "Sí"
X$Desertado[X$Desercion == FALSE] <- "No"
X$Sexo <- X$SEXO == "F"

###Collapsing variables "Creditos" and "Promedio" by mean
X$Creditos.Prom <- rowMeans(X[, grepl("Creditos",colnames(X))], na.rm = TRUE) 
X$Promedio <- rowMeans(X[, grepl("Promedio",colnames(X))], na.rm = TRUE)

##Variable-wise boxplots (and barplot, for the case of Sexo)
Data <- matrix(data = rep(0,4), nrow = 2, ncol = 2)
rownames(Data) <- c("Sí", "No")
colnames(Data) <- c("M", "F")
for (i in 1:2) {
  for (j in 1:2) {
    Data[i,j] <- sum((X$SEXO==colnames(Data)[j])*(X$Desertado==rownames(Data)[i]))
  }
}

rownames(Data) <- c("Desertados", "No Desertados")
barplot(height = Data,beside = TRUE,legend.text = c("Desertados", "No Desertados"))

ggplot(X, aes(x=Desertado, y=Creditos.Prom, fill=Desertado)) +
  geom_boxplot()+ scale_fill_brewer(palette="Greys")
ggplot(X, aes(x=Desertado, y=Promedio, fill=Desertado)) +
  geom_boxplot()+ scale_fill_brewer(palette="Greys")
ggplot(X, aes(x=Desertado, y=PUNTAJE.SABER.11, fill=Desertado)) +
  geom_boxplot()+ scale_fill_brewer(palette="Greys")
ggplot(X, aes(x=Desertado, y=ESTRATO, fill=Desertado)) +
  geom_boxplot()+ scale_fill_brewer(palette="Greys")

ggplot(X, aes(x=Desertado, y=Edad, fill=Desertado)) +
  geom_boxplot()+ scale_fill_brewer(palette="Greys")

###Splitting Males from Females
X_M <- X[X$SEXO == "M", ]
X_F <- X[X$SEXO == "F", ]

###Testing equality of dropout proportion between M vs. F groups
#z test for dropout proportions equality
p1 <- sum(X_M$Desercion == TRUE)/nrow(X_M)
p2 <- sum(X_F$Desercion == TRUE)/nrow(X_F)
p <- sum(X$Desercion == TRUE)/nrow(X); q <- 1-p
z <- (p1-p2)/sqrt(p*q/nrow(X_M) + p*q/nrow(X_F))
p_value_z <- pnorm(z, lower.tail = FALSE)

#Chi-squared test for male/female proportions equality 
p_value_chi2 <- prop.test(x = c(sum(X_M$Desercion == TRUE), 
                                sum(X_F$Desercion == TRUE)), 
                          n = c(nrow(X_M), nrow(X_F)))$p.value

###Splitting dropouts from non-dropouts
X_Desertores <- X[X$ESTADO.A.2022.2 == "DESERTOR", ]
X_No_Desertores <- X[X$ESTADO.A.2022.2 != "DESERTOR", ]

###Test normality for Creditos.mean, Promedio.mean, PUNTAJE.SABER.11, and ESTRATO
shapiro.test(X_Desertores$Creditos.Prom)$p.value
shapiro.test(X_No_Desertores$Creditos.Prom)$p.value
shapiro.test(X_Desertores$Promedio)$p.value
shapiro.test(X_No_Desertores$Promedio)$p.value
shapiro.test(X_Desertores$PUNTAJE.SABER.11)$p.value
shapiro.test(X_No_Desertores$PUNTAJE.SABER.11)$p.value
shapiro.test(X_Desertores$ESTRATO)$p.value
shapiro.test(X_No_Desertores$ESTRATO)$p.value
shapiro.test(X_Desertores$Edad)$p.value
shapiro.test(X_No_Desertores$Edad)$p.value

###Test mean differences between droput vs. non-dropout groups for
#Sex.Boolean, Creditos.mean, Promedio.mean, PUNTAJE.SABER.11, and ESTRATO 
p_value_Creditos <- wilcox.test(X_Desertores$Creditos.Prom,
                                X_No_Desertores$Creditos.Prom)$p.value
p_value_Promedio <- wilcox.test(X_Desertores$Promedio,
                                X_No_Desertores$Promedio)$p.value
p_value_SABER.11 <- wilcox.test(X_Desertores$PUNTAJE.SABER.11,
                                X_No_Desertores$PUNTAJE.SABER.11)$p.value
p_value_ESTRATO <- wilcox.test(X_Desertores$ESTRATO, 
                               X_No_Desertores$ESTRATO)$p.value

p_value_Edad <- wilcox.test(X_Desertores$Edad, 
                            X_No_Desertores$Edad)$p.value

p_value_Promedio
p_value_SABER.11
p_value_ESTRATO
p_value_Edad

###Attempting to create prediction models for binary variable Dropout
##Random Forest model
X_RF <- X[,c("Sexo","Creditos.Prom","Promedio","PUNTAJE.SABER.11",
             "ESTRATO", "Desercion", "Edad")]

##Data Pre-treatment
X_RF <- X_RF[is.na(X_RF$Promedio) == F,]
X_RF <- X_RF[is.na(X_RF$PUNTAJE.SABER.11) == F,]
X_RF <- X_RF[is.na(X_RF$ESTRATO) == F,]
X_RF <- X_RF[is.na(X_RF$Edad) == F,]

set.seed(10)
ind <- sample(1:2, nrow(X_RF), replace = T, prob = c(0.7,0.3))
X_RF_Training <- X_RF[ind == 1,]
X_RF_Testing <- X_RF[ind == 2,]
#X_RF_Training <- X_RF[sample(1:nrow(X_RF), 650, replace = F),]
#X_RF_Testing <- X_RF[sample(1:nrow(X_RF), 650, replace = F),]

RF.mod <- randomForest(x = X_RF_Training[,c("Sexo","Creditos.Prom",
                                            "Promedio", "PUNTAJE.SABER.11", "Edad")], 
                       y=as.factor(X_RF_Training$Desercion), 
                       ntree = 500)
#Fitting error
Fitting <- predict(RF.mod, newdata = X_RF_Training[,c("Sexo","Creditos.Prom",
                                                      "Promedio", 
                                                      "PUNTAJE.SABER.11", "Edad")])
fit.error.RF <- 100*sum(X_RF_Training$Desercion!=Fitting)/nrow(X_RF_Training)
fit.error.RF

#Prediction error
Preds <- predict(RF.mod, newdata = X_RF_Testing[,c("Sexo","Creditos.Prom",
                                                   "Promedio", 
                                                   "PUNTAJE.SABER.11", "Edad")])
pred.error.RF <- 100*sum(X_RF_Testing$Desercion != Preds)/nrow(X_RF_Testing)
pred.error.RF

#Computing variable importance with Gini's purity increment criterion
var.importance <- importance(RF.mod)
var.importance

##Correlation Heatmaps
#Point-biserial correlation is equivalent to Pearson's when binary vs. continuous variables are analyzed
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], method = "pearson", ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
corMat=cor(X_RF, method = "pearson")
temp <- is.na(corMat)
corMat[temp] <- as.numeric(0)
p.mat <- cor.mtest(X_RF)
corrplot(corMat, type="upper", order="hclust", p.mat = p.mat, sig.level = 0.05, 
         pch.cex=1.1, cl.cex = 0.7, tl.cex = 0.7)

##Logit Regression
#First model
logit.mod <- glm(formula = Desercion ~ Sexo + Creditos.Prom + Promedio + 
                   PUNTAJE.SABER.11 + ESTRATO + Edad, 
                 family = binomial(link = "logit"), data = X_RF_Training)
summary(logit.mod)

#Second model
logit.mod2 <- glm(formula = Desercion ~ Creditos.Prom + Promedio + Edad, 
                  family = binomial(link = "logit"), data = X_RF_Training)
summary(logit.mod2)
#Fitting error
Fitting.logit <- predict(logit.mod2, newdata = X_RF_Training)
Fitting.logit <- Fitting.logit >= 0.5
Fit.error.logit<-100*sum(X_RF_Training$Desercion!=Fitting.logit)/nrow(X_RF_Training)
Fit.error.logit

#Prediction error
Preds.logit <- predict(logit.mod2, newdata = X_RF_Testing)
Preds.logit <- Preds.logit >= 0.5
pred.error.logit<-100*sum(X_RF_Testing$Desercion!=Preds.logit)/nrow(X_RF_Testing)
pred.error.logit
