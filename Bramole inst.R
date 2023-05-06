library("igraph")
library("readxl")
library("openxlsx")
library(ggplot2)
library(corrplot)
library(AER)
library(dplyr)
library(lmtest)
library(sandwich)
library(plm)
library(readxl)
library(stargazer)
library(stats)
library(car)
library(MASS)
cse <- function(reg) {
  rob <- sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}



Gf <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/МатрицаW2 all from surv.xlsx")

Gf <- as.matrix(Gf)
dim(Gf) 
rowSums(Gf)



# Функция для деления на сумму по строке 
abc <- function(x) {
  x/sum(x)
}

Gf <- apply(Gf,1,abc)
Gf <- t(Gf)
View(Gf)




#Матрица Х
OFmatrix <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W2 surv only features.xlsx")
View(OFmatrix)

summary(OFmatrix$dil_cul)

#Переменные
Cul <- OFmatrix$cul
Dil <- OFmatrix$dil_cul
X <- OFmatrix[4:11]
View(X)
X <- as.matrix(X)
I <- diag(192)


corX <- cor(X)
corrplot(corX, method = "circle",title = "", addCoef.col = TRUE)
covX <- cov(X)
covX
stargazer(covX, type = 'text', out = 'covmatrix.html')

######### ПЕРВЫЙ ШАГ РУКАМИ #########
#Создадим матрицу S
i_g <- I - Gf

dim(i_g)
dim(X)
sum(is.na(i_g))
sum(is.na(X))
s1 <- i_g%*%X
s2 <- i_g%*%Gf%*%X
s3 <- i_g%*%Gf%*%Gf%*%X

S <- cbind(s1,s2,s3)
dim(S)
S

#Создадим матрицу P

S_ <- t(S)
P <- S%*%solve(S_%*%S)%*%S_
dim(P)
P

#Создадим матрицу Xcul

ycul <- i_g%*%Gf%*%Cul
xcul1 <- i_g%*%X
xcul2 <- i_g%*%Gf%*%X

Xcul <- cbind(ycul,xcul1,xcul2)
Xcul_ <- t(Xcul)

# 2SLS Cul
Q2SLScul <- solve(Xcul_%*%P%*%Xcul)%*%Xcul_%*%P%*%Cul
dim(Q2SLScul)
Q2SLScul

#Создадим матрицу Xdil
ydil <- i_g%*%Gf%*%Dil
xdil1 <- i_g%*%X
xdil2 <- i_g%*%Gf%*%X

Xdil <- cbind(ydil,xdil1,xdil2)
Xdil_ <- t(Xdil)

# 2SLS dil
Q2SLSdil <- solve(Xdil_%*%P%*%Xdil)%*%Xdil_%*%P%*%Dil
dim(Q2SLSdil)
Q2SLSdil

###### ВТОРОЙ ШАГ РУКАМИ ########

#QLEE Cul
b1 <- Q2SLScul[1]
gam1 <- Q2SLScul[2:9]
del1 <- Q2SLScul[10:17]

Eycul <- Gf%*%solve(I-b1*Gf)%*%i_g%*%(X%*%gam1 + Gf%*%X%*%del1)
dim(Eycul)

Z1 <- cbind(Eycul,xcul1, xcul2)
Z1_ <- t(Z1)

QLEEcul <- solve(Z1_%*%Xcul)%*%Z1_%*%Cul
QLEEcul

# QLEE DIL
b2 <- Q2SLSdil[1]
gam2 <- Q2SLSdil[2:9]
del2 <- Q2SLSdil[10:17]

EyDil <- Gf%*%solve(I-b2*Gf)%*%i_g%*%(X%*%gam2 + Gf%*%X%*%del2)
dim(EyDil)

Z2 <- cbind(EyDil, xdil1, xdil2)
Z2_ <- t(Z2)

QLEEdil <- solve(Z2_%*%Xdil)%*%Z2_%*%Dil
QLEEdil


######## ПЕРВЫЙ ШАГ НЕ РУКАМИ #######

# Первый шаг Cul
Zcul_inst <- cbind(ycul,Z1)
View(Zcul_inst)
Zcul_inst <- as.data.frame(Zcul_inst)
names(Zcul_inst)[2:17] <- LETTERS[1:16]
mod1 <- lm(V1~.-1, data = Zcul_inst)
y_step1cul <- predict(mod1)

# Второй шаг Cul
Xcul_inst <- cbind(Cul,y_step1cul,xcul1,xcul2)
Xcul_inst <- as.data.frame(Xcul_inst)
names(Xcul_inst)[2:17] <- LETTERS[1:16]
View(Xstat_inst)

mod_LEEcul <- lm(Cul~.-1,data = Xcul_inst)
summary(mod_LEEcul)

stargazer(mod_LEEcul,
          title="", type="text",
          se=list(cse(mod_LEEcul)),
          df=FALSE, out = 'Bramole W3 Cul.html')

# Первый шаг dil
Zdil_inst <- cbind(ydil,Z2)
Zdil_inst <- as.data.frame(Zdil_inst)
names(Z2)
names(Zdil_inst)[2:17] <- LETTERS[1:16]
mod2 <- lm(V1~.-1, data = Zdil_inst)
summary(mod2)
y_step1dil <- fitted(mod2)
head(y_step1dil)
# Второй шаг dil
Xdil_inst <- cbind(Dil,y_step1dil,xdil1,xdil2)
Xdil_inst <- as.data.frame(Xdil_inst)
names(Xdil_inst)[2:17] <- LETTERS[1:16]
View(Xdil_inst)

mod_LEEdil <- lm(Dil~.-1,data = Xdil_inst)
summary(mod_LEEdil)
vif(mod_LEEdil)

stargazer(mod_LEEdil,
          title="", type="text",
          se=list(cse(mod_LEEdil)),
          df=FALSE, out = 'Bramole W2 Dil.html')

stargazer(mod_LEEstat, mod_LEEecm,   
          se=list(cse(mod_LEEstat), cse(mod_LEEecm)), 
          title="Оценка с помощью инструмента Брамоля и соавторов", type="text", 
          column.labels=c("Математическая статистика", "Эконометрика"), 
          df=FALSE, digits=3, out="ThetaLEE.html")

mod_LEEecm$coefficients

QLEEecm <- solve(Z2_%*%Xecm)%*%Z2_%*%Ecm
QLEEecm
EyEcm <- Gf%*%solve(I-b2*Gf)%*%i_g%*%(X%*%gam2 + Gf%*%X%*%del2)
dim(EyEcm)

Z2 <- cbind(EyEcm, xecm1, xecm2)
