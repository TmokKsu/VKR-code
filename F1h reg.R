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


## Робасные стандартные

cse <- function(reg) {
  rob <- sqrt(diag(vcovHC(reg, type = "HC1")))
  return(rob)
}

#W1
data <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W1 full net features.xlsx")
View(data)

mod1 <- lm(cul~ f1h_cul+dil_cul+female+host+mathtest+angtest+ege_mat+ege_rus+indeg+outdeg+fhost+fmat_test+fang_test+fege_mat+fege_rus, data=data)
summary(mod1)

stargazer(mod1,
          title="Инструмент друзья через одно рукопожатие", type="text",
          se=list(cse(mod1)),
          df=FALSE, out = "W1 1 Friend 1 hand instrument cul.html")
vif(mod1)
resettest(mod1)
crPlots(mod1)
bptest(mod1)

mod1.1 <- update(mod1, .~. -fege_rus-ege_rus-fmat_test-fhost)
summary(mod1.1)
stargazer(mod1.1,
          title="Инструмент друзья через одно рукопожатие", type="text",
          se=list(cse(mod1.1)),
          df=FALSE, out = "W1 1 Friend 1 hand instrument cul mod1.1.html")
vif(mod1.1)
resettest(mod1.1)
crPlots(mod1.1)
bptest(mod1.1)

#mod2 <- lm((dil_cul)~ dil_cul1hand+female+host+mathtest+angtest+ege_mat+ege_rus+indeg+outdeg+female_f+host_f+outdeg_f+indef_f, data=data)
#summary(mod2)

#stargazer(mod2,
#          title="Инструмент друзья через одно рукопожатие", type="text",
#          se=list(cse(mod2)),
#          df=FALSE, out = "W1 1 Friend 1 hand instrument dilcul.html")

#W2
data1 <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W2 full net features.xlsx")
View(data1)

mod2 <- lm(cul~ f1h_cul+dil_cul+female+host+mathtest+angtest+ege_mat+ege_rus+indeg+outdeg+fhost+fmat_test+fang_test+fege_mat+fege_rus, data=data1)
summary(mod2)

stargazer(mod2,
          title="Инструмент друзья через одно рукопожатие", type="text",
          se=list(cse(mod2)),
          df=FALSE, out = "W2 Friend 1 hand instrument cul.html")
vif(mod2)
resettest(mod2)
crPlots(mod2)
bptest(mod2)

mod2.1 <- update(mod2, .~. -female - fhost - fmat_test - fege_rus -outdeg -angtest -fang_test)
summary(mod2.1)
stargazer(mod2.1,
          title="Инструмент друзья через одно рукопожатие", type="text",
          se=list(cse(mod1.1)),
          df=FALSE, out = "W2 Friend 1 hand instrument cul mod2.1.html")
vif(mod2.1)
resettest(mod2.1)
crPlots(mod2.1)
bptest(mod2.1)

#W3

data2 <- read_excel("C:/Users/Пользователь/Desktop/dIpLoM/data/static models/W3 full net features.xlsx")
View(data1)

mod3 <- lm(cul~ f1h_cul+dil_cul+female+host+mathtest+angtest+ege_mat+ege_rus+indeg+outdeg+fhost+fmat_test+fang_test+fege_mat+fege_rus, data=data2)
summary(mod3)

stargazer(mod3,
          title="Инструмент друзья через одно рукопожатие", type="text",
          se=list(cse(mod3)),
          df=FALSE, out = "W3 Friend 1 hand instrument cul.html")
vif(mod3)
resettest(mod3)
crPlots(mod3)
bptest(mod3)

mod3.1 <- update(mod2, .~.  -ege_rus -outdeg -fang_test - fhost -angtest -fege_ang -female -fege_mat -fege_rus)
summary(mod3.1)
stargazer(mod3.1,
          title="Инструмент друзья через одно рукопожатие", type="text",
          se=list(cse(mod3.1)),
          df=FALSE, out = "W3 Friend 1 hand instrument cul mod3.1.html")
vif(mod3.1)
resettest(mod3.1)
crPlots(mod3.1)
bptest(mod3.1)
