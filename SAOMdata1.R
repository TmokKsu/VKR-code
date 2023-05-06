library("RSiena")
library("readxl")
library("openxlsx")

w0 <- read_excel("/Users/Пользователь/Desktop/dIpLoM/data/Матрица смежности W0.xlsx")
w1 <- read_excel("/Users/Пользователь/Desktop/dIpLoM/data/Матрица смежности W1.xlsx")
w2 <- read_excel("/Users/Пользователь/Desktop/dIpLoM/data/Матрица смежности W2.xlsx")
w3 <- read_excel("/Users/Пользователь/Desktop/dIpLoM/data/Матрица смежности W3.xlsx")

dim(w3)
rowSums(w3)

dim(w2)
rowSums(w2)

cor(rowSums(w3),rowSums(w1))

w0 <- as.matrix(w0)
w1 <- as.matrix(w1)
w2 <- as.matrix(w2)
w3 <- as.matrix(w3)

variables <- read_excel("/Users/Пользователь/Desktop/dIpLoM/data/SAOM data.xlsx")
dim(variables)
View(variables)
female <- coCovar(variables$female)
dorm <- coCovar(variables$dorm)
group <- coCovar(variables$group)
mattest <- coCovar(variables$mattest)
angtest <- coCovar(variables$angtest)
ege_m <- coCovar(variables$egemat)
ege_r <- coCovar(variables$egerus)


diligence_cul <- as.matrix(variables[9:11])
View(diligence_cul)
dil_cul <- varCovar(diligence_cul)

#diligence_alg <- as.matrix(variables[6:8])
#dil_alg <- varCovar(diligence_alg)

culculus <- as.matrix(variables[13:15])
cul <- varCovar(culculus)
View(culculus)


Wdata <- array( c(w1,w2,w3),
                         dim = c( 162, 162, 3) )

friendship <- sienaDependent(Wdata)
cul <- sienaDependent(array(c(culculus$cul_w1, culculus$cul_w2,
                              culculus$cul_w3 ), dim = c(162, 1, 3 ) ), type = "behavior" )


mydata <- sienaDataCreate( friendship, female, dorm, group)
mydata1 <- sienaDataCreate( friendship, female, dorm, group, dil_cul, cul, mattest, angtest, ege_m, ege_r)
# Check what we have
mydata1

print01Report( mydata, modelname="SAOM data without varcovar.html")

myeff <- getEffects( mydata )
myeff1 <- getEffects( mydata1 )
# All the effects that are available given the structure of this data set can be seen from
effectsDocumentation(myeff1)
myeff <- includeEffects( myeff, transTrip, cycle3, density)#, recip, balance, inPop)
myeff1 <- includeEffects( myeff1, transTrip, cycle3, density, balance)
# and some covariate effects:
myeff <- includeEffects( myeff, egoX, altX, sameX, interaction1 = "female" )
myeff <- includeEffects( myeff, sameX, egoX, altX, interaction1 = "dorm" )
myeff <- includeEffects( myeff, simX,sameX, interaction1 = "group" )

myeff1 <- includeEffects( myeff1, egoX, altX, sameX, interaction1 = "female" )
myeff1 <- includeEffects( myeff1, sameX, egoX, altX, interaction1 = "dorm" )
myeff1 <- includeEffects( myeff1, simX,sameX, interaction1 = "group" )
myeff1 <- includeEffects( myeff1, egoX, altX, simX, interaction1 = "mattest" )
myeff1 <- includeEffects( myeff1, egoX, altX, simX, interaction1 = "angtest" )
myeff1 <- includeEffects( myeff1, egoX, altX, simX, interaction1 = "ege_m" )
myeff1 <- includeEffects( myeff1, egoX, altX, simX, interaction1 = "ege_r" )


myeff1 <- includeEffects( myeff1, egoX, altX, simX, interaction1 = "dil_cul" )
myeff1 <- includeEffects( myeff1, egoX, altX, simX, interaction1 = "cul" )
myeff1 <- includeEffects(myeff1, name = "cul",
                                   simAllNear,totSim,avAlt, indeg,outdeg,
                                   interaction1 = "friendship" )

myeff1 <- includeEffects(myeff1,name = "cul", avAltAltX,
                         interaction1 = "female", interaction2 = "friendship")
myeff1 <- includeEffects(myeff1,name = "cul", effFrom,
                         interaction1 = "female")
myeff1 <- includeEffects(myeff1,name = "cul", effFrom,
                         interaction1 = "dorm")
myeff1 <- includeEffects(myeff1,name = "cul", effFrom,
                         interaction1 = "group")
myeff1 <- includeEffects(myeff1,name = "cul", effFrom,
                         interaction1 = "dil_cul")
myeff1 <- includeEffects(myeff1,name = "cul", effFrom,
                         interaction1 = "mattest")
myeff1 <- includeEffects(myeff1,name = "cul", effFrom,
                         interaction1 = "angtest")
myeff1 <- includeEffects(myeff1,name = "cul", effFrom,
                         interaction1 = "ege_m")
myeff1 <- includeEffects(myeff1,name = "cul", effFrom,
                         interaction1 = "ege_r")

myeff1 <- includeEffects(myeff1,name = "cul", avAltAltX,
                         interaction1 = "dil_cul", interaction2 = "friendship")
#myeff
myeff1


myCoEvAlgorithm <- sienaAlgorithmCreate(projname = 'w1w3ev' )
w1w3ev <- siena07(myCoEvAlgorithm, data = mydata1,
                   effects = myeff1)

w1w3beh1 <- siena07(myCoEvAlgorithm, data = mydata1,
                   effects = myeff1, prevAns = w1w3beh)

siena.table(w1w3ev, type="html", sig=TRUE)

w0w3beh <- siena07(myCoEvAlgorithm, data = mydata1,
                   effects = myeff1)
siena.table(w0w3beh, type="html", sig=TRUE)

#myalgorithm <- sienaAlgorithmCreate(projname = 'saom data without var w1-w3.alg' )
#myalgorithm1 <- sienaAlgorithmCreate(projname = 'saom data with varcovar w1-w3.alg' )
?sienaAlgorithmCreate
#ans <- siena07( myalgorithm, data = mydata, effects = myeff)
#ans
ans$tconv.max
summary(ans)

ans1 <- siena07( myalgorithm1, data = mydata1, effects = myeff1, prevAns=ans)
ans1
ans$tconv.max
summary(ans)



#выдача в html
siena.table(ans, type="html", sig=TRUE)

culscore <- sienaDependent(cul)
mydata1 <- sienaDataCreate( culscore, friendship, female, dorm, group, dil_cul)
# Check what we have
mydata1

print01Report( mydata, modelname="saomdata1.1")

myeff1 <- getEffects( mydata1 )
myeff1 <- includeEffects( myeff1, transTrip, cycle3)
# and some covariate effects:
myeff1 <- includeEffects( myeff1, egoX, altX, simX, interaction1 = "female" )
myeff1 <- includeEffects( myeff1, simX, egoX, altX, interaction1 = "dorm" )
myeff1 <- includeEffects( myeff1, simX, interaction1 = "group" )
myeff1 <- includeEffects( myeff1, egoX, altX, simX, interaction1 = "dil_cul" )

myeff1

myalgorithm1 <- sienaAlgorithmCreate( projname = 'saomdata1.2.alg' )
ans1 <- siena07( myalgorithm1, data = mydata1, effects = myeff1)
ans1
ans1$tconv.max
summary(ans)

#выдача в html
siena.table(ans1, type="html", sig=TRUE)

