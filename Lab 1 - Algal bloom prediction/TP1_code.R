rm(list = ls())

#Ouverture des librairies
library(DMwR)

#Début du dataset
head(algae)

#Résumé stat des variables
summary(algae)

#Histogramme, estimateur à noyau de la densité et QQ-plot
library(car)
op = par(mfrow=c(1,2))
hist(algae$mxPH, prob=T, xlab="",
     main="Histogram of maximum pH value",ylim=0:1)
lines(density(algae$mxPH,na.rm=T))
rug(jitter(algae$mxPH))
qqnorm(algae$mxPH,main="Normal QQ plot of maximum pH")
par(op)

#Boxplot conditionnelle à la variable catégorielle size
library(lattice)
bwplot(size ~ a1, data=algae, ylab="River Size",xlab="Algal A1")

### Données manquantes ###
algae[!complete.cases(algae),]
nrow(algae[!complete.cases(algae),])

#Suppression des lignes avec NA --> création de algae1
algae1 = na.omit(algae)
nrow(algae)
nrow(algae1)

#On remplace les NAs par valeurs plus fréquentes
algae2 = centralImputation(algae)
nrow(algae[!complete.cases(algae),])
nrow(algae2[!complete.cases(algae2),])

#Avec méthode stat
algae3 = knnImputation(algae, k =10, meth = "median")
nrow(algae[!complete.cases(algae),])
nrow(algae3[!complete.cases(algae3),])

### Régression lin?aire multiple ###
algae = knnImputation(algae, k =10, meth = "median")
lm.a1 <- lm(a1 ~ ., data = algae[, 1:12])
summary(lm.a1)


anova(lm.a1)

final.lm = step(lm.a1)

summary(final.lm)

### Decision trees ###
algae = knnImputation(algae, k =10, meth = "median")
library(rpart)
rt.a1 = rpart(a1 ~ ., data = algae[, 1:12])
rt.a1

#Affichage de l'arbre de décision obtenu :
par(lwd=2, col="red")
plot(rt.a1, compress=TRUE)
text(rt.a1, use.n=TRUE,col="blue")

#Affichage alternatif :
par(lwd=2, bg="lemonchiffon3")
prettyTree(rt.a1,col="navy",bg="lemonchiffon")

#Qualité de prédiction :
lm.predictions.a1 = predict(final.lm, algae)
rt.predictions.a1 = predict(rt.a1, algae)
regr.eval(algae[, "a1"], rt.predictions.a1, train.y = algae[,"a1"])
regr.eval(algae[, "a1"], lm.predictions.a1, train.y = algae[,"a1"])

#Plot des valeurs prédites vs observées
par(mfrow = c(1, 2), col="navy", bg="lemonchiffon1")
plot(lm.predictions.a1, algae[, "a1"], main = "Linear Model",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
plot(rt.predictions.a1, algae[, "a1"], main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)

#On fait la même chose, mais sur la base de test
summary(test.algae)

#Imputation de valeurs manquantes
test.algae = knnImputation(test.algae, k =10, meth = "median")
#Prédictions sur base de test
lm.predictions.a1 = predict(final.lm,test.algae)
rt.predictions.a1 = predict(rt.a1,test.algae)

#Evaluation des performances des modèles sur la base de test
regr.eval(algae.sols$a1, lm.predictions.a1, train.y = algae[,"a1"])
regr.eval(algae.sols$a1, rt.predictions.a1, train.y = algae[,"a1"])

#Et maintenant sur les variables a2 à a7
#a2
lm.a2 <- lm(a2 ~ ., data = algae[, c(1:11,13)])
final.lm = step(lm.a2)

rt.a2 = rpart(a2 ~ ., data = algae[, c(1:11,13)])

lm.predictions.a2 = predict(final.lm,test.algae)
rt.predictions.a2 = predict(rt.a2,test.algae)

regr.eval(algae.sols$a2, lm.predictions.a2,train.y = algae[,"a2"])
regr.eval(algae.sols$a2, rt.predictions.a2,train.y = algae[,"a2"])

par(mfrow = c(1, 2), col="navy", bg="lemonchiffon1")
plot(lm.predictions.a2, algae[, "a2"], main = "Linear Model",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
plot(rt.predictions.a2, algae[, "a2"], main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)


#a3
lm.a3 <- lm(a3 ~ ., data = algae[, c(1:11,14)])
final.lm = step(lm.a3)

rt.a3 = rpart(a3 ~ ., data = algae[, c(1:11,14)])

lm.predictions.a3 = predict(final.lm,test.algae)
rt.predictions.a3 = predict(rt.a3,test.algae)

regr.eval(algae.sols$a3, lm.predictions.a3,train.y = algae[,"a3"])
regr.eval(algae.sols$a3, rt.predictions.a3,train.y = algae[,"a3"])

par(mfrow = c(1, 2), col="navy", bg="lemonchiffon1")
plot(lm.predictions.a3, algae[, "a3"], main = "Linear Model",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
plot(rt.predictions.a3, algae[, "a3"], main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)

#a4
lm.a4 <- lm(a4 ~ ., data = algae[, c(1:11,15)])
final.lm = step(lm.a4)

rt.a4 = rpart(a4 ~ ., data = algae[, c(1:11,15)])

lm.predictions.a4 = predict(final.lm,test.algae)
rt.predictions.a4 = predict(rt.a4,test.algae)

regr.eval(algae.sols$a4, lm.predictions.a4,train.y = algae[,"a4"])
regr.eval(algae.sols$a4, rt.predictions.a4,train.y = algae[,"a4"])

par(mfrow = c(1, 2), col="navy", bg="lemonchiffon1")
plot(lm.predictions.a4, algae[, "a4"], main = "Linear Model",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
plot(rt.predictions.a4, algae[, "a4"], main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)

#a5
lm.a5 <- lm(a5 ~ ., data = algae[, c(1:11,16)])
final.lm = step(lm.a5)

rt.a5 = rpart(a5 ~ ., data = algae[, c(1:11,16)])

lm.predictions.a5 = predict(final.lm,test.algae)
rt.predictions.a5 = predict(rt.a5,test.algae)

regr.eval(algae.sols$a5, lm.predictions.a5,train.y = algae[,"a5"])
regr.eval(algae.sols$a5, rt.predictions.a5,train.y = algae[,"a5"])

par(mfrow = c(1, 2), col="navy", bg="lemonchiffon1")
plot(lm.predictions.a5, algae[, "a5"], main = "Linear Model",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
plot(rt.predictions.a5, algae[, "a5"], main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)

#a6
lm.a6 <- lm(a6 ~ ., data = algae[, c(1:11,17)])
final.lm = step(lm.a6)

rt.a6 = rpart(a6 ~ ., data = algae[, c(1:11,17)])

lm.predictions.a6 = predict(final.lm,test.algae)
rt.predictions.a6 = predict(rt.a6,test.algae)

regr.eval(algae.sols$a6, lm.predictions.a6,train.y = algae[,"a6"])
regr.eval(algae.sols$a6, rt.predictions.a6,train.y = algae[,"a6"])

par(mfrow = c(1, 2), col="navy", bg="lemonchiffon1")
plot(lm.predictions.a6, algae[, "a6"], main = "Linear Model",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
plot(rt.predictions.a6, algae[, "a6"], main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)

#a7
lm.a7 <- lm(a7 ~ ., data = algae[, c(1:11,18)])
final.lm = step(lm.a7)

rt.a7 = rpart(a7 ~ ., data = algae[, c(1:11,18)])

lm.predictions.a7 = predict(final.lm,test.algae)
rt.predictions.a7 = predict(rt.a7,test.algae)

regr.eval(algae.sols$a7, lm.predictions.a7,train.y = algae[,"a7"])
regr.eval(algae.sols$a7, rt.predictions.a7,train.y = algae[,"a7"])

par(mfrow = c(1, 2), col="navy", bg="lemonchiffon1")
plot(lm.predictions.a7, algae[, "a7"], main = "Linear Model",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
plot(rt.predictions.a7, algae[, "a7"], main = "Regression Tree",
     xlab = "Predictions", ylab = "True Values", xlim=c(-15,62))
abline(0, 1, lty = 2)
