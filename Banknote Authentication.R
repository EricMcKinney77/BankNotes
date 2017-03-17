# STAT5600 Project
# Banknote Authentication Data
# Eric McKinney

library(randomForest)
library(verification)
library(ada)
library(gbm)
library(rpart)
library(rpart.plot)
library(MASS)
library(caret)
library(knitr)
library(e1071)
library(utils)
source("../kappa and classsum.R")
library(corrplot)

notes = read.csv("C:/Users/Eric/Documents/data_banknote_authentication.txt", header = FALSE)
names(notes) = c("WaveVar", "WaveSkew", "WaveKurt", "Entropy", "Counterfeit")

pairs(Counterfeit ~ ., data = notes, main = "Scatterplot Matrix")
plot(notes[, c(1:4)], pch = ifelse(notes$Counterfeit == 1, 2, 1), col = ifelse(notes$Counterfeit == 1, rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

plot(notes$WaveVar, notes$WaveSkew, pch = ifelse(notes$Counterfeit == 1, 2, 1), col = ifelse(notes$Counterfeit == 1, rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
legend("bottomright", pch = c(2, 1), col = c(rgb(1,0,0), rgb(0,0,1)), c("Counterfeit", "Genuine"), cex = .8)
plot(notes$WaveVar, notes$WaveKurt, pch = ifelse(notes$Counterfeit == 1, 2, 1), col = ifelse(notes$Counterfeit == 1, rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
plot(notes$WaveVar, notes$Entropy, pch = ifelse(notes$Counterfeit == 1, 2, 1), col = ifelse(notes$Counterfeit == 1, rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
plot(notes$WaveSkew, notes$WaveKurt, pch = ifelse(notes$Counterfeit == 1, 2, 1), col = ifelse(notes$Counterfeit == 1, rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
plot(notes$WaveSkew, notes$Entropy, pch = ifelse(notes$Counterfeit == 1, 2, 1), col = ifelse(notes$Counterfeit == 1, rgb(1,0,0,0.5), rgb(0,0,1,0.5)))
plot(notes$WaveKurt, notes$Entropy, pch = ifelse(notes$Counterfeit == 1, 2, 1), col = ifelse(notes$Counterfeit == 1, rgb(1,0,0,0.5), rgb(0,0,1,0.5)))

notes$WaveVar[notes$Counterfeit == 1]

par(mfrow = c(2, 2))
hist(notes$WaveVar[notes$Counterfeit == 1], col = rgb(1,0,0,0.5), xlim = c(-8, 8), ylim = c(0,170), main = "Counterfeit vs. Genuine WaveVar", xlab = "Wavelength Variance")
hist(notes$WaveVar[notes$Counterfeit == 0], col = rgb(0,0,1,0.5), add = TRUE)
box()
legend("topleft", fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5), rgb(0.5,0,0.5, 0.8)), leg = c("Counterfeit", "Genuine", "Overlap"), cex = .8)

hist(notes$WaveSkew[notes$Counterfeit == 1], col = rgb(1,0,0,0.5), xlim = c(-15,15), ylim = c(0,150), main = "Counterfeit vs. Genuine WaveSkew", xlab = "Wavelength Variance")
hist(notes$WaveSkew[notes$Counterfeit == 0], col = rgb(0,0,1,0.5), add = TRUE)
box()
legend("topleft", fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5), rgb(0.5,0,0.5, 0.8)), leg = c("Counterfeit", "Genuine", "Overlap"), cex = .8)

hist(notes$WaveKurt[notes$Counterfeit == 1], col = rgb(1,0,0,0.5), xlim = c(-7,20), ylim = c(0,150), main = "Counterfeit vs. Genuine WaveKurt", xlab = "Wavelength Variance")
hist(notes$WaveKurt[notes$Counterfeit == 0], col = rgb(0,0,1,0.5), add = TRUE)
box()
legend("topright", fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5), rgb(0.5,0,0.5, 0.8)), leg = c("Counterfeit", "Genuine", "Overlap"), cex = .8)

hist(notes$Entropy[notes$Counterfeit == 1], col = rgb(1,0,0,0.5), xlim = c(-9,3), ylim = c(0,200), main = "Counterfeit vs. Genuine Entropy", xlab = "Wavelength Variance")
hist(notes$Entropy[notes$Counterfeit == 0], col = rgb(0,0,1,0.5), add = TRUE)
box()
legend("topleft", fill = c(rgb(1,0,0,0.5), rgb(0,0,1,0.5), rgb(0.5,0,0.5, 0.8)), leg = c("Counterfeit", "Genuine", "Overlap"), cex = .8)



cov(notes[, 1:4])
cor(notes[, 1:4])

corrplot(cor(notes[, 1:4]), method = "number")

# # PC Analysis
# notesPC = princomp(notes[, 1:4], cor = TRUE)
# kable(notesPC$loadings)
# 
# screeplot(notesPC, npcs = 4, type = "l", main = "Scree plot for PC on notes data")
# notesPC$loadings
# # I don't think PC is necessary since we only have 4 predictors.



### LDA
Counterfeit.lda = lda(Counterfeit ~ ., data = notes)

table(notes$Counterfeit, predict(Counterfeit.lda)$class)
class.sum(notes$Counterfeit, predict(Counterfeit.lda)$posterior[,2])

Counterfeit.lda.xval = rep(0, nrow(notes))
xvs = rep(1:10, length = nrow(notes))
xvs = sample(xvs)
for (i in 1:10) {
  test = notes[xvs == i,]
  train = notes[xvs != i,]
  glub = lda(Counterfeit ~ ., data = train)
  Counterfeit.lda.xval[xvs == i] = predict(glub, test)$class
}

table(notes$Counterfeit, Counterfeit.lda.xval)
class.sum(notes$Counterfeit, Counterfeit.lda.xval)

## QDA
Counterfeit.qda = qda(Counterfeit ~ ., data = notes)

table(notes$Counterfeit, predict(Counterfeit.qda)$class)
class.sum(notes$Counterfeit, predict(Counterfeit.qda)$posterior[, 2])

Counterfeit.qda.xval = rep(0, nrow(notes))
xvs = rep(1:10, length = nrow(notes))
xvs = sample(xvs)
for (i in 1:10) {
  test = notes[xvs == i, ]
  train = notes[xvs != i, ]
  glub = qda(Counterfeit ~ ., data = train)
  Counterfeit.qda.xval[xvs == i] = predict(glub, test)$class
}

table(notes$Counterfeit,Counterfeit.qda.xval)
class.sum(notes$Counterfeit, Counterfeit.qda.xval)


### Logistic Regression
Counterfeit.lr = glm(Counterfeit ~ ., family = binomial, data = notes)

table(notes$Counterfeit, round(predict(Counterfeit.lr, type = "response") + 0.0000001))
class.sum(notes$Counterfeit, predict(Counterfeit.lr, type = "response"))

Counterfeit.lr.xval = rep(0, length = nrow(notes))
xvs = rep(1:10, length = nrow(notes))
xvs = sample(xvs)
for (i in 1:10) {
  test = notes[xvs == i, ]
  train = notes[xvs != i, ]
  glub = glm(Counterfeit ~ ., family = binomial, data = train)
  Counterfeit.lr.xval[xvs == i] = predict(glub, test, type="response")
}

table(notes$Counterfeit, round(Counterfeit.lr.xval))
class.sum(notes$Counterfeit, Counterfeit.lr.xval)



# Nearest neighbor

# I ran knn3 for k = 2, 3, 4, 5, 6, 7, 8, and 9 nearest neighbors.
# Several k's give us a perfect 10-fold cross validated classification.
nNeighbors = 5
Counterfeit.knn = knn3(Counterfeit ~ ., data = notes, k = nNeighbors)

# Resubstitution confusion matrix and accuracies.
table(notes$Counterfeit, round(predict(Counterfeit.knn, notes, type = "prob")[, 2]))
class.sum(notes$Counterfeit, predict(Counterfeit.knn, notes, type = "prob")[, 2])

Counterfeit.knn.xval = rep(0, length = nrow(notes))
xvs = rep(c(1:10), length = nrow(notes))
xvs = sample(xvs)
for (i in 1:10) {
  train = notes[xvs != i, ]
  test = notes[xvs == i, ]
  glub = knn3(Counterfeit ~ ., data = train, k = nNeighbors)
  Counterfeit.knn.xval[xvs == i] = predict(glub, test, type = "prob")[, 2]
}

table(notes$Counterfeit, round(Counterfeit.knn.xval))
class.sum(notes$Counterfeit, Counterfeit.knn.xval)



# CART
Counterfeit.rpartfull = rpart(Counterfeit ~ ., method = "class", control = rpart.control(cp = 0.0, minsplit = 2), data = notes)
# par(mfcol = c(3, 1))
plot(Counterfeit.rpartfull)
plotcp(Counterfeit.rpartfull)

Counterfeit.rpartCP0023 = rpart(Counterfeit ~ ., method = "class", control=rpart.control(cp=0.0023), data = notes)
prp(Counterfeit.rpartCP0023, Margin = 0.1, varlen = 0, extra = 1)
# Counterfeit.rpartCP0023

table(notes$Counterfeit, predict(Counterfeit.rpartCP0023, type = "class"))
class.sum(notes$Counterfeit, predict(Counterfeit.rpartCP0023, type = "prob")[, 2])

Counterfeit.rpartCP0023.xval = rep(0, length(nrow(notes)))
xvs = rep(c(1:10), length = nrow(notes))
xvs = sample(xvs)
for(i in 1:10) {
  train = notes[xvs != i, ]
  test = notes[xvs == i, ]
  rp = rpart(Counterfeit ~ ., method = "class", data = train, control = rpart.control(cp=0.0023))
  Counterfeit.rpartCP0023.xval[xvs == i] = predict(rp, test, type = "prob")[, 2]
}

table(notes$Counterfeit, round(Counterfeit.rpartCP0023.xval))
class.sum(notes$Counterfeit, Counterfeit.rpartCP0023.xval)



### Random Forest
Counterfeit.rf = randomForest(as.factor(Counterfeit) ~ ., data = notes)

Counterfeit.rf$confusion
class.sum(notes$Counterfeit, predict(Counterfeit.rf, type = "prob")[, 2])

Counterfeit.rf.xval.class = rep(0,length = nrow(notes))
Counterfeit.rf.xval.prob = rep(0,length = nrow(notes))
xvs = rep(1:10, length = nrow(notes))
xvs = sample(xvs)
for(i in 1:10){
    train = notes[xvs != i, ]
    test = notes[xvs == i, ]
    glub = randomForest(as.factor(Counterfeit) ~ ., data = train)
    Counterfeit.rf.xval.class[xvs == i] = predict(glub, test, type = "response")
    Counterfeit.rf.xval.prob[xvs==i] = predict(glub, test, type = "prob")[, 2]
}

table(notes$Counterfeit, Counterfeit.rf.xval.class)
class.sum(notes$Counterfeit, Counterfeit.rf.xval.prob)


Counterfeit.rf = randomForest(as.factor(Counterfeit) ~ ., importance = TRUE, keep.forest = TRUE, data = notes)
varImpPlot(Counterfeit.rf)
par(mfrow = c(4,2))
partialPlot(Counterfeit.rf, notes, Entropy, which.class = 0, main = "Genuine")
partialPlot(Counterfeit.rf, notes, Entropy, which.class = 1, main = "Counterfeit")
partialPlot(Counterfeit.rf, notes, WaveVar, which.class = 0, main = "Genuine")
partialPlot(Counterfeit.rf, notes, WaveVar, which.class = 1, main = "Counterfeit")
partialPlot(Counterfeit.rf, notes, WaveSkew, which.class = 0, main = "Genuine")
partialPlot(Counterfeit.rf, notes, WaveSkew, which.class = 1, main = "Counterfeit")
partialPlot(Counterfeit.rf, notes, WaveKurt, which.class = 0, main = "Genuine")
partialPlot(Counterfeit.rf, notes, WaveKurt, which.class = 1, main = "Counterfeit")



### Adaboost
Counterfeit.ada = ada(Counterfeit ~ ., data = notes, loss = "exponential")

Counterfeit.ada.xval.class = rep(0, length = nrow(notes))
Counterfeit.ada.xval.prob = rep(0, length = nrow(notes))
xvs = rep(1:10, length = nrow(notes))
xvs = sample(xvs)
for (i in 1:10) {
    train = notes[xvs != i, ]
    test = notes[xvs == i, ]
    glub = ada(as.factor(Counterfeit) ~ ., data = train, loss = "exponential")
    Counterfeit.ada.xval.class[xvs == i] = predict(glub, test, type = "vector")
    Counterfeit.ada.xval.prob[xvs == i] = predict(glub, test, type = "probs")[, 2]
}

table(notes$Counterfeit, Counterfeit.ada.xval.class)
class.sum(notes$Counterfeit, Counterfeit.ada.xval.prob)
# adavx = as.numeric(class.sum(notes$Counterfeit, Counterfeit.ada.xval.prob)[1, 2])


### Support Vector Machines
Counterfeit.svm = svm(as.factor(Counterfeit) ~ ., probability = TRUE, data = notes)

Counterfeit.svm.resubpred = predict(Counterfeit.svm, notes, probability = TRUE)
table(notes$Counterfeit, round(attr(Counterfeit.svm.resubpred, "probabilities")[, 2]))
class.sum(notes$Counterfeit, attr(Counterfeit.svm.resubpred, "probabilities")[, 2])

Counterfeit.svm.xvalpred = rep(0, nrow(notes))
xvs = rep(1:10, length = nrow(notes))
xvs = sample(xvs)
for (i in 1:10) {
      train = notes[xvs != i, ]
      test = notes[xvs == i, ]
      glub = svm(as.factor(Counterfeit) ~ ., probability = TRUE, data = train)
      Counterfeit.svm.xvalpred[xvs == i] = attr(predict(glub, test, probability = TRUE), "probabilities")[, 2]
}

table(notes$Counterfeit, round(Counterfeit.svm.xvalpred))
class.sum(notes$Counterfeit, Counterfeit.svm.xvalpred)
# svmvx = as.numeric(class.sum(notes$Counterfeit, Counterfeit.svm.xvalpred)[1, 2])


### Gradient Boosted Machines
Counterfeit.gbm = gbm(Counterfeit ~ ., distribution = "bernoulli", n.trees = 100, interaction.depth = 1, shrinkage = 0.01, data = notes)

table(notes$Counterfeit, round(predict(Counterfeit.gbm, type = "response", n.trees = 100, interaction.depth = 1, shrinkage = 0.01) + 0.0000001))
class.sum(notes$Counterfeit, predict(Counterfeit.gbm, type = "response", n.trees = 100, interaction.depth = 1, shrinkage = 0.01))

Counterfeit.gbm.xvalpr = rep(0, nrow(notes))
xvs = rep(1:10, length = nrow(notes))
xvs = sample(xvs)
for (i in 1:10) {
      train = notes[xvs != i, ]
      test = notes[xvs == i, ]
      glub = gbm(Counterfeit ~ ., distribution = "bernoulli", n.trees = 100, interaction.depth = 1, shrinkage = 0.01, data = train)
      Counterfeit.gbm.xvalpr[xvs == i] = predict(glub, newdata = test, type = "response", n.trees = 100, interaction.depth = 1, shrinkage = 0.01)
}

table(notes$Counterfeit, round(Counterfeit.gbm.xvalpr + 0.0000001))
class.sum(notes$Counterfeit, Counterfeit.gbm.xvalpr)
# gbmux = as.numeric(class.sum(notes$Counterfeit, Counterfeit.gbm.xvalpr)[1, 2])

# Tuning the gbm
fitControl = trainControl(method = "cv", number = 10)
gbmGridNotes = expand.grid(interaction.depth = c(12, 14, 16, 18, 20), n.trees = c(25, 50, 75, 100), shrinkage = c(0.01, 0.05, 0.1, 0.2), n.minobsinnode = 10)
gbmFitNotes = train(as.factor(Counterfeit) ~ ., method = "gbm", tuneGrid = gbmGridNotes, trControl = fitControl, data = notes)
gbmFitNotes

gbmGridNotes = expand.grid(interaction.depth = c(15, 16, 17), n.trees = c(100, 150, 200, 300), shrinkage = c(0.2, 0.3, 0.5, 0.8), n.minobsinnode = 10)
gbmFitNotes = train(as.factor(Counterfeit) ~ ., method = "gbm", tuneGrid = gbmGridNotes, trControl = fitControl, data = notes)
gbmFitNotes

gbmGridNotes = expand.grid(interaction.depth = c(16), n.trees = c(80, 100, 120), shrinkage = c(0.8, 0.9, 1.2), n.minobsinnode = 10)
gbmFitNotes = train(as.factor(Counterfeit) ~ ., method = "gbm", tuneGrid = gbmGridNotes, trControl = fitControl, data = notes)
gbmFitNotes

gbmGridNotes = expand.grid(interaction.depth = c(16), n.trees = c(110, 120, 130), shrinkage = c(0.75, 0.8, 0.85), n.minobsinnode = 10)
gbmFitNotes = train(as.factor(Counterfeit) ~ ., method = "gbm", tuneGrid = gbmGridNotes, trControl = fitControl, data = notes)
gbmFitNotes

gbmGridNotes = expand.grid(interaction.depth = c(16), n.trees = c(125, 130, 135), shrinkage = c(0.75), n.minobsinnode = 10)
gbmFitNotes = train(as.factor(Counterfeit) ~ ., method = "gbm", tuneGrid = gbmGridNotes, trControl = fitControl, data = notes)
gbmFitNotes

Counterfeit.gbm = gbm(Counterfeit ~ ., distribution = "bernoulli", n.trees = 125, interaction.depth = 16, shrinkage = 0.75, data = notes)

table(notes$Counterfeit, round(predict(Counterfeit.gbm, type = "response", n.trees = 125, interaction.depth = 16, shrinkage = 0.75) + 0.0000001))
class.sum(notes$Counterfeit, predict(Counterfeit.gbm, type = "response", n.trees = 125, interaction.depth = 16, shrinkage = 0.75))

Counterfeit.gbm.xvalpr = rep(0, nrow(notes))
xvs = rep(1:10, length = nrow(notes))
xvs = sample(xvs)
for (i in 1:10) {
  train = notes[xvs != i, ]
  test = notes[xvs == i, ]
  glub = gbm(Counterfeit ~ ., distribution = "bernoulli", n.trees = 125, interaction.depth = 16, shrinkage = 0.75, data = train)
  Counterfeit.gbm.xvalpr[xvs == i] = predict(glub, newdata = test, type = "response", n.trees = 125, interaction.depth = 16, shrinkage = 0.75)
}

table(notes$Counterfeit, round(Counterfeit.gbm.xvalpr + 0.0000001))
class.sum(notes$Counterfeit, Counterfeit.gbm.xvalpr)
# gbmux = as.numeric(class.sum(notes$Counterfeit, Counterfeit.gbm.xvalpr)[1, 2])

### Table of Resubstitution, 10-fold, and test set crossvalidation PCC.
xvalTabl <- matrix(c(lrvx, lrux, partvx, partux, rfvx, rfux, adavx, adaux, svmvx, svmux, gbmvx, gbmux, tgbmvx, tgbmux), ncol = 2, byrow = TRUE)
colnames(xvalTabl) <- c("Resubstitution", "10-fold", "Test data")
rownames(xvalTabl) <- c("LDA", "QDA", "Logistic Regression", "Single Tree", "Random Forests", "Nearest Neighbor", "Adaboosted Trees", "Support Vector Machines", "Gradient Boosted Trees", "Tuned Gradient Boosted Trees")
xvalTabl <- as.table(xvalTabl)
kable(xvalTabl, digits = 2, caption = "Cross Validated PCC's")
# align = "c", 
