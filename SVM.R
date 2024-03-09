#Libraries
library(ggplot2)
library(GGally)
library(skimr)
library(MASS)
library(e1071)

data = read.csv("group_22.csv")
data = data[, -1]
data = data[, -13]
data$Amphet <- as.factor(data$Amphet)
data$Age    <- as.factor(data$Age)
data$Gender  <- as.factor(data$Gender)
data$Education <- as.factor(data$Education)
data$Country  <- as.factor(data$Country)
data$Ethnicity   <- as.factor(data$Ethnicity)
head(data)
skim(data)

#SVM Build
set.seed(1)
n <- nrow(data)
train.idx <- sample(n, round(n*0.75))
train <- data[train.idx,]
test <- data[-train.idx,]

cost_range <- c(0.01,0.1,1,10,100)
degree_range <- 1:8
gamma_range <- c(0.001,0.01,0.1,1,10,100)
# Polynomial SVM
SVM_poly <- tune.svm(Amphet~., data=train, type="C-classification", kernel="polynomial", cost=cost_range, degree=degree_range)
summary(SVM_poly)

#Radial SVM
SVM_RBF <- tune.svm(Amphet~., data=train, type="C-classification", kernel="radial", cost=cost_range, gamma=gamma_range)
summary(SVM_RBF)


#Best parameters
SVM_poly$best.parameters
SVM_RBF$best.parameters

#Prediction
gamma.opt <- SVM_RBF$best.parameters[1]
cost.opt <- SVM_RBF$best.parameters[2]
SVM_final <- svm(Amphet~., data, type="C-classification", kernel="radial", gamma=gamma.opt, cost=cost.opt)
test.pred <- predict(SVM_final,test)
table(test$Amphet,test.pred)
