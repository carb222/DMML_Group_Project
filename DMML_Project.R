# DMML Group Project ----

#__________________________________________________________

set.seed(555)

## Libraries ----
library(ggplot2) 
library(GGally)
library(skimr)
library(MASS)
library(e1071)
library(caret)
library(fastDummies)
library(class)
library(tidyr)
library(dplyr)

#__________________________________________________________

## Building the data set ----

### Read data ----
drug <- read.csv("group_22.csv")

### Data tidying ----
#Eliminate over-claimers
drug <- drug %>% filter(Semer=="CL0")

#Since these variables do not have an inherent hierarchy 
#convert Ethnicity and Country columns to dummies
drug$Country <- ifelse(drug$Country==-0.09765,"Australia",
                       ifelse(drug$Country==0.24923,"Canada",
                              ifelse(drug$Country==-0.46841,"New_Zealand",
                                     ifelse(drug$Country==-0.28519,"Other",
                                            ifelse(drug$Country==0.21128,"ROI",
                                                   ifelse(drug$Country==0.96082,"UK",
                                                          ifelse(drug$Country==-0.57009,"USA","Unknown")))))))
levels(factor(drug$Country))

drug$Ethnicity <- ifelse(drug$Ethnicity==-0.50212,"Asian",
                         ifelse(drug$Ethnicity==-1.10702,"Black",
                                ifelse(drug$Ethnicity==1.90725,"Mixed_Black_Asian",
                                       ifelse(drug$Ethnicity==0.12600,"Mixed_White_Asian",
                                              ifelse(drug$Ethnicity==-0.22166,"Mixed_White_Black",
                                                     ifelse(drug$Ethnicity==0.11440,"Other",
                                                            ifelse(drug$Ethnicity==-0.31685,"White","Unknown")))))))

levels(factor(drug$Ethnicity))

drug <- dummy_cols(drug,c("Country", "Ethnicity"), remove_selected_columns=TRUE)


#target variable to factor
drug$Merged_Amphet <- ifelse(drug$Amphet %in% c("CL0"), "Never Used",
                             ifelse(drug$Amphet %in% c("CL1", "CL2"), "Used Over a Year Ago",
                                    "Used in the Last Year"))
drug$Amphet <- as.factor(drug$Amphet)
drug$Merged_Amphet <- as.factor(drug$Merged_Amphet)

#remove ID and fake drug 
cols <- c("ID", "Semer")
drug_numeric <- drug %>% select(-one_of(cols)) %>% 
  relocate(Amphet, .before=Merged_Amphet)


#__________________________________________________________

## Split into training and testing ----
training_n <- floor(0.8*nrow(drug))
training_indices <- sample(c(1:nrow(drug)), training_n)

train <- drug_numeric[training_indices, ]
test <- drug_numeric[-training_indices, ]
#__________________________________________________________



## LDA Method----
### Creating Model ----
data.lda <- lda(Merged_Amphet~. , data=train[,-25])
data.pred.LDA <- predict(data.lda, test[,-25])
dataset <- data.frame(Type=test$Merged_Amphet, lda=data.pred.LDA$x)

### Plots of LDAs----
#Density plots
ggplot(dataset, aes(x=lda.LD1)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)
ggplot(dataset, aes(x=lda.LD2)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)
#LDA1 vs LDA2 plot
ggplot(dataset, aes(x=lda.LD1, y=lda.LD2)) + 
  geom_point(aes(group=Type, colour=Type, shape=Type))

### LDA Prediction Rate----
# Create confusion matrix
LDA_conf_matrix <- confusionMatrix(data.pred.LDA$class, test$Merged_Amphet)
# Extract accuracy
LDA_accuracy <- LDA_conf_matrix$overall["Accuracy"]
cat("SVM Accuracy:", LDA_accuracy, "\n")


#__________________________________________________________

## SVM Method ----
### Parameter tunning ----
#Create possible parameters
cost_range <- c(0.1,1,10,12,100)
degree_range <- 1:5
gamma_range <- c(0.001,0.01,0.015,0.1,1)
# Tune Polynomial SVM
SVM_poly <- tune.svm(Merged_Amphet~., data=train[,-25], type="C-classification", kernel="polynomial", cost=cost_range, degree=degree_range)
summary(SVM_poly)
#Tune Radial SVM
SVM_RBF <- tune.svm(Merged_Amphet~., data=train[,-25], type="C-classification", kernel="radial", cost=cost_range, gamma=gamma_range)
summary(SVM_RBF)
#Best parameters
SVM_poly$best.parameters
SVM_RBF$best.parameters
gamma <- SVM_RBF$best.parameters[, "gamma"]
cost <- SVM_RBF$best.parameters[, "cost"]
#RBF showed less error

###Creating Model----
SVM_final <- svm(Merged_Amphet~., data=train[,-25], type="C-classification", kernel="radial", cost=cost, gamma_range=gamma)
test.pred <- predict(SVM_final,test[,-25])

### SVM Prediction Rate----
# Create confusion matrix
SVM_conf_matrix <- confusionMatrix(test.pred, test$Merged_Amphet)
# Extract accuracy
SVM_accuracy <- SVM_conf_matrix$overall["Accuracy"]
cat("SVM Accuracy:", SVM_accuracy, "\n")

#__________________________________________________________


## kNN Method ----
### Leave-One-Out Cross-Validation on 7 Classes----
set.seed(555)
K <- c(1:20)
cv.corr <- c()
for (k in K){
  train.pred <- knn.cv(train[, 1:24], train[, 25], k = k)
  cv.corr[k] <- mean(train[, 25] == train.pred)
}
plot(K, cv.corr, type = "b", ylab = "Leave-One-Out Cross-Validation CCR")
abline(v = which.max(cv.corr), lty = 2, col = "blue")

### Fitting 15-NN model----
k.opt <- which.max(cv.corr)
test.pred <- knn(train[, 1:24], test[, 1:24], train[, 25], k = k.opt)
# Test CCR
mean(test[, 25] == test.pred)


### Leave-One-Out Cross Validation on 3 Classes----
set.seed(555)
K <- c(1:15)
cv.corr <- c()
for (k in K){
  train.pred <- knn.cv(train[, 1:24], train[, 26], k = k)
  cv.corr[k] <- mean(train[, 26] == train.pred)
}
plot(K, cv.corr, type = "b", ylab = "Leave-One-Out Cross-Validation CCR")
abline(v = which.max(cv.corr), lty = 2, col = "blue")

### Fitting 5-NN model----
k.opt <- which.max(cv.corr)
test.pred <- knn(train[, 1:24], test[, 1:24], train[, 26], k = k.opt)

### KNN Prediction Rate----
# Create confusion matrix
KNN_conf_matrix <- confusionMatrix(test.pred, test$Merged_Amphet)
# Extract accuracy
KNN_accuracy <- KNN_conf_matrix$overall["Accuracy"]
cat("SVM Accuracy:", SVM_accuracy, "\n")


## Model Comparison----


