#Libraries
library(ggplot2)
library(GGally)
library(skimr)
library(MASS)
library(e1071)
library(tidyr)
library(caret)

#Read data
data = read.csv("group_22.csv")
#Delete ID column
data = data[, -1]
#Delete Semer column
data = data[, -13]
#Create new Classes
data$Merged_Amphet <- ifelse(data$Amphet %in% c("CL0"), "Never Used",
                             ifelse(data$Amphet %in% c("CL1", "CL2"), "Used Over a Year Ago",
                                    "Used in the Last Year"))
#Delete Amphet column
data = data[, -13]

#Change Variables to factor
data$Merged_Amphet <- as.factor(data$Merged_Amphet)
data$Country  <- as.factor(data$Country)
data$Ethnicity   <- as.factor(data$Ethnicity)

#Explore data
head(data)
skim(data)

#Split to Train and Test
set.seed(1)
n <- nrow(data)
train.idx <- sample(n, round(n*0.75))
train <- data[train.idx,]
test <- data[-train.idx,]

#Create possible variables
cost_range <- c(0.001,0.01,0.1,1,10,100)
degree_range <- 1:8
gamma_range <- c(0.001,0.01,0.1,1,10,100)

# Tune Polynomial SVM
SVM_poly <- tune.svm(Merged_Amphet~., data=train, type="C-classification", kernel="polynomial", cost=cost_range, degree=degree_range)
summary(SVM_poly)

#Tune Radial SVM
SVM_RBF <- tune.svm(Merged_Amphet~., data=train, type="C-classification", kernel="radial", cost=cost_range, gamma=gamma_range)
summary(SVM_RBF)


#Best parameters
SVM_poly$best.parameters
SVM_RBF$best.parameters

#Prediction for best model
SVM_final <- svm(Merged_Amphet~., data=train, type="C-classification", kernel="polynomial", cost=10, degree_range=2)
test.pred <- predict(SVM_final,test)

# Check levels of test$Merged_Amphet
levels(test$Merged_Amphet)
# Check levels of test.pred
levels(test.pred)
# Align levels if needed
levels(test.pred) <- levels(test$Merged_Amphet)

# Now try the table() function
result_table <- table(test$Merged_Amphet, test.pred)
print(result_table)

# Create confusion matrix
conf_matrix <- confusionMatrix(test.pred, test$Merged_Amphet)

# Extract accuracy
accuracy <- conf_matrix$overall["Accuracy"]
cat("Accuracy:", accuracy, "\n")


