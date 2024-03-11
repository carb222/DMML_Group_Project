#DMML Group Project
set.seed(555)
####################################
# Building the data set
####################################

#Libraries
library(ggplot2)
library(GGally)
library(skimr)
library(MASS)
library(e1071)
library(caret)
library(fastDummies)
library(tidyr)
library(dplyr)

#read data
drug <- read.csv("group_22.csv") 

#eliminate over-claimers
drug <- drug %>% filter(Semer=="CL0")


#since these variables do not have an inherent hierarchy 
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


#split into training and testing
training_n <- floor(0.8*nrow(drug))
training_indices <- sample(c(1:nrow(drug)), training_n)

#remove ID and fake drug 
cols <- c("ID", "Semer", "Amphet")
drug_numeric <- drug[, -which(names(drug) %in% cols)]


train <- drug_numeric[training_indices, ]
test <- drug_numeric[-training_indices, ]


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
SVM_final <- svm(Merged_Amphet~., data=train, type="C-classification", kernel="radial", cost=10, gamma_range=0.01)
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


