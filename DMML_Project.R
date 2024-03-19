# DMML Group Project ----

#__________________________________________________________

set.seed(555)

## Libraries ----
library(ggplot2)
library(tidyverse)
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


##Exploratory Analysis----
str(train)
summary(train)

# Provided values and their corresponding age ranges
# Plotting the density plot with customized x-axis labels
age_values <- c(-0.95197, -0.07854, 0.49788, 1.09449, 1.82213, 2.59171)
age_ranges <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
ggplot(train, aes(x = Age, colour = Merged_Amphet)) +
  geom_density() +
  scale_x_continuous(breaks = age_values, labels = age_ranges)


# Provided values and their corresponding education levels
education_values <- c(-2.43591, -1.73790, -1.43719, -1.22751, -0.61113, -0.05921, 0.45468, 1.16365, 1.98437)
education_levels <- c("Left school before 16 years", "Left school at 16 years", "Left school at 17 years",
                      "Left school at 18 years", "Some college or university, no certificate or degree",
                      "Professional certificate/diploma", "University degree", "Masters degree", "Doctorate degree")

# Plotting the density plot with customized x-axis labels
ggplot(train, aes(x = Education, colour = Merged_Amphet)) +
  geom_density() +
  scale_x_continuous(breaks = education_values, labels = education_levels)+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  scale_x_continuous(breaks = age_values, labels = age_ranges) 




# Boxplot of drug usage based on genders and age
age_values <- c(-0.95197, -0.07854, 0.49788, 1.09449, 1.82213, 2.59171)
age_ranges <- c("18-24", "25-34", "35-44", "45-54", "55-64", "65+")
ggplot(train, aes(x = as.factor(Merged_Amphet), y = Age, fill = as.factor(Gender))) +
  geom_boxplot() +
  labs(x = "Drug consumption", y = "Age", fill = "Gender") +
  scale_y_continuous(breaks = age_values, labels = age_ranges) +
 scale_fill_discrete(labels = c("Male" , "Female"))




ggplot(train, aes(x = Merged_Amphet, y = Nscore, fill = Merged_Amphet)) +
  geom_boxplot() +  
  labs(title = "Distribution of Nscore by Merged Amphet",
       x = "Merged Amphet",
       y = "Nscore") +
  theme_minimal()



ggplot(train, aes(x = Merged_Amphet, y = Escore, fill = Merged_Amphet)) +
  geom_boxplot() +  
  labs(title = "Distribution of Nscore by Merged Amphet",
       x = "Merged Amphet",
       y = "Escore") +
  theme_minimal()


ggplot(train, aes(x = Merged_Amphet, y = Oscore, fill = Merged_Amphet)) +
  geom_boxplot() +  
  labs(title = "Distribution of Nscore by Merged Amphet",
       x = "Merged Amphet",
       y = "Oscore") +
  theme_minimal()


ggplot(train, aes(x = Merged_Amphet, y = Ascore, fill = Merged_Amphet)) +
  geom_boxplot() +  
  labs(title = "Distribution of Nscore by Merged Amphet",
       x = "Merged Amphet",
       y = "Ascore") +
  theme_minimal()


ggplot(train, aes(x = Merged_Amphet, y = Ascore, fill = Merged_Amphet)) +
  geom_boxplot() +  
  labs(title = "Distribution of Nscore by Merged Amphet",
       x = "Merged Amphet",
       y = "Ascore") +
  theme_minimal()


ggplot(train, aes(x = Merged_Amphet, y = Cscore, fill = Merged_Amphet)) +
  geom_boxplot() +  
  labs(title = "Distribution of Nscore by Merged Amphet",
       x = "Merged Amphet",
       y = "Cscore") +
  theme_minimal()

ggplot(train, aes(x = Merged_Amphet, y = Impulsive, fill = Merged_Amphet)) +
  geom_boxplot() +  
  labs(title = "Distribution of Nscore by Merged Amphet",
       x = "Merged Amphet",
       y = "Impulsive") +
  theme_minimal()


ggplot(train, aes(x = Merged_Amphet, y = SS, fill = Merged_Amphet)) +
  geom_boxplot() +  
  labs(title = "Distribution of Nscore by Merged Amphet",
       x = "Merged Amphet",
       y = "SS") +
  theme_minimal()


#creating the test data again with normal columns 
drug <- read.csv("group_22.csv") 
drug <- drug %>% filter(Semer=="CL0")
drug$Merged_Amphet <- ifelse(drug$Amphet %in% c("CL0"), "Never Used",
                             ifelse(drug$Amphet %in% c("CL1", "CL2"), "Used Over a Year Ago",
                                    "Used in the Last Year"))

drug$Amphet <- as.factor(drug$Amphet)
drug$Merged_Amphet <- as.factor(drug$Merged_Amphet)


#split into training and testing
training_n <- floor(0.8*nrow(drug))
training_indices <- sample(c(1:nrow(drug)), training_n)

#remove ID and fake drug 
cols <- c("ID", "Semer")
drug_numeric <- drug %>% select(-one_of(cols)) %>% 
  relocate(Amphet, .before=Merged_Amphet)

train2 <- drug_numeric[training_indices, ]
test2 <- drug_numeric[-training_indices, ]



train2$Ethnicity <- factor(train2$Ethnicity)
train2$Merged_Amphet <- factor(train2$Merged_Amphet, levels = c("Never Used", "Used in the Last Year", "Used Over a Year Ago"))

#mapping numeric values to ethnicities
ethnicity_labels <- c("-0.50212" = "Asian",
                      "-1.10702" = "Black",
                      "1.90725" = "Mixed-Black/Asian",
                      "0.12600" = "Mixed-White/Asian",
                      "-0.22166" = "Mixed-White/Black",
                      "0.11440" = "Other",
                      "-0.31685" = "White")

ggplot(train2, aes(x = as.factor(Ethnicity), fill = Merged_Amphet)) +
  geom_bar(position = "fill") +
  scale_x_discrete(labels = function(x) ethnicity_labels[x]) + # Map numeric values to ethnicities
  scale_fill_manual(values = c("Never Used" = "blue", "Used in the Last Year" = "green", "Used Over a Year Ago" = "red")) +
  labs(title = "Amphetamine Usage by Ethnicity", x = "Ethnicity", y = "Proportion of drug usage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

###COUNTRY~PROPORTION
#Mapping numeric values to countries
country_labels <- c("-0.09765" = "Australia",
                    "0.24923" = "Canada",
                    "-0.46841" = "New Zealand",
                    "-0.28519" = "Other",
                    "0.21128" = "Republic of Ireland",
                    "0.96082" = "UK",
                    "-0.57009" = "USA")

ggplot(train2, aes(x = as.factor(Country), fill = Merged_Amphet)) +
  geom_bar(position = "fill") +
  scale_x_discrete(labels = function(x) country_labels[x]) + # Map numeric values to countries
  scale_fill_manual(values = c("Never Used" = "blue", "Used in the Last Year" = "green", "Used Over a Year Ago" = "red")) +
  labs(title = "Amphetamine Usage by Country", x = "Country", y = "Proportion of drug usage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###GENDER PROPORTION
gender_labels <- c("0.48246" = "Female",
                   "-0.48246" = "Male")

ggplot(train2, aes(x = as.factor(Gender), fill = Merged_Amphet)) +
  geom_bar(position = "fill") +
  scale_x_discrete(labels = function(x) gender_labels[x]) + # Map numeric values to genders
  scale_fill_manual(values = c("Never Used" = "blue", "Used in the Last Year" = "green", "Used Over a Year Ago" = "red")) +
  labs(title = "Amphetamine Usage by Gender", x = "Gender", y = "Proportion of drug usage") +
  theme_minimal()

####EDUCATION~PROPORTION
education_labels <- c("-2.43591" = "Left school before 16 years",
                      "-1.73790" = "Left school at 16 years",
                      "-1.43719" = "Left school at 17 years",
                      "-1.22751" = "Left school at 18 years",
                      "-0.61113" = "Some college or university, no certificate or degree",
                      "-0.05921" = "Professional certificate/ diploma",
                      "0.45468" = "University degree",
                      "1.16365" = "Masters degree",
                      "1.98437" = "Doctorate degree")

ggplot(train2, aes(x = as.factor(Education), fill = Merged_Amphet)) +
  geom_bar(position = "fill") +
  scale_x_discrete(labels = function(x) education_labels[x]) + # Map numeric values to education levels
  scale_fill_manual(values = c("Never Used" = "blue", "Used in the Last Year" = "green", "Used Over a Year Ago" = "red")) +
  labs(title = "Amphetamine Usage by Education Level", x = "Education Level", y = "Proportion of drug usage") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


###AGE~PROPORTION
age_labels <- c("-0.95197" = "18-24",
                "-0.07854" = "25-34",
                "0.49788" = "35-44",
                "1.09449" = "45-54",
                "1.82213" = "55-64",
                "2.59171" = "65+")

ggplot(train2, aes(x = as.factor(Age), fill = Merged_Amphet)) +
  geom_bar(position = "fill") +
  scale_x_discrete(labels = function(x) age_labels[x]) + # Map numeric values to age groups
  scale_fill_manual(values = c("Never Used" = "blue", "Used in the Last Year" = "green", "Used Over a Year Ago" = "red")) +
  labs(title = "Amphetamine Usage by Age Group", x = "Age Group", y = "Proportion of drug usage") +
  theme_minimal()

#__________________________________________________________

## LDA Method----
set.seed(555)
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
set.seed(555)
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


## KNN Method ----
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
#

# Specificity Plot

df1 = data.frame(LDA_conf_matrix$byClass)
df1 = tibble::rownames_to_column(df1, var = "Class")
df1$Method = "LDA"
df2 = data.frame(SVM_conf_matrix$byClass)
df2 = tibble::rownames_to_column(df2, var = "Class")
df2$Method = "SVM"
df3 = data.frame(KNN_conf_matrix$byClass)
df3 = tibble::rownames_to_column(df3, var = "Class")
df3$Method = "KNN"

appended_df <- rbind(df1, df2, df3)

ggplot(appended_df, aes(x = Class, y = Sensitivity, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Sensitivity", x = "Class", y = "Values") +
  theme_minimal()

ggplot(appended_df, aes(x = Class, y = Specificity , fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Specificity", x = "Class", y = "Values") +
  theme_minimal()

ggplot(appended_df, aes(x = Class, y = Pos.Pred.Value, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Pos.Pred.Value", x = "Class", y = "Values") +
  theme_minimal()

ggplot(appended_df, aes(x = Class, y = Neg.Pred.Value, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Neg.Pred.Value", x = "Class", y = "Values") +
  theme_minimal()

ggplot(appended_df, aes(x = Class, y = Precision, fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Precision", x = "Class", y = "Values") +
  theme_minimal()

ggplot(appended_df, aes(x = Class, y = F1 , fill = Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "F1", x = "Class", y = "Values") +
  theme_minimal()
