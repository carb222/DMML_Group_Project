#DMML Group Project
set.seed(555)
####################################
# Building the data set
####################################
library(fastDummies)
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
cols <- c("ID", "Semer")
drug_numeric <- drug %>% select(-one_of(cols)) %>% 
  relocate(Amphet, .before=Merged_Amphet)

train <- drug_numeric[training_indices, ]
test <- drug_numeric[-training_indices, ]


library(class)

# Leave-One-Out Cross-Validation on 7 Classes
set.seed(555)
K <- c(1:20)
cv.corr <- c()
for (k in K){
  train.pred <- knn.cv(train[, 1:24], train[, 25], k = k)
  cv.corr[k] <- mean(train[, 25] == train.pred)
}

plot(K, cv.corr, type = "b", ylab = "Leave-One-Out Cross-Validation CCR")
abline(v = which.max(cv.corr), lty = 2, col = "blue")

# Fitting 15-NN model
k.opt <- which.max(cv.corr)
test.pred <- knn(train[, 1:24], test[, 1:24], train[, 25], k = k.opt)
# Test CCR
mean(test[, 25] == test.pred)


# Leave-One-Out Cross Validation on 3 Classes
set.seed(555)
K <- c(1:15)
cv.corr <- c()
for (k in K){
  train.pred <- knn.cv(train[, 1:24], train[, 26], k = k)
  cv.corr[k] <- mean(train[, 26] == train.pred)
}

plot(K, cv.corr, type = "b", ylab = "Leave-One-Out Cross-Validation CCR")
abline(v = which.max(cv.corr), lty = 2, col = "blue")

# Fitting 5-NN model
k.opt <- which.max(cv.corr)
test.pred <- knn(train[, 1:24], test[, 1:24], train[, 26], k = k.opt)
mean(test[, 26] == test.pred)



