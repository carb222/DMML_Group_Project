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
#Create new class
data$Merged_Amphet <- ifelse(data$Amphet %in% c("CL0"), "Never Used",
                             ifelse(data$Amphet %in% c("CL1", "CL2"), "Used Over a Year Ago",
                                    "Used in the Last Year"))
#Delete Amphet column
data = data[, -13]

#Change variables to factor
data$Merged_Amphet <- as.factor(data$Merged_Amphet)
data$Country  <- as.factor(data$Country)
data$Ethnicity   <- as.factor(data$Ethnicity)

#Explore data
head(data)
skim(data)

#Split Train and Test
set.seed(1)
n <- nrow(data)
ind <- sample(c(1:n), floor(0.8*n))
data.train <- data[ind,]
data.test  <- data[-ind,]

#LDA Method
data.lda <- lda(Merged_Amphet~., data=data.train)
data.pred.LDA <- predict(data.lda, data.test)
dataset <- data.frame(Type=data.train$Merged_Amphet, lda=data.pred.LDA$x)

#Density plots of LDAs
ggplot(dataset, aes(x=lda.LD1)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)
ggplot(dataset, aes(x=lda.LD2)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)

# LDA1 vs LDA2
ggplot(dataset, aes(x=lda.LD1, y=lda.LD2)) + 
  geom_point(aes(group=Type, colour=Type, shape=Type))


# LDA Prediction rate
mean(data.test$Merged_Amphet == data.pred.LDA$class)

####ERROR HERE###
#QDA Method
drugs.qda <- qda(Merged_Amphet~., data=data.train)
drugs.pred.QDA <- predict(drugs.qda, data.test)
# QDA Prediction rate
mean(data.test$Merged_Amphet == drugs.pred.QDA$class)
###----------###