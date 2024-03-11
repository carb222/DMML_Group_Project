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
drug <- drug %>% 
  filter(Semer == "CL0")


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

#remove ID , fake drug and Amphet
cols <- c("ID", "Semer", "Amphet")
drug_numeric <- drug %>% select(-one_of(cols))


train <- drug_numeric[training_indices, ]
test <- drug_numeric[-training_indices, ]



#LDA Method
data.lda <- lda(Merged_Amphet~. , data=train)
data.pred.LDA <- predict(data.lda, test)
dataset <- data.frame(Type=test$Merged_Amphet, lda=data.pred.LDA$x)

#Density plots of LDAs
ggplot(dataset, aes(x=lda.LD1)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)
ggplot(dataset, aes(x=lda.LD2)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)

# LDA1 vs LDA2
ggplot(dataset, aes(x=lda.LD1, y=lda.LD2)) + 
  geom_point(aes(group=Type, colour=Type, shape=Type))

# LDA Prediction rate
mean(test$Merged_Amphet == data.pred.LDA$class)

####ERROR HERE###
#QDA Method
drugs.qda <- qda(Merged_Amphet~., data=train)
drugs.pred.QDA <- predict(drugs.qda, test)
# QDA Prediction rate
mean(test$Merged_Amphet == drugs.pred.QDA$class)
###----------###
