#DMML Group Project
set.seed(555)
####################################
# Building the data set
####################################
library(fastDummies)
library(tidyverse)
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


###EXPLORATORY ANALYSIS
train
str(train)
summary(train)


# Provided values and their corresponding age ranges
library(ggplot2)
library(GGally)
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





