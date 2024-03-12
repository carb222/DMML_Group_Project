#DMML Group Project
set.seed(555)
####################################
# Building the data set
####################################
library(fastDummies)

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


#######################
# Neural Network
#######################

library(neuralnet)
library(NeuralNetTools)

#pca since the model  training is too slow 
nn_train <- train ; nn_test <- test
#my.pca <- princomp(nn_train[,-c(25,26)], cor=TRUE) #correlation-based PCA
#summary(my.pca) #the first 18 components contain 90% of the variance

#nn_train <- my.pca$scores[,1:18] %>% as.data.frame() %>% mutate(Merged_Amphet = nn_train$Merged_Amphet)
#nn_test <- predict(my.pca, nn_test[,-c(25,26)])[,1:18] %>% as.data.frame() %>% mutate(Merged_Amphet = nn_test$Merged_Amphet)

#checking correlation of covariates against target
corr <- abs(cor(nn_train[,-c(25)]%>%mutate(Merged_Amphet=as.numeric(as.numeric(nn_train$Merged_Amphet))))[-25,25]) %>% as.data.frame()
colnames(corr) <- c("Abs_Cor")
corr <- cbind(var = rownames(corr), corr)
rownames(corr) <- 1:nrow(corr)
corr %>% arrange(desc(Abs_Cor)) %>% head(10)

# min-max normalisation
maxs <- apply(nn_train[,-c(25,26)], 2, max)
mins <- apply(nn_train[,-c(25,26)], 2, min)
nn_train[,-c(25,26)] <- as.data.frame(scale(nn_train[,-c(25,26)],center = mins, scale = maxs - mins))
nn_test[,-c(25,26)] <- as.data.frame(scale(nn_test[,-c(25,26)],center = mins, scale = maxs - mins))


#train neural network
softplus <- function(x) log(1+exp(x))


#nn=neuralnet(Merged_Amphet~Age+Gender+Education+Nscore+Escore+Oscore+
#               Ascore+Cscore+Impulsive+Impulsive+SS+
#               Country_Australia+Country_Canada+Country_New_Zealand+Country_Other+Country_ROI+Country_UK+Country_USA+
#              Ethnicity_Asian+Ethnicity_Black+Ethnicity_Mixed_Black_Asian+Ethnicity_Mixed_White_Asian+Ethnicity_Mixed_White_Black+Ethnicity_Other+Ethnicity_White,
#             data=nn_train, hidden=c(2,2,2,2,2,2,2,2),rep=10,err.fct = "sse",lifesign="full",act.fct="tanh",linear.output=FALSE)
#SS+Country_UK+Impulsive+Country_USA+Oscore+Gender+Cscore+Education+Ascore+Age+Ethnicity_Black
nn=neuralnet(Merged_Amphet~.,
           data=nn_train[,-25], hidden=c(14),rep=2,err.fct = "ce",stepmax=150000,lifesign="full",act.fct="logistic",linear.output=FALSE)

#Predict and evaluate training and testing accuracy
#Training accuracy
nn_train_pred <-
  nn_train %>% mutate(pred = max.col(predict(nn, nn_train))) %>% 
  mutate(accuracy= pred==as.numeric(nn_train$Merged_Amphet))
mean(nn_train_pred$accuracy)
#Testing accuracy
nn_test_pred <-
  nn_test %>% mutate(pred = max.col(predict(nn, nn_test))) %>% 
  mutate(accuracy= pred==as.numeric(nn_test$Merged_Amphet))
mean(nn_test_pred$accuracy)
#training accuracy around 70%, testing around 57%



