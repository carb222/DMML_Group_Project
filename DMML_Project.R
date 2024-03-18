#DMML Group Project

drugs <- read.csv("group_22.csv")
#Libraries
library(ggplot2)
library(GGally)
library(MASS)
library(e1071)  

#Split the data 50% training, 25% validation and 25% test
drugs$Semer <- as.factor(drugs$Semer)
drugs$Amphet <- as.factor(drugs$Amphet)

n <- nrow(drugs)
intrain <- sample(c(1:n),round(n/2))
invalid <- sample((c(1:n)[-intrain]),round(n/4))
train.data <- drugs[intrain,]
valid.data <- drugs[invalid,]
test.data <- drugs[-c(intrain,invalid),]


#Fit the classification SVM for different values of C
#and calculate the validation prediction error

pred.error<-function(pred,truth){
  mean(pred!=truth)
}
C.val <- c(0.1,0.5,1,2,5,10)

######SOMETHING WRONG WITH THIS
#C.error <- numeric(length(C.val))
#for (i in 1:length(C.val)) {
#  model <- svm(Amphet~.,data=train.data,type="C-classification",kernel="linear",
#               cost=C.val[i]) 
# pred.model <- predict(model, valid.data)
#  C.error[i] <- pred.error(pred.model, valid.data$Amphet)
#}
#C.sel <- C.val[min(which.min(C.error))]
#C.sel
#######



#Tuning
tune.linear <- tune.svm(Amphet~.,data=train.data,type="C-classification",kernel="linear",
         cost=C.val)
tune.linear
summary(tune.linear)

final.svm<-svm(Amphet~.,data=train.data,kernel="linear",cost=0.1, type="C-classification")
summary(final.svm)

#Predictions/accuracy for Linear
pred.valid <- predict(tune.linear$best.model, newdata = valid.data)
table(valid.data$Amphet , pred.valid)
pred.error(pred.test,test.data$Amphet)

plot(tune.linear$best.model , train.data)



#POlynomial

tune.poly <- tune.svm( Amphet~.,data=train.data,type="C-classification",kernel="polynomial",
                        cost=C.val)
tune.poly
summary(tune.poly)

#predictions
pred.valid <- predict(tune.poly$best.model, newdata = valid.data)
table(valid.data$Amphet , pred.valid)
pred.error(pred.test,test.data$Amphet)



#Radial

tune.radial <- tune.svm( Amphet~.,data=train.data,type="C-classification",kernel="radial", gamma = c(0.01,0.1,0.5,1),
                      cost=C.val)
summary(tune.radial)

#predictions
pred.valid <- predict(tune.radial$best.model, newdata = valid.data)
table(valid.data$Amphet , pred.valid)
pred.error(pred.test,test.data$Amphet)


summary(tune.radial$best.model)
