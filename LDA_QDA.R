#Libraries
library(ggplot2)
library(GGally)
library(skimr)
library(MASS)

data = read.csv("group_22.csv")
data = data[, -1]
data = data[, -13]
head(data)
skim(data)
# Checking normality
ggpairs(data, columns=2:13, ggplot2::aes(colour=Amphet, alpha=0.2))

# Checking if the groups have equal covariance
for (i in 1:12) {
  cat(colnames(data)[i],sep="\n")
  print(aggregate(data[,i],by=list(data$Amphet),var)) #compute variance for each group
}

#LDA
set.seed(1)
n <- nrow(data)
ind <- sample(c(1:n), floor(0.8*n))
data.train <- data[ind,]
data.test  <- data[-ind,]

data.lda <- lda(Amphet~., data=data)
data.pred.tr <- predict(data.lda, data.train)
dataset <- data.frame(Type=data.train$Amphet, lda=data.pred.tr$x)

#Density plots of LDAs
ggplot(dataset, aes(x=lda.LD1)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)
ggplot(dataset, aes(x=lda.LD2)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)
ggplot(dataset, aes(x=lda.LD3)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)
ggplot(dataset, aes(x=lda.LD4)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)
ggplot(dataset, aes(x=lda.LD5)) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)
ggplot(dataset, aes(x=lda.LD6  )) + 
  geom_density(aes(group=Type, colour=Type, fill=Type), alpha=0.3)

## LDA1 vs LDA2
ggplot(dataset, aes(x=lda.LD1, y=lda.LD2)) + 
  geom_point(aes(group=Type, colour=Type, shape=Type))
## LDA Prediction rate
mean(data.test$Amphet == data.pred.tr$class)


##QDA
drugs.qda <- qda(Amphet~., data=data.train)
drugs.pred.te2 <- predict(drugs.qda, data.test)
## QDA Prediction rate
mean(data.test$Amphet == drugs.pred.te2$class)

