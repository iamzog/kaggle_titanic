setwd("E:/Users/Paul/Desktop/Kaggle Work/kaggle_titanic/")
train <- read.csv("train.csv", stringsAsFactors=FALSE)
test <- read.csv("test.csv")


table(train$Survived)#shows the people that survived and the people that died
# 0   1 
#549 342 
prop.table(table(train$Survived)) #converts to a proportion
#     0         1 
#0.6161616 0.3838384

test$Survived <- rep(0,418) #assumes everyone died
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "theyallperish.csv", row.names = FALSE)

#Look at the death proportaion based on gender
prop.table(table(train$Sex)) #proportion of men and women on board
prop.table(table(train$Sex, train$Survived),1) #does the proportion by rows, if set to 2 this would do the propotion by columns
#We can see that more women survived than men
#            0          1
#female 0.2579618 0.7420382
#male   0.8110919 0.1889081

test$Survived <- 0
test$Survived[test$Sex == "female"] <- 1
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "allfemalesurvive.csv", row.names = FALSE)

#Now lets look for children
summary(train$Age)
train$Child <- 0
train$Child[train$Age < 18] <-1
aggregate(Survived ~ Child + Sex, data=train, FUN=function(x) {sum(x)/length(x)}) #shows how many of the children of each gender survived

#Create the fare2 column for fare categories
train$Fare2 <- '30+'
train$Fare2[train$Fare < 30 & train$Fare >= 20] <- '20-30'
train$Fare2[train$Fare < 20 & train$Fare >= 10] <- '10-20'
train$Fare2[train$Fare < 10] <- '<10'

aggregate(Survived ~ Fare2 + Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)}) #expensive 3rd class ticket holder females died more

test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Sex == 'female' & test$Pclass == 3 & test$Fare >= 20] <- 0
submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "allfemalesurvive.csv", row.names = FALSE)

#Using Decision trees
library(rpart)
#first algorithm
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train, method="class")
plot(fit) #just lines
text(fit) #still bad looking

#install.packages('rattle')
#install.packages('rpart.plot')
#install.packages('RColorBrewer')
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Time to make a better plot
fancyRpartPlot(fit)

#Create a file with predictions from the tree
Prediction <- predict(fit, test, type = "class") #predicts 
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "myfirstdtree.csv", row.names = FALSE)

#go crazy and overfit with the tree
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit=2, cp=0))
fancyRpartPlot(fit)

#now start to trim it down
fit <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data=train,
             method="class", control=rpart.control(minsplit =10, minbucket=5))
fancyRpartPlot(fit)
Prediction <- predict(fit, test, type = "class") #predicts 
submit <- data.frame(PassengerId = test$PassengerId, Survived = Prediction)
write.csv(submit, file = "mytry.csv", row.names = FALSE)