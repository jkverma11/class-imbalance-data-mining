data <- read.csv("~/cancer_project/formed_one.csv",header = TRUE)
data <- data[-1]
str(data)
data <- data[-32]
str(data)
data$M <- as.factor(data$M)
summary(data)

#barplot
barplot(prop.table(table(data$M)), col = rainbow(2), ylim = c(0, 0.7), main = "class distribution")

#Data partition
set.seed(512)
ind <- sample(2,nrow(new_data),replace=TRUE, prob = c(0.70,0.30))
fast_train <- data[ind==1,]
fast_test <- data[ind==2,]

#Data for developing predictive model
table(fast_train$M)
prop.table(table(fast_train$M))
#summary(fast_train)
#row.has.na <- apply(fast_train, 1, function(x){any(is.na(x))})
#predictors_no_NA <- fast_train[!row.has.na,]
#for predictive model we are going to use Randomforest
#install.packages("randomForest")
library(randomForest)
rffast_train <- randomForest(M~., data=fast_train)


#evaluation of predictive model using fast_test data
library(caret)
confusionMatrix(predict(rffast_train,fast_test), fast_test$M, positive='B')


#Oversampling for better prediction
#install.packages("ROSE")
library(ROSE)
over <- ovun.sample(M~., data = fast_train, method = "over", N=1200)$data
table(over$M)
summary(over)

library(randomForest)
rfover <- randomForest(M~., data=over)
confusionMatrix(predict(rfover,fast_test), fast_test$M, positive='B')

#Undersampling for better prediction
#install.packages("ROSE")
library(ROSE)
under <- ovun.sample(M~., data = fast_train, method = "under", N=300)$data
table(under$M)
summary(under)

library(randomForest)
rfunder <- randomForest(M~., data=under)
confusionMatrix(predict(rfunder,fast_test), fast_test$M, positive='B')

#synthetic creation of data values
rose <- ROSE(M~., data = fast_train, N=500, seed = 111)$data
table(rose$M)
summary(rose)