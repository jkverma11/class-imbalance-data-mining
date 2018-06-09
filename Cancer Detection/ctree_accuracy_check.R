main_dataset <- read.csv("~/cancer_project/Dataset.csv")
#install.packages("party")
library('party')
View(main_dataset)
#main_dataset <- main_dataset[-1]
set.seed(1001)
ind <- sample(2,nrow(main_dataset),replace=TRUE, prob = c(0.70,0.30))
train.data <- main_dataset[ind==1,]
test.data <- main_dataset[ind==2,]
myf <- M ~. #coloumn names
our_ctree <- ctree(myf, data=train.data)
table(predict(our_ctree), train.data$M)
plot(our_ctree)
testpred <- predict(our_ctree, newdata = test.data)
table(testpred, test.data$M)