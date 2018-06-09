library(ROSE);
library(caret);
rm(list = ls());
given <- read.csv("formed_one.csv", header = TRUE);
heading <- c("Serial","Diagnosis","radius_mean","texture_mean","perimeter_mean","area_mean","smoothness_mean","compactness_mean","concavity_mean","points_mean","symmetry_mean","dimension_mean","radius_se","texture_se","perimeter_se","area_se","smoothness_se","compactness_se","concavity_se","points_se","symmetry_se","dimension_se","radius_worst","texture_worst","perimeter_worst","area_worst","smoothness_worst","compactness_worst","concavity_worst","points_worst","symmetry_worst","dimension_worst", "junk")
names(given) <- heading;
given <- given[-1]; # remove the column with serial numbers
given <- given[-32]; # column with missing values
given$Diagnosis <- as.factor(given$Diagnosis);

# print(summary(given));

# barplot(table(given$Diagnosis)/nrow(given), col = rainbow(2), main = "Class Distribution of Original");

set.seed(12345);

# Partition the data
intrain <- createDataPartition(given$Diagnosis, p = 0.7, list = FALSE);
training <- given[intrain, ];
testing <- given[-intrain, ];

print(table(training$Diagnosis));
print(table(training$Diagnosis)/nrow(training));

mdl <- train(Diagnosis ~ ., data = training, method = "rf", trControl = trainControl(method = "repeatedcv", number = 5));

predictions <- predict(mdl, newdata = testing);

print(confusionMatrix(predictions, testing$Diagnosis, positive = 'M'));
print("********************************************************************");

# Balancing the dataset

# Random Oversampling
set.seed(45678);
over_sampled <- ovun.sample (Diagnosis ~ ., data = training, method = "over", N = 1500)$data
mdl_over <- train(Diagnosis ~ ., data = over_sampled, method = "rf", trControl = trainControl(method = "repeatedcv", number = 5));

pred_over <- predict(mdl_over, newdata = testing);

# barplot(table(over_sampled$Diagnosis)/nrow(over_sampled), col = rainbow(2), main = "Class Distribution of Over-Sampled");

print(table(over_sampled$Diagnosis));
print(table(over_sampled$Diagnosis)/nrow(over_sampled));
print(confusionMatrix(pred_over, testing$Diagnosis, positive = "M"));
print("********************************************************************");

# Under-sampling
set.seed(23456);
under_sampled <- ovun.sample (Diagnosis ~ ., data = training, method = "under", N = 600)$data
mdl_under <- train(Diagnosis ~ ., data = under_sampled, method = "rf", trControl = trainControl(method = "repeatedcv", number = 5));

pred_under <- predict(mdl_under, newdata = testing);

# barplot(table(under_sampled$Diagnosis)/nrow(under_sampled), col = rainbow(2), main = "Class Distribution of Under-Sampled");

print(table(under_sampled$Diagnosis));
print(table(under_sampled$Diagnosis)/nrow(under_sampled));
print(confusionMatrix(pred_under, testing$Diagnosis, positive = "M"));
print("********************************************************************");

# oversampling, and then removing some instances to counter over-fitting
set.seed(34567);
sampled <- ovun.sample (Diagnosis ~ ., data = training, method = "both", N = nrow(training))$data
mdl_both <- train(Diagnosis ~ ., data = sampled, method = "rf", trControl = trainControl(method = "repeatedcv", number = 5));

pred_both <- predict(mdl_both, newdata = testing);

# barplot(table(sampled$Diagnosis)/nrow(sampled), col = rainbow(2), main = "Class Distribution of Modified-Sampled");

print(table(sampled$Diagnosis));
print(table(sampled$Diagnosis)/nrow(sampled));
print(confusionMatrix(pred_both, testing$Diagnosis, positive = "M"));
print("********************************************************************");

# SMOTE
set.seed(48451);
smote_sampled <- DMwR::SMOTE(Diagnosis ~ ., data = training, perc.over = 300, perc.under = 225, k = 10)
mdl_smote <- train(Diagnosis ~ ., data = smote_sampled, method = "rf", trControl = trainControl(method = "repeatedcv", number = 5));

pred_smote <- predict(mdl_smote, newdata = testing);

# barplot(table(sampled$Diagnosis)/nrow(sampled), col = rainbow(2), main = "Class Distribution of Modified-Sampled");

print(table(smote_sampled$Diagnosis));
print(table(smote_sampled$Diagnosis)/nrow(smote_sampled));
print(confusionMatrix(pred_smote, testing$Diagnosis, positive = "M"));
print("********************************************************************");

# Printing the ROC, and the Area under the Curve
roc.curve(testing$Diagnosis, predictions, col = "black", main = "ROC Curve of the analysis")
roc.curve(testing$Diagnosis, pred_over, col = "red", add.roc = T)
roc.curve(testing$Diagnosis, pred_under, col = "green", add.roc = T)
roc.curve(testing$Diagnosis, pred_both, col = "cyan", add.roc = T)
roc.curve(testing$Diagnosis, pred_smote, col = "darkblue", add.roc = T)