##article on feature selection using caret package: https://machinelearningmastery.com/feature-selection-with-the-caret-r-package/

# Importing the dataset
dataset = read.csv('mushrooms.csv')

#Checking variables
summary(dataset)
str(dataset)
summary(dataset$class)
#all of the variables have categorical variables. And only variable 'veil.type' has only type, meaning it doens't have any predictive power on decising on class type of mashroom

#Removing variable 'veil.type
dataset$veil.type = NULL
names(dataset)


# Splitting the dataset into the Training set and Test set
library(caTools)
set.seed(123)
split = sample.split(dataset$class, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#__________________________________________________________________

#Fitting Decision tree

library(rpart)
classifier_dt = rpart(formula = class ~ .,
                      data = training_set)
y_pred_dt = predict(classifier_dt, newdata = test_set[-1], type = 'class')
library(caret)
confusionMatrix(test_set$class, y_pred_dt)
library(rpart.plot)
rpart.plot(classifier_dt)

#Visualizing the sorted attributes
ggplot(dataset, aes( x = odor, fill = class)) + facet_wrap(~spore.print.color) +geom_histogram(stat = 'count') +
  ggtitle('Class distribution based on odor and spore.print.color')

#Removing additional attributes from training and test set for further models
training_set = training_set[c('class','odor','spore.print.color')]
test_set = test_set [c('class','odor', 'spore.print.color')]


#________________________________________________________________

# Fitting Logistic Regression to the Training set
classifier_glm = glm(formula = class ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier_glm, type = 'response', newdata = test_set[-1])
y_pred_glm = ifelse(prob_pred > 0.5, 'p', 'e')

# Making the Confusion Matrix
confusionMatrix(test_set$class, y_pred_glm)

#__________________________________________________________________

## Fitting k-nearest neighbors model
library(knncat)
classifier_knn = knncat(training_set, classcol = 1)

y_pred_knn = predict(classifier_knn, training_set, test_set,
                     train.classcol = 1, newdata.classcol = 1)
confusionMatrix(y_pred_knn, test_set$class)


#__________________________________________________________________

#Fitting SVM model
library(e1071)
classifier_svm = svm(formula = class ~ .,
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'linear')
y_pred_svm = predict(classifier_svm, newdata = test_set[-1])
confusionMatrix(test_set$class, y_pred_svm)

#__________________________________________________________________

#Fitting Naive Bayes

classifier_nb = naiveBayes(x = training_set[-1],
                       y = training_set$class)
y_pred_nb = predict(classifier_nb, newdata = test_set[-1])
confusionMatrix(test_set$class, y_pred_nb)


#__________________________________________________________________

#Fitting randomforest
library(randomForest)
classifier_rf = randomForest(x = training_set[-1], y = training_set$class, 
                             data = training_set,ntree = 500)
y_pred_rf = predict(classifier_rf, newdata = test_set[-1])
confusionMatrix(test_set$class, y_pred_rf)

