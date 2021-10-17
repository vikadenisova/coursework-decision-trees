#THEME: Decision tree.

#TASK: Classify wines on RED and WHITE.

#Part 1. Data-preprocessing.

#import libraries
#install.packages('rpart.plot')
#install.packages('corrplot')
#install.packages('PerformanceAnalytics')
library(dplyr)
library(psych)
library(corrplot)
library("PerformanceAnalytics")



#Read data.
wines <- read.csv('C:/Users/plomo/Downloads/winequalityN1.csv')[,c(1:6)]

str(wines)

head(wines)

#Checking NAs on dataset.
sum(is.na(wines))


#Remove NAs from DF.
wines <- na.omit(wines)

#Check that's right.
sum(is.na(wines))

#Randomize data.
shuffle_index <- sample(1:nrow(wines))
wines <- wines[shuffle_index, ]

chart.Correlation(wines[, 2:length(wines)], histogram=TRUE, pch=20)
#Remove 'quality' from DF.
wines$quality <- NULL

head(wines)

#Making 'type' as factor with two levels.

wines$type <- as.factor(wines$type)


wines$type <- gsub('red', 1, wines$type)

wines$type <- gsub('white', 0, wines$type)

wines$type <- as.numeric(wines$type)

hist(wines$type, xlab = 'Type of wine', main = 'Frequency of wine color', col = c('white', 'red'), breaks = 10, labels = TRUE)

#Splitting data on Train and Test samples.

split_on_train_and_test <- function(data, size = 0.75, train = TRUE) {
  n_row = nrow(data)
  total_row = size * n_row
  train_sample <- c(1 : total_row)
  if (train == TRUE) {
    return (data[train_sample, ])
  } else {
    return (data[-train_sample, ])
  }
}

X_train <- split_on_train_and_test(wines, 0.8, train = TRUE)
X_test <- split_on_train_and_test(wines, 0.8, train = FALSE)

#Check lengths of DFS.
length(X_train[,1])
length(X_test[,1])

#Check randomization of samples.
prop.table(table(X_train$type))

#PART 2. Fitting a classification model.

#Starting to fit a classifier.

fit <- rpart::rpart(type~., data=X_train, method='class')
rpart.plot::rpart.plot(fit, extra=106)

#PART 3. Predict a class of wine.
prediction <- predict(fit, X_test, type='class')

#PART 4. Check score of classifier.
accuracy_matrix <- table(X_test$type, prediction)
print(accuracy_matrix)

accuracy_score <- sum(diag(accuracy_matrix)) / sum(accuracy_matrix)

print(paste('Accuracy score of your decision tree is', ceiling(accuracy_score * 100), '%'))
