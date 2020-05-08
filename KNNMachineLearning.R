#Use of k Nearest Neighbour to predict what type of iris is present in the dataset
#This is plotted on bar charts to show the variance between the actual dataset and that predicted by the model

install.packages("class")
library(class)
summary(iris)

#Normalise the dataset

#First build normalize() function]
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
#Code to normalize the dataset
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))
summary(iris_norm)

#Making a training and a test set

#Generate random values
set.seed(1234)

#Assign these values to a sample
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

#Create a training set
iris.training <- iris[ind==1,1:4]
head(iris.training)

#Create a test set
iris.test <- iris[ind==2, 1:4]
head(iris.test)

#Add labels to both
iris.trainingLabels <- iris[ind==1,5]
iris.testLabels <- iris[ind==2,5]
print(iris.testLabels)
print(iris.trainingLabels)

#Building the model:
iris_prediction_model <- knn(train = iris.training, test = iris.test, cl = iris.trainingLabels, k=3)
iris_prediction_model

library(ggplot2)

pred_plot <- qplot(iris_prediction_model, 
                   xlab = 'Flower', 
                   ylab = 'Count of species',
                   main = 'Predicted values',)
actual_plot <- qplot(iris.testLabels,
                     xlab = 'Flower', 
                     ylab = 'Count of species',
                     main = 'Actual values')

qplot(iris_prediction_model, iris.testLabels, facets= iris.testLabels~.)

library(pacman)
pacman :: p_load(ggpubr)

ggarrange(pred_plot, actual_plot)

pred_plot_length <- qplot(x = iris_prediction_model, y = iris_prediction_model$Petal.Length, 
                   xlab = 'Flower', 
                   ylab = 'Count of species',
                   main = 'Predicted values')
actual_plot_length <- qplot(x = iris.testLabels, y = iris.testLabels$Petal.Length,
                     xlab = 'Flower', 
                     ylab = 'Count of species',
                     main = 'Actual values')

ggarrange(pred_plot_length, actual_plot_length)

          