#Artifical Neural Network Creation using cereal dataset:
#This ANN takes 5 variables of a cereal to predict the rating of the cereal

library(pacman)
pacman :: p_load('neuralnet')

#Import CSV
cereal_data <- read.csv('cereal.csv')

#Create Random Sample
samplesize <- 0.6 *nrow(cereal_data)
set.seed(80)
index <- sample(seq_len(nrow(cereal_data)), size = samplesize)

#Create a Training Set
training_cereal <- cereal_data[index, ]

#Create a Test Set
test_cereal <- cereal_data[-index, ]

#Normalise Data set - this is to scale data so one variable doesn't have  large
#impact on the precdiction variable

max <- apply(cereal_data, 2, max)
min <- apply(cereal_data, 2, min)
scaled <- as.data.frame(scale(cereal_data, center = min, scale = max - min))

#Create Neural Network
training_neural_net <- scaled[index, ]
test_neural_net <- scaled[-index, ]

set.seed(2)
cereal_neural_net <- neuralnet(rating ~ calories + protein + fat + sodium +fiber,
                               training_neural_net,
                               hidden = 3,
                               linear.output = T)


#Using the ANN to predict ratings od cereals based on: calories, protein, fat, sodium and fibre;
predict_testing_nn <- compute(cereal_neural_net, 
                              test_neural_net[,c(1:5)])
predict_testing_nn <- (predict_testing_nn$net.result * (max(cereal_data$rating) - min(cereal_data$rating)))+ min(cereal_data$rating)
                                                                                                                  
#Plotting the actual data against what the neural network predicted based off training set
plot(test_cereal$rating, predict_testing_nn,
     col = 'red',
     pch = 18,
     ylab = "Neural network's predicted rating",
     xlab = "Actual Rating")

abline(0,1)

#Using GGplot2 software to plot this same visual
library(ggplot2)
qplot(test_cereal$rating, predict_testing_nn,
      col = 'red',
      ylab = "Neural network's predicted rating",
      xlab = "Actual Rating",
      )


