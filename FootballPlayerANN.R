library(pacman)
pacman :: p_load('neuralnet')

#Import CSV
player_data <- read.csv('Players_Score.csv')



player_data$Goals <- as.integer(player_data$Goals)
player_data$Assists <- as.integer(player_data$Assists)
player_data$PS <- as.numeric(player_data$PS)
player_data$MotM <- as.integer(player_data$MotM)

#Create Random Sample
samplesize <- 0.6 *nrow(player_data)
set.seed(80)
index <- sample(seq_len(nrow(player_data)), size = samplesize)

#Create a Training Set
training_players <- player_data[index, ]

#Create a Test Set
test_players <- player_data[-index, ]

#Normalise

max_players <- apply(player_data, 2, max)
min_players <- apply(player_data, 2, min)
scaled <- as.data.frame(scale(player_data, center = min_players, scale = max_players - min_players))

#Create Neural Network
training_player_nn <- scaled[index, ]
test_player_nn <- scaled[-index, ]

set.seed(2)
player_neural_net <- neuralnet(Rating ~ Mins + age,
                               training_players,
                               hidden = 3,
                               linear.output = T)

player_neural_net

plot(player_neural_net)


#Using the ANN to predict
predict_testing_nn <- compute(player_neural_net, 
                              test_player_nn[,c(1:5)])
predict_testing_nn <- (predict_testing_nn$net.result * (max(player_data$Rating) - min(player_data$Rating)))+ min(player_data$Rating)


predict_testing_nn 
qplot(predict_testing_nn)

qplot(test_player_nn$Rating)

