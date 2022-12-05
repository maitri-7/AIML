data =read.csv(url("https://raw.githubusercontent.com/TarekDib03/Analytics/master/Week2%20-%20Linear%20Regression/Data/climate_change.csv"), header=T)
# Random sampling
samplesize = 0.60 * nrow(data)
set.seed(80)
index = sample( seq_len ( nrow ( data ) ), size = samplesize )
index
# Create training and test set
datatrain = data[ index, ]
datatest = data[ -index, ]
max = apply(data , 2 , max)
min = apply(data, 2 , min)
scaled = as.data.frame(scale(data, center = min, scale = max - min))
library(neuralnet)
# creating training and test set
trainNN = scaled[index , ]
testNN = scaled[-index , ]
names(trainNN)
# fit neural network
set.seed(2)
NN = neuralnet(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI +
                 Aerosols, trainNN, hidden = 4 , linear.output = T )
# plot neural network
plot(NN)
predict_testNN = compute(NN, testNN[,c(3:10)])
predict_testNN = (predict_testNN$net.result * (max(data$Temp) -
                                                 min(data$Temp))) + min(data$Temp)
plot(datatest$Temp, predict_testNN, col='blue', pch=16, ylab =
       "predicted temp NN", xlab = "real temp")
abline(0,1)
# Calculate Root Mean Square Error (RMSE)
RMSE.NN = (sum((datatest$Temp - predict_testNN)^2) / nrow(datatest)) ^
  0.5
RMSE.NN
