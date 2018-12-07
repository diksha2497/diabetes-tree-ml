library(MASS)
# install.packages("neuralnet")
library(neuralnet)

# Setting up seed
set.seed(123)
data <- Boston
View(Boston)
#Structure of Boston DataFrame
str(Boston)
head(Boston)

# help(boston)

hist(data$medv)

## As the the data of each variable is not scaled 
## Let's Normalize the data in interval [0,1]
## Normalization is necessary so that each variable is scaled properly and no values overdominates

## scale function will give mean = 0 and standard deviation=1 for reach val

maxValue <- apply(data, 2, max)
maxValue
minValue <- apply(data, 2, min)
minValue
#matrix to dataframe
updated_data <- as.data.frame(scale(data,center = minValue, scale = maxValue - minValue))
updated_data
input <- sample(1:nrow(updated_data),400)
train_data <- updated_data[input,]
test_data <- updated_data[-input,]

## NeuralNetwork 
## We need to setup output vs. inputs like medv ~ crim + zn + indus + chas .. + lstat
#stores all column variables
all_vars <- colnames(updated_data)

# stores all columns except medv i.e. the output 
predictorvars <- all_vars[!all_vars%in%"medv"]
predictorvars

# creading all comuns sum except the output
predictorvars <- paste(predictorvars, collapse = "+")
predictorvars
data_form <- as.formula(paste("medv~",predictorvars, collapse = "+"))
data_form

## input + 4 hidden 2 hidden 1 o/p
neural_model <- neuralnet(formula=data_form, hidden = c(4,2), 
                          linear.output = T, data = train_data)

plot(neural_model)

#NEURAL NETS PREDICTION

neural_prediction <- compute(neural_model, test_data[,1:13])
str(neural_prediction)

# Predictions & Actual values comparisons
predictions <- neural_prediction$net.result*(max(test_data$medv)-min(test_data$medv))+min(test_data$medv)
predictions
# Actual Predictions
actual_values <- (test_data$medv)*(max(test_data$medv)-min(test_data$medv))+min(test_data$medv)
actual_values
#MEAN SQUARE ERROR
MSE <- sum((predictions-actual_values)^2)/nrow(test_data)
MSE

#plot(test_data$medv,predictions,col='red',main='Real Vs Predicted',pch=1,cex=0.9,type ="p",xlab="Actual",ylab="Predicted")
