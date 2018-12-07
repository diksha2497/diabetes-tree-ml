diabetes <- read.csv("~/diabetes2.csv")
View(diabetes)
#----------------------------------------------------------------
set.seed(3)
id <- sample(2, nrow(diabetes),prob = c(0.7,0.3),replace = TRUE)
#----------------------------------------------------------------
diabetes_train <- diabetes[id==1,]
diabetes_test <- diabetes[id==2,]
#----------------------------------------------------------------
library(rpart)

colnames(diabetes)

diabetes_model <- rpart(Outcome~., data = diabetes_train)
diabetes_model 

plot(diabetes_model, margin = 0.1)

# Margin is used to adjust the size of the plot 
text(diabetes_model, use.n = TRUE, pretty = TRUE, cex = 0.8)
#cex=expansion factor
pred_diabetes <- predict(diabetes_model, newdata = diabetes_test, type = "vector")

pred_diabetes

table(pred_diabetes, diabetes_test$Outcome)

#library(caret)
#confusion_matrix = confusionMatrix(table(pred_diabetes, diabetes_test$Outcome))

