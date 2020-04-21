library(tidyverse)
library(ISLR)
library(ROCR)
library(pROC)
library(caret)
datos <- Default
Default

head(datos)

datos <- datos %>%
  
  select(default, balance) %>%
  
  mutate(default = recode(default,
                          
                          "No"  = 0,
                          
                          "Yes" = 1))

#Setting up 60% sample data
sampleSize <- floor(0.60 * nrow(datos))

#Creating training data
train_index <- sample(seq_len(nrow(datos)), size = sampleSize)
train_data <- datos[train_index,]

# Creating test data
test_data <- datos[-train_index, ]

#Creating logistic regression model
logisticModel <- glm(default ~ balance, family = "binomial", train_data)

summary(logisticModel)
anova(logisticModel)

#Plotting performance
predLogistic <- predict(logisticModel, newdata = test_data, type = "response")
predLogistic <- ifelse(predLogistic > 0.5, 1, 0)


roc_log <- pROC::roc(test_data$default, as.numeric(predLogistic))
plot.roc(roc_log, add=FALSE, reuse.auc=TRUE, legacy.axes=FALSE, print.auc=TRUE, aces=TRUE)


#accuracy Contigency analysis
acc_contigency <- confusionMatrix(factor(predLogistic, levels = c(0,1)), as.factor(test_data$default))
acc_contigency
