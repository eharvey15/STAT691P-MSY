











# remove all saved values from R

rm(list=ls())



library(caret)



predict.x1 <- as.factor(c(1,0,1,0,1,0,0,1))

truth.x2 <- as.factor(c(1,1,0,0,1,0,1,1))

outcome1.test <- confusionMatrix(predict.x1,truth.x2)



