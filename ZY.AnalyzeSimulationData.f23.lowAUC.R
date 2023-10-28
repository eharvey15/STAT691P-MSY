

# remove all saved values from R

rm(list=ls())

library("Amelia")

library("ellipse")

library("plyr")

library("corrgram")

library("Hmisc")

library("caret")

library("pscl")

library("glmnet")

library("car")

library("rms")

library("DAAG")

library("pROC")


### Set seed, so that values can be re-created later

set.seed(234865)


# Read in training data and testing data

# First, create data types

train.data.types <- c('numeric', #  rownum
                        'factor',   # y
                        'numeric', # x1
                        'numeric', # x2
                        'numeric',  # x3
                        'numeric', # x4
                        'numeric', # x5
                        'numeric'  # x6
)

test.data.types <- c('numeric', #  rownum
                        'numeric', # x1
                        'numeric', # x2
                        'numeric',  # x3
                        'numeric', # x4
                        'numeric', # x5
                        'numeric'  # x6
)




# The following commands read in the data files. 

## training data has known y outcome

train.data.pre <- read.table("~/Documents/Stat 691P/Ass7/lowAUC.train.data.txt",
colClasses=train.data.types,header=T)

## testing data has unknown y outcome

test.data.pre <- read.table("~/Documents/Stat 691P/Ass7/lowAUC.test.data.txt",
colClasses=test.data.types,header=T)



# copy the data set to a new name, so that the original data set
# can be used again:


train.data <- train.data.pre

test.data <- test.data.pre



# create PDF file for output
##### NEED TO CHANGE SUBDIRECTORY FOR FILE LOCATION #####
##### CAN CHANGE FILE NAME FOR YOUR DATA SET #####


pdf("~/Documents/Stat 691P/Ass7/lowAUC.simulate.f23.pdf")


### copy data to a new name


train.data.fin <- train.data

## split training data into a training batch and a validation batch
## use 80% for training, 20% for validation later

train.percent <- 0.8

## the "createDataPartition" function is from the "caret" package

training.rows <- createDataPartition(train.data.fin[,1], 
                                     p = train.percent, list = FALSE)
train.batch <- data.frame(train.data.fin[training.rows, ])
validate.batch <- data.frame(train.data.fin[-training.rows, ])



###########################
#### START STEPWISE HERE
#### CAN USE DIFFERENT METHODS SUCH AS LASSO, AND DIFFERENT STARTING MODEL
###########################

## Model 1 (Stepwise with high-order interactions and polynomials of x1-x5)

# use a model with all three-term interactions and
# polynomial terms up to third-order for all variables.

# "poly(x1,degree=3,raw=TRUE)[,2:3]" uses only the second and third
# column, which are: x1^2, x1^3 

fit.logistic.step1.pre <- glm(y ~ (x1 + x2 + x3 + x4 + x5)^3 + x6 + 
                                poly(x1,degree=3,raw=TRUE)[,2:3] + 
                                poly(x2,degree=3,raw=TRUE)[,2:3] + 
                                poly(x3,degree=3,raw=TRUE)[,2:3] + 
                                poly(x4,degree=3,raw=TRUE)[,2:3] + 
                                poly(x5,degree=3,raw=TRUE)[,2:3],
                              data=train.batch,
                              family=binomial("logit"))

fit.logistic.step2 <- step(fit.logistic.step1.pre,direction="backward",test="F")

summary(fit.logistic.step2)

## Model 2 (Lasso with higher-order interactions between x1 to x5 and x6 and polynomials for x1-x5)

fit.logistic.lasso2.matrix <- model.matrix(y ~ (x1 + x2 + x3 + x4 + x5) +
                                             I((x1 + x2 + x3 + x4 + x5)^2) +  # Adding second-degree interactions
                                             I((x1 + x2 + x3 + x4 + x5)^3) +  # Adding third-degree interactions
                                             (x1 + x6)^2 +
                                             (x2 + x6)^2 +
                                             (x3 + x6)^2 +
                                             (x4 + x6)^2 +
                                             (x5 + x6)^2 +
                                             poly(x1, degree = 3, raw = TRUE)[, 2:3] +
                                             poly(x2, degree = 3, raw = TRUE)[, 2:3] +
                                             poly(x3, degree = 3, raw = TRUE)[, 2:3] +
                                             poly(x4, degree = 3, raw = TRUE)[, 2:3] +
                                             poly(x5, degree = 3, raw = TRUE)[, 2:3],
                                           data=train.batch)[,-1]

fit.logistic.lasso2.glm <- glmnet(
  fit.logistic.lasso2.matrix,
  y=as.factor(train.batch$y),
  alpha=1,
  family="binomial"
)

fit.logistic.lasso2.glm.cv = cv.glmnet(
  fit.logistic.lasso2.matrix,
  y=as.numeric(train.batch$y),
  alpha=1
)
dev.new()
plot(fit.logistic.lasso2.glm.cv)
title("Cross-validation of Lasso Logistic Regression 2", line=3)
fit.logistic.lasso2.lambda <- fit.logistic.lasso2.glm.cv$lambda.min

fit.logistic.lasso2 = coef(fit.logistic.lasso2.glm, s=fit.logistic.lasso2.lambda)
fit.logistic.lasso2

############################
###### MODEL EVALUATION
###### CHECK AUC VALUE
############################

## Model 1 (Stepwise with high-order interactions and polynomials of x1-x5)

# use logistic regression model from "fit.logistic.step1"
# and apply it to the "validate.batch" data

# create probabilities for the "validate.batch" data:

validate.step2.prob2 <- predict(fit.logistic.step2, newdata = validate.batch, type = "response")
# create ROC curve information and AUC

validate.step2.roc2 <- roc(validate.batch$y ~ validate.step2.prob2, plot = FALSE, print.auc = TRUE)

validate.step2.auc <- as.numeric(validate.step2.roc2$auc)

# plot the ROC curve and show AUC on curve

# NEED TO EDIT THE TITLE FOR WHICH MODEL

dev.new()
plot(validate.step2.roc2,main="ROC Curve for \n Stepwise Logistic Regression 2",print.auc=TRUE)

## Model 2 (Lasso with higher-order interactions between x1 to x5 and x6 and polynomials for x1-x5)

validate.lasso2.matrix <- model.matrix(y ~ (x1 + x2 + x3 + x4 + x5) +
                                         I((x1 + x2 + x3 + x4 + x5)^2) +  # Adding second-degree interactions
                                         I((x1 + x2 + x3 + x4 + x5)^3) +  # Adding third-degree interactions
                                         (x1 + x6)^2 +
                                         (x2 + x6)^2 +
                                         (x3 + x6)^2 +
                                         (x4 + x6)^2 +
                                         (x5 + x6)^2 +
                                         poly(x1, degree = 3, raw = TRUE)[, 2:3] +
                                         poly(x2, degree = 3, raw = TRUE)[, 2:3] +
                                         poly(x3, degree = 3, raw = TRUE)[, 2:3] +
                                         poly(x4, degree = 3, raw = TRUE)[, 2:3] +
                                         poly(x5, degree = 3, raw = TRUE)[, 2:3],
                                       data=validate.batch)[,-1]

validate.lasso2.prob2 <- as.vector(predict(
  fit.logistic.lasso2.glm,
  s=fit.logistic.lasso2.lambda,
  validate.lasso2.matrix,
  type="response"
))

validate.lasso2.roc = roc(validate.batch$y ~ validate.lasso2.prob2, plot = FALSE, print.auc = TRUE)

validate.lasso2.auc = as.numeric(validate.lasso2.roc$auc)

dev.new()
plot(validate.lasso2.roc, main = "ROC Curve for \n Lasso Logistic Regression 2", print.auc = TRUE)



########################################
###### MODEL PREDICTION FOR TEST DATA
######   (that has unknown outcome)
########################################

# Select model that outputs highest validation AUC
model.index = which.max(c(validate.step2.auc, validate.lasso2.auc))

fit.logistic.model = c(fit.logistic.step2, fit.logistic.lasso2)[model.index]

# use logistic regression model from "fit.logistic.step1"
# and apply it to the "test.data" (that has unknown outcome)

# create probabilities for the "test.data":

test_prob2 <- predict(fit.logistic.step2, newdata = test.data, type = "response")


## predict y from probabilities for "test.data", using 0.5 cutoff


test.data.predict.y <- as.numeric(test_prob2 > 0.5)



#### THIS IS THE LAST STEP
#### write out rownum, predicted y for test.data


predict.test.data <- cbind(test.data[,1],test.data.predict.y)

predict.colnames <- c("rownum","predicty")

# write out the rownum and predicted y for test.data

options(scipen=10)
write.table(predict.test.data,
"~/Documents/Stat 691P/Ass7/predict.LowAUC.testdata.txt",sep="\t",
row.names=F,col.names=predict.colnames)
options(scipen=0)



graphics.off()






