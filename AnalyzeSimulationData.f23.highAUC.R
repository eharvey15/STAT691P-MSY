

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

train.data.types <- c('numeric',   # rownum
                        'factor',  # y
                        'numeric', # x1
                        'numeric', # x2
                        'numeric', # x3
                        'numeric', # x4
                        'numeric', # x5
                        'numeric'  # x6
)

test.data.types <- c('numeric',    # rownum
                        'numeric', # x1
                        'numeric', # x2
                        'numeric', # x3
                        'numeric', # x4
                        'numeric', # x5
                        'numeric'  # x6
)


# The following commands read in the data files.

train.data.pre <- read.table(
    "./highAUC.train.data.txt",
    colClasses=train.data.types,
    header=T
)

test.data.pre <- read.table(
    "./highAUC.test.data.txt",
    colClasses=test.data.types,
    header=T
)


# copy the data set to a new name, so that the original data set
# can be used again:

train.data <- train.data.pre

test.data <- test.data.pre


# create PDF file for output

pdf("./highAUC.simulate.f23.pdf")


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


###############################################
#### MODEL FITTING
#### STEPWISE / LASSO / BAYESIAN INFERENCE
###############################################

## Model 1 (Stepwise with high-order interactions and polynomials of x1-x5)

# Use a model with all three-term interactions and
#  polynomial terms up to third-order for all variables x1-x5
# Also include a main effect of x6

# "poly(x1,degree=3,raw=TRUE)[,2:3]" uses only the second and third
# column, which are: x1^2, x1^3 

fit.logistic.step1.pre <- glm(y ~ (x1 + x2 + x3 + x4 + x5)^3 + x6 + 
                                  poly(x1, degree=3, raw=TRUE)[,2:3] + 
                                  poly(x2, degree=3, raw=TRUE)[,2:3] + 
                                  poly(x3, degree=3, raw=TRUE)[,2:3] + 
                                  poly(x4, degree=3, raw=TRUE)[,2:3] + 
                                  poly(x5, degree=3, raw=TRUE)[,2:3],
                              data=train.batch,
                              family=binomial("logit"))

fit.logistic.step1 <- step(fit.logistic.step1.pre, direction="backward")

summary(fit.logistic.step1)

############################
###### MODEL EVALUATION
###### CHECK AUC VALUE
############################


# use logistic regression model from "fit.logistic.step1"
# and apply it to the "validate.batch" data

# create probabilities for the "validate.batch" data:

validate_prob1 <- predict(fit.logistic.step1, newdata = validate.batch, type = "response")
# create ROC curve information and AUC

validate_roc1 <- roc(validate.batch$y ~ validate_prob1, plot = FALSE, print.auc = TRUE)

as.numeric(validate_roc1$auc)

# plot the ROC curve and show AUC on curve

# NEED TO EDIT THE TITLE FOR WHICH MODEL

plot(validate_roc1,main="ROC Curve for \n Backward Stepwise Logistic Regression",print.auc=TRUE)


## Model 2 (Lasso with  higher-order interactions of x1-x6 and polynomials of x1-x5)

# Use a model with all three-term interactions and
#  polynomial terms up to third-order for all variables x1-x5
# Also include two-term interactions between all variables x1-x6

# Use Lasso regression to reduce the number of sub-models assessed
#  1. Fit GLM with lasso to find OLS estimates while minimizing # of terms
#  2. Cross-validate to find tuning parameter (lambda) that minimizes model MSE
#  3. Obtain coefficients of model at best lambda

fit.logistic.lasso1.matrix <- model.matrix(y ~ (x1 + x2 + x3 + x4 + x5)^3 + 
                                               (x1 + x6)^2 +
                                               (x2 + x6)^2 + 
                                               (x3 + x6)^2 + 
                                               (x4 + x6)^2 + 
                                               (x5 + x6)^2 + 
                                               poly(x1, degree=3, raw=TRUE)[,2:3] + 
                                               poly(x2, degree=3, raw=TRUE)[,2:3] + 
                                               poly(x3, degree=3, raw=TRUE)[,2:3] + 
                                               poly(x4, degree=3, raw=TRUE)[,2:3] + 
                                               poly(x5, degree=3, raw=TRUE)[,2:3],
                                           data=train.batch)[,-1]

fit.logistic.lasso1.glm <- glmnet(
    fit.logistic.lasso1.matrix,
    y=as.factor(train.batch$y),
    alpha=1,
    family="binomial"
)

fit.logistic.lasso1.glm.cv = cv.glmnet(
    fit.logistic.lasso1.matrix,
    y=as.numeric(train.batch$y),
    alpha=1
)
plot(fit.logistic.lasso1.glm.cv)
title("Cross-validation of Lasso Logistic Regression 1", line=3)
fit.logistic.lasso1.lambda <- fit.logistic.lasso1.glm.cv$lambda.min

(fit.logistic.lasso1 <- coef(fit.logistic.lasso1.glm, s=fit.logistic.lasso1.lambda))


# Model 3 (Bayesian inference?)

# https://biostat.app.vumc.org/wiki/pub/Main/StatisticalComputingSeries/bayes_reg_rstanarm.html


############################
###### MODEL EVALUATION
###### CHECK AUC VALUE
############################

## Model 1 (Stepwise with high-order interactions and polynomials of x1-x5)

# use logistic regression model from "fit.logistic.step1"
# and apply it to the "validate.batch" data

# create probabilities for the "validate.batch" data:

validate.step1.prob <- predict(fit.logistic.step1, newdata = validate.batch, type = "response")
# create ROC curve information and AUC

validate.step1.roc <- roc(validate.batch$y ~ validate.step1.prob, plot = FALSE, print.auc = TRUE)

(validate.step1.auc = as.numeric(validate.step1.roc$auc))

# plot the ROC curve and show AUC on curve

plot(validate.step1.roc, main = "ROC Curve for \n Stepwise Logistic Regression 1", print.auc = TRUE)


## Model 2 (Lasso with  higher-order interactions of x1-x6 and polynomials of x1-x5)

validate.lasso1.matrix <- model.matrix(y ~ (x1 + x2 + x3 + x4 + x5)^3 + 
                                           (x1 + x6)^2 +
                                           (x2 + x6)^2 + 
                                           (x3 + x6)^2 + 
                                           (x4 + x6)^2 + 
                                           (x5 + x6)^2 + 
                                           poly(x1, degree=3, raw=TRUE)[,2:3] + 
                                           poly(x2, degree=3, raw=TRUE)[,2:3] + 
                                           poly(x3, degree=3, raw=TRUE)[,2:3] + 
                                           poly(x4, degree=3, raw=TRUE)[,2:3] + 
                                           poly(x5, degree=3, raw=TRUE)[,2:3],
                                       data=validate.batch)[,-1]

validate.lasso1.prob1 <- as.vector(predict(
    fit.logistic.lasso1.glm,
    s=fit.logistic.lasso1.lambda,
    validate.lasso1.matrix,
    type="response"
))

validate.lasso1.roc = roc(validate.batch$y ~ validate.lasso1.prob1, plot = FALSE, print.auc = TRUE)

(validate.lasso1.auc = as.numeric(validate.lasso1.roc$auc))

plot(validate.lasso1.roc, main = "ROC Curve for \n Lasso Logistic Regression 1", print.auc = TRUE)


########################################
###### MODEL PREDICTION FOR TEST DATA
######   (that has unknown outcome)
########################################

# Select model that outputs highest validation AUC
model.index = which.max(c(validate.step1.auc, validate.lasso1.auc))

fit.logistic.model = c(fit.logistic.step1, fit.logistic.lasso1)[model.index]

# use logistic regression model from "fit.logistic.step1"
# and apply it to the "test.data" (that has unknown outcome)

# create probabilities for the "test.data":

test_prob <- predict(fit.logistic.model, newdata = test.data, type = "response")


## predict y from probabilities for "test.data", using 0.5 cutoff


test.data.predict.y <- as.numeric(test_prob > 0.5)


#### THIS IS THE LAST STEP
#### write out rownum, predicted y for test.data

predict.test.data <- cbind(test.data[,1], test.data.predict.y)

predict.colnames <- c("rownum","predicty")

# write out the rownum and predicted y for test.data

options(scipen=10)
write.table(
    predict.test.data,
    "./data/predict.HighAUC.testdata.txt",sep="\t",
    row.names=F,
    col.names=predict.colnames
)
options(scipen=0)

graphics.off()

