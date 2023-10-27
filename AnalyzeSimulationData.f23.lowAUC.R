










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

##### NEED TO CHANGE SUBDIRECTORY FOR DATA LOCATION #####

##### NEED TO CHANGE FILE NAME FOR YOUR DATA SET #####


## training data has known y outcome

train.data.pre <- read.table("c:/temp/lowAUC.train.data.txt",
colClasses=train.data.types,header=T)

## testing data has unknown y outcome

test.data.pre <- read.table("c:/temp/lowAUC.test.data.txt",
colClasses=test.data.types,header=T)



# copy the data set to a new name, so that the original data set
# can be used again:


train.data <- train.data.pre

test.data <- test.data.pre



# create PDF file for output
##### NEED TO CHANGE SUBDIRECTORY FOR FILE LOCATION #####
##### CAN CHANGE FILE NAME FOR YOUR DATA SET #####


pdf("c:/temp/simulate.f23.pdf")


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

# use a model with all three-term interactions and
# polynomial terms up to third-order for all variables.

# "poly(x1,degree=3,raw=TRUE)[,2:3]" uses only the second and third
# column, which are: x1^2, x1^3 

fit.logistic.step1.pre <- glm(y ~ (x1 + x2 + x3 + x4 + x5)^3 + x6 + poly(x1,degree=3,raw=TRUE)[,2:3] + poly(x2,degree=3,raw=TRUE)[,2:3] + poly(x3,degree=3,raw=TRUE)[,2:3] + poly(x4,degree=3,raw=TRUE)[,2:3] + poly(x5,degree=3,raw=TRUE)[,2:3],data=train.batch,family=binomial("logit"))

fit.logistic.step1 <- step(fit.logistic.step1.pre,direction="backward",test="F")

summary(fit.logistic.step1)



###########################
#### START STEPWISE HERE
#### MODEL CAN BE CHANGED
#### CAN USE DIFFERENT METHODS SUCH AS LASSO, AND DIFFERENT STARTING MODEL
###########################


# use logistic regression model from "fit.logistic.step1"
# and apply it to the "validate.batch" data

# create probabilities for the "validate.batch" data:

validate_prob1 <- predict(fit.logistic.step1, newdata = validate.batch, type = "response")
# create ROC curve information and AUC

validate_roc1 <- roc(validate.batch$y ~ validate_prob1, plot = FALSE, print.auc = TRUE)

as.numeric(validate_roc1$auc)

# plot the ROC curve and show AUC on curve

# NEED TO EDIT THE TITLE FOR WHICH MODEL

plot(validate_roc1,main="ROC Curve for \n Stepwise Logistic Regression 1",print.auc=TRUE)




##########


### MODEL PREDICTION FOR TEST DATA (that has unknown outcome)

# use logistic regression model from "fit.logistic.step1"
# and apply it to the "test.data" (that has unknown outcome)

# create probabilities for the "test.data":

test_prob2 <- predict(fit.logistic.step1, newdata = test.data, type = "response")


## predict y from probabilities for "test.data", using 0.5 cutoff


test.data.predict.y <- as.numeric(test_prob2 > 0.5)



#### THIS IS THE LAST STEP
#### write out rownum, predicted y for test.data


predict.test.data <- cbind(test.data[,1],test.data.predict.y)

predict.colnames <- c("rownum","predicty")

# write out the rownum and predicted y for test.data

options(scipen=10)
write.table(predict.test.data,
"c:/temp/predict.LowAUC.testdata.txt",sep="\t",
row.names=F,col.names=predict.colnames)
options(scipen=0)



graphics.off()






