











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

set.seed(134562)


# read in airlines data for commercial domestic 
# U.S. flights originating at a particular airport
# in the year 2022


# Read in data

# First, create data types

flight.data.types <- c('factor',   # Month
                        'factor',    # Day of Week 
                        'factor',    # Carrier 
                        'factor',    # origin airport
                        'factor',    # origin state
                        'factor',    # destination airport
                        'factor',    # destination state
                        'numeric',    # Departure Time
                        'numeric', #  on time or not 
                        'numeric',    # elapsed time (duration)
                        'numeric'    # distance
)

missing.values <- c("NA","")

# Departure Time for Flight: time of scheduled flight departure 
# in the form of number of minutes starting at 12:01 a.m. 
# (note that 11:59 p.m. = 1439 minutes)
# For example: 300 = 5:00 a.m., since 300/60 = 5 hours starting at 12:01 a.m.


# The following reads in the data.  
##### NEED TO CHANGE SUBDIRECTORY FOR DATA LOCATION #####

##### NEED TO CHANGE FILE NAME FOR YOUR DATA SET #####


flight.data <- read.csv("c:/temp/flights2022.f23.csv",
colClasses=flight.data.types,na.strings=missing.values)


# copy the data set to a new name, so that the original data set
# can be used again:


flight.data.new <- flight.data


# create PDF file for output
##### NEED TO CHANGE SUBDIRECTORY FOR FILE LOCATION #####
##### CAN CHANGE FILE NAME FOR YOUR DATA SET #####


pdf("c:/temp/flightlogistic.f23.pdf")


# randomly shuffle the data set for extracting
# train and test data


rand.shuffle <- sample(nrow(flight.data.new))

flight.data.new2.new <- flight.data.new[rand.shuffle,]



## create a new final data set

flight.data.fin <- flight.data.new2.new

## split training data into a training batch and a testing batch
## use 80% for training, 20% for testing later

train.percent <- 0.8

## the "createDataPartition" function is from the "caret" package

training.rows <- createDataPartition(flight.data.fin$delay, 
                                     p = train.percent, list = FALSE)
train.batch <- flight.data.fin[training.rows, ]
test.batch <- flight.data.fin[-training.rows, ]


# fit a preliminary logistic regression model with 5 predictors

# outcome variable is delay
# predictor variables are: day, carrier, depart, duration, month






#####################################

flight.logistic.1 <- glm(delay ~ day + carrier + depart + duration + month, 
                       data = train.batch, family=binomial("logit"))



summary(flight.logistic.1)

#####################################



############################
###### MODEL EVALUATION
############################

## We will check the performance of our model, using the
## "test.batch" data (the remaining 20% of the data)

## ROC CURVE AND AUC HERE


# use the logistic regression model from "flight.logistic.1"
# and apply it to the "test.batch" data

# create probabilities for the "test.batch" data:

test_prob <- predict(flight.logistic.1, newdata = test.batch, type = "response")
# create ROC curve information and AUC

test_roc <- roc(test.batch$delay ~ test_prob, plot = FALSE, print.auc = TRUE)

as.numeric(test_roc$auc)

# plot the ROC curve and show AUC on curve

plot(test_roc,main="ROC Curve for Logistic Regression",print.auc=TRUE)

graphics.off()





