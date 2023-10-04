--------------------------
# DATA INITIALIZATION ----
--------------------------

## Remove all saved values from R and load libraries ----

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


## Set seed, so that values can be re-created later ----

set.seed(234865)


# read in airlines data for commercial domestic 
# U.S. flights originating at a particular airport
# in the year 2022

## Read in data ----

# First, create data types

flight.data.types <- c('factor',    # Month
                       'factor',    # Day of Week
                       'factor',    # Carrier
                       'factor',    # origin airport
                       'factor',    # origin state
                       'factor',    # destination airport
                       'factor',    # destination state
                       'numeric',   # Departure Time
                       'numeric',   # on time or not
                       'numeric',   # elapsed time (duration)
                       'numeric'    # distance
)

missing.values <- c("NA","")

# Departure Time for Flight: time of scheduled flight departure 
# in the form of number of minutes starting at 12:01 a.m. 
# (note that 11:59 p.m. = 1439 minutes)
# For example: 300 = 5:00 a.m., since 300/60 = 5 hours starting at 12:01 a.m.


# The following reads in the data.

flight.data <- read.csv(
  "./NewOrlFlights2022.csv",
  colClasses=flight.data.types,
  na.strings=missing.values
)


# copy the data set to a new name, so that the original data set
# can be used again:


flight.data.new <- flight.data


## Relabel month and day factors ----

# Relabel months from indices to names
# Not all months are present, so levels are determined from data
months.names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
flight.data.new$month <- factor(
  months.names[
    as.numeric(flight.data.new$month) +
      min(as.numeric(levels(flight.data.new$month))) - 1
    ],
  levels = months.names[sort(as.numeric(levels(flight.data.new$month)))]
)

# Relabel days from indices to names
days.names <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
flight.data.new$day <- factor(
  days.names[flight.data.new$day],
  levels = days.names
)

----------------------------------------------
# MODEL TRAINING / TEST DATA ORGANIZATION ----
----------------------------------------------

## Create PDF file for output ----

pdf("./MSYflightlogistic.f23.pdf")


## Randomly shuffle the data set for extracting train and test data ----

rand.shuffle <- sample(nrow(flight.data.new))

flight.data.new2.new <- flight.data.new[rand.shuffle,]



## create a new final data set ----

flight.data.fin <- flight.data.new2.new

## Split training data into a training batch and a testing batch ----
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



--------------------------
# START STEPWISE HERE ----
--------------------------

## Model 1 ----
  
# start with a model that includes the 5 predictors, and all
# pairwise interaction terms

# the function "y ~ (a+b)^2" includes all pairwise interaction terms plus
# all lower terms

# We will use backward stepwise regression, which starts with all predictors
# in the model and removes predictors that do not meet a threshold

flight.logistic.step1.pre <- glm(delay ~ (day + depart + duration + month)^2 + carrier,
                                 data=train.batch,
                                 family=binomial("logit"))

# "test="F"" is to test using the F test statistic

flight.logistic.step1 <- step(flight.logistic.step1.pre, direction="backward", test="F")

summary(flight.logistic.step1)


## Model 2 ----

#### use polynomials

# "poly(depart,degree=3,raw=TRUE)[,:3]" uses only the second and third
# column, which are: depart^2, depart^3 


flight.logistic.step2.pre <- glm(delay ~ (day + depart + duration + month)^2 + carrier +
                                          poly(depart, degree=3, raw=TRUE)[,2:3] +
                                          poly(duration, degree=3, raw=TRUE)[,2:3],
                                 data=train.batch,
                                 family=binomial("logit"))


# "test="F"" is to test using the F test statistic

flight.logistic.step2 <- step(flight.logistic.step2.pre, direction="backward", test="F")

summary(flight.logistic.step2)


## Model 3 ----

#### Use three-term interactions and lower terms for day, depart, duration, and month

# Add two-term main effect of destination airport with carrier
# Most of these terms should hopefully drop out due to backward stepwise regression
#  leaving most significant carrier destinations
#  (which would ideally align with higher volume carrier destinations from MSY)

flight.logistic.step3.pre <- glm(delay ~ (day + depart + duration + month)^3 + (carrier + dest)^2,
                                 data=train.batch,
                                 family=binomial("logit"))

# "test="F"" is to test using the F test statistic

flight.logistic.step3 <- step(flight.logistic.step3.pre, direction="backward", test="F")

summary(flight.logistic.step3)



-----------------------
# MODEL EVALUATION ----
-----------------------

## We will check the performance of our model, using the
## "test.batch" data (the remaining 20% of the data)

## ROC CURVE AND AUC HERE


## Model 1 ----

# use the logistic regression model from "flight.logistic.1"
# and apply it to the "test.batch" data

# create probabilities for the "test.batch" data:

test_prob1 <- predict(flight.logistic.step1, newdata = test.batch, type = "response")
# create ROC curve information and AUC

test_roc1 <- roc(test.batch$delay ~ test_prob1, plot = FALSE, print.auc = TRUE)

as.numeric(test_roc1$auc)

# plot the ROC curve and show AUC on curve

plot(test_roc1, main="ROC Curve for \n Stepwise Logistic Regression 1", print.auc=TRUE)


## Model 2 ----


# use the logistic regression model from "flight.logistic.step2"
# and apply it to the "test.batch" data

# create probabilities for the "test.batch" data:

test_prob2 <- predict(flight.logistic.step2, newdata = test.batch, type = "response")
# create ROC curve information and AUC

test_roc2 <- roc(test.batch$delay ~ test_prob2, plot = FALSE, print.auc = TRUE)

as.numeric(test_roc2$auc)

# plot the ROC curve and show AUC on curve

plot(test_roc2, main="ROC Curve for \n Stepwise Logistic Regression 2", print.auc=TRUE)


## Model 3 ----

# use the logistic regression model from "flight.logistic.1"
# and apply it to the "test.batch" data

# create probabilities for the "test.batch" data:

test_prob3 <- predict(flight.logistic.step3, newdata = test.batch, type = "response")
# create ROC curve information and AUC

test_roc3 <- roc(test.batch$delay ~ test_prob3, plot = FALSE, print.auc = TRUE)

as.numeric(test_roc3$auc)

# plot the ROC curve and show AUC on curve

plot(test_roc3, main="ROC Curve for \n Stepwise Logistic Regression 3", print.auc=TRUE)


## Close PDF connection ----

graphics.off()

