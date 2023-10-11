













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
##### NEED TO CHANGE SUBDIRECTORY FOR FILE LOCATION 

##### CAN CHANGE FILE NAME FOR YOUR DATA SET #####


pdf("c:/temp/flightlasso.f23.pdf")




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



###########################
#### START LASSO HERE
###########################

### MODEL 1


model1 <- model.matrix(delay ~ (day + depart + duration + month)^2 + carrier,data=train.batch)[,-1]


glmmod1 <- glmnet(model1,y=as.factor(train.batch$delay),alpha=1,family='binomial')

cv.glmmod1 <- cv.glmnet(model1,y=train.batch$delay,alpha=1)
plot(cv.glmmod1)
best_lambda1 <- cv.glmmod1$lambda.min

best_lambda1 

model1.results <- coef(glmmod1,s=best_lambda1)

model1.results

### MODEL 2

model2 <- model.matrix(delay ~ (day + depart + duration + month)^2 + carrier + poly(depart, degree=3,raw=TRUE)[,2:3]+ poly(duration,degree=3,raw=TRUE)[,2:3],data=train.batch)[,-1]


glmmod2 <- glmnet(model2,y=as.factor(train.batch$delay),alpha=1,family='binomial')

cv.glmmod2 <- cv.glmnet(model2,y=train.batch$delay,alpha=1)
plot(cv.glmmod2)
best_lambda2 <- cv.glmmod2$lambda.min

best_lambda2 


model2.results <- coef(glmmod2,s=best_lambda2)

model2.results

############################
###### MODEL EVALUATION
############################

## Evaluate Model 1


# first, need to create the model matrix for the chosen model,
# but with "test.batch" data


model1.testdata <- model.matrix(delay ~ (day + depart + duration + month)^2 + carrier,data=test.batch)[,-1]


# create predictions using the model created above on the
# training data; this produces probabilities.
# Need to use "as.vector" for input to "roc" function


glm.predict1.lasso <- as.vector(predict(glmmod1, s=best_lambda1,model1.testdata,type="response"))


### CREATE CONFUSION MATRIX

### use cutoff of 0.5

glm.predict1.lasso.outcome <- factor(as.numeric(glm.predict1.lasso > 0.5))



confusion.outcome1.lasso <- confusionMatrix(glm.predict1.lasso.outcome,factor(test.batch$delay))


print(confusion.outcome1.lasso)

# calculate the ROC values

test_roc1 <- roc(test.batch$delay ~ glm.predict1.lasso, plot = FALSE, print.auc = TRUE)

# convert "auc" values to numeric

as.numeric(test_roc1$auc)

# plot the ROC curve and show AUC on curve

# NEED TO EDIT THE TITLE FOR WHICH MODEL

plot(test_roc1,main="ROC Curve for \n Lasso Regression 1",print.auc=TRUE)




## Evaluate Model 2

model2.testdata <- model.matrix(delay ~ (day + depart + duration + month)^2 + carrier + poly(depart, degree=3,raw=TRUE)[,2:3]+ poly(duration,degree=3,raw=TRUE)[,2:3],data=test.batch)[,-1]


glm.predict2.lasso <- as.vector(predict(glmmod2, s=best_lambda2,model2.testdata,type="response"))




### CREATE CONFUSION MATRIX

### use cutoff of 0.5



glm.predict2.lasso.outcome <- factor(as.numeric(glm.predict2.lasso > 0.5))



confusion.outcome2.lasso <- confusionMatrix(glm.predict2.lasso.outcome,factor(test.batch$delay))

print(confusion.outcome2.lasso)


# calculate the ROC values


test_roc2 <- roc(test.batch$delay ~ glm.predict2.lasso, plot = FALSE, print.auc = TRUE)

as.numeric(test_roc2$auc)

# plot the ROC curve and show AUC on curve

# NEED TO EDIT THE TITLE FOR WHICH MODEL

plot(test_roc2,main="ROC Curve for \n Lasso Regression 2",print.auc=TRUE)


graphics.off()















