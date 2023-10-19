
------------------------
# ENVIRONMENT SETUP ----
------------------------

## simulate logistic regression data


rm(list=ls())

# need to install package "mvtnorm"

library("mvtnorm")

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


------------------------------------------
# DATA SIMULATION VARIABLES (MODEL 1) ----  
------------------------------------------

# number of data values to simulate 

N <- 20000

set.seed(874695)

# p is probability

p <- rep(NA, N)

# x's are continuous covariates

x1 <- rep(NA, N)
x2 <- rep(NA, N)
x3 <- rep(NA, N)
x4 <- rep(NA, N)
x5 <- rep(NA, N)
x6 <- rep(NA, N)

# simulate correlated data

num.corr.predictors <- 3  # number of correlated predictors

# this can be changed to have more predictors correlated than 3

x <- matrix(NA, N, num.corr.predictors)

# next, specify the variance-covariance matrix

var.diagonal <- 1  # this is variance on diagonal

## next, specify covariance of "0.2" for predictors x1,x2,x3
## this can be increased or decreased

cov.off.diagonal <- 0.2 * var.diagonal  # covariance of predictors
# this is the value off the diagonal in the covariance matrix

# "cov.off.diagonal" value has restrictions for the covariance matrix
# to be positive definite (non-singular)


# the following sets the means to zero

mean.vec <- rep(0, num.corr.predictors)

# the following assigns the covariance matrix

sigma.mat <- matrix(cov.off.diagonal, num.corr.predictors, num.corr.predictors)

# the following assigns the diagonal of the covariance matrix

for (i in 1:num.corr.predictors){
  sigma.mat[i,i] <- var.diagonal
}


# create matrix of 3 correlated predictors
# this can be changed to have more than 3 predictors that are correlated

x <- round(rmvnorm(N, mean=mean.vec, sigma=sigma.mat), 6)
         
x1 <- x[,1]
x2 <- x[,2]
x3 <- x[,3]

# create independent normally distributed predictors

x4 <- round(rnorm(N, mean=2, sd=1), 6)

x5 <- round(rnorm(N, mean=-2, sd=1), 6)

# create a binary predictor; can change prob. of success
# of "0.3" to higher or lower

x6 <- rbinom(N, 1, prob=0.3)


#### create new variables based on the first 6 predictors

x7 <- x1 * x4
x8 <- x2 * x4 * x5    # third-order variable
x9 <- (x1) ^ 2        # second-degree polynomial
x10 <- x3 * x2
x11 <- (x2) ^ 3       # third-degree polynomial
x12 <- x1 * x2 * x6


---------------------------------------------
# DATA SIMULATION COEFFICIENTS (MODEL 1) ----
---------------------------------------------

# assign values for beta values (regression coefficients)


b1 <- 0.082
b2 <- -0.21
b3 <- 1.05
b4 <- -0.65
b5 <- 0.73
b6 <- 0.5

b7 <- 0.06
b8 <- -0.07
b9 <- 0.09
b10 <- 1.1
b11 <- -1.4
b12 <- 0.12


--------------------------------
# DATA SIMULATION (MODEL 1) ----
--------------------------------

# simulate probabilities based on logistic regression model

for (i in 1:N){
  xterm = b1 * x1[i] + b2 * x2[i] + b3 * x3[i] + b4 * x4[i] + b5 * x5[i] + b6 * x6[i] +
    b7 * x7[i] + b8 * x8[i] + b9 * x9[i] + b10 * x10[i] + b11 * x11[i] + b12 * x12[i]
  
  p[i] <- exp(xterm) / (1 + exp(xterm))
}


# simulate binary "y" values based on probabilities

y <- rep(NA, N)

for (i in 1:N){
  y[i] <- rbinom(1, 1, p[i])
}


# check the values of the logistic regression model, and
# whether they are significant at alpha = 0.1 or less

logistic.out <- glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 - 1, family=binomial("logit"))


# compare the coefficients here with the values that were simulated

(logistic.out.summ = summary(logistic.out))

# check how many of the predictors have p-values
# less than or equal to an alpha-level cutoff (such as 0.1 or 0.25).

stopifnot(
  sum(logistic.out.summ$coefficients[1:6,4] <= 0.1) >= 4, # At least 4 of 6 main predictors
  sum(logistic.out.summ$coefficients[,4] <= 0.1) >= 10    # At least 10 total predictors
)

# write out the simulation data

rownum <- rep(1:length(y))


sim.data.out <- cbind(rownum, y, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12)


## split simulation data into a training batch and a testing batch
## use 80% for training, 20% for testing later

sim.data.fin <- sim.data.out

train.percent <- 0.8

## the "createDataPartition" function is from the "caret" package

training.rows <- createDataPartition(sim.data.fin[,1], 
                                     p = train.percent, list = FALSE)
train.batch <- data.frame(sim.data.fin[training.rows, ])
test.batch <- data.frame(sim.data.fin[-training.rows, ])



--------------------------------
# CHECK MODEL 1 PERFORMANCE ----
--------------------------------


# start with the training batch to create model


fit.logistic.1 <- glm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 - 1, data=train.batch, family=binomial("logit"))


summary(fit.logistic.1)


-------------------------
# MODEL 1 EVALUATION ----
-------------------------


# use logistic regression model from "fit.logistic.1"
# and apply it to the "test.batch" data

# create probabilities for the "test.batch" data:

test_prob1 <- predict(fit.logistic.1, newdata = test.batch, type = "response")
# create ROC curve information and AUC

test_roc1 <- roc(test.batch$y ~ test_prob1, plot = FALSE, print.auc = TRUE)

as.numeric(test_roc1$auc)

# plot the ROC curve and show AUC on curve

plot(test_roc1, main="ROC Curve for Test Batch \n Logistic Regression 1", print.auc=TRUE)



#### THIS IS THE LAST STEP, once all simulation work is
#### complete



# write out all data in plain text, tab-delimited format; 
# directory needs to be edited

# "options(scipen=10)" tells R not to use exponential notation,
# such as "e+10", etc.


#### For high AUC data

options(scipen=10)

write.table(
  sim.data.out,
  "./data/HighAUC.logistic.all.txt",
  sep="\t",
  row.names=F,
  col.names=T
)

options(scipen=0)






