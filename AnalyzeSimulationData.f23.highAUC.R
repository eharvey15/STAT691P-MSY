

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

library("rstanarm")


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

fit.logistic.lasso1.glm.cv <- cv.glmnet(
    fit.logistic.lasso1.matrix,
    y=as.numeric(train.batch$y),
    alpha=1
)
plot(fit.logistic.lasso1.glm.cv)
title("Cross-validation of Lasso Logistic Regression 1", line=3)
fit.logistic.lasso1.lambda <- fit.logistic.lasso1.glm.cv$lambda.min

(fit.logistic.lasso1 <- coef(fit.logistic.lasso1.glm, s=fit.logistic.lasso1.lambda))


# Model 3 (Bayesian model averaging)

# Use both initial models used in 1 and 2 as well as four simpler models

# Use Bayesian model averaging to increase explained variance further
#  https://biostat.app.vumc.org/wiki/pub/Main/StatisticalComputingSeries/bayes_reg_rstanarm.html
#  https://onlinelibrary-wiley-com.silk.library.umass.edu/doi/epdf/10.1002/sim.1930
#
#  1. Create unit information prior for use as prior parameters and prior dist.
#  2. Run `stan_glm` for each model to:
#   a. Create both probability models, each using the priors
#   b. Generate posterior distributions for both models
#   c. Draw samples from each posterior distribution

# "Comparison of Bayesian model averaging and stepwise..." (Wang, Zhang, and Bakhai, 2004)
#  suggests using "a unit information prior, i.e. a multivariate normal prior with mean
#  at the maximum likelihood estimate and variance equal to the expected information
#  matrix for observation"
# In practice, using the described prior results in several errors during the
#  `stan_glm` step in which the MCMC regression does not converge to a useable model
# Instead, the student t distribution centered around 0 is used

# MLE of Bernoulli response (used for both Bayesian models)
#bayes.mean <- mean(as.numeric(train.batch$y))

# Fit each Bayesian inference logistic regression model with `stan_glm`
fitBayesLogistic <- function(formula) {
    n <- length(attr(terms(formula), "term.labels"))
    
    # Fisher Information of response for first Bayesian model
    #  Remove intercept since that is specified separately
    #bayes.nonbayes.glm <- glm(formula, data=train.batch, family=binomial("logit"))
    #bayes.inf.matrix <- solve(summary(bayes.nonbayes.glm, dispersion=1)$cov.unscaled)
    #bayes.inf.matrix.diag <- diag(bayes.inf.matrix)[-1]
    
    # Create prior distribution for each predictor
    #bayes.prior <- normal(location=bayes.mean, scale=bayes.inf.matrix.diag)
    bayes.prior <- student_t(df=n - 1, location=0, scale=2.5)
    
    # Fit through series of Markov chains that include warmup and sampling steps
    #  Leave at defaults of 4 chains of 2K iters such that the R-hat (convergence diagnostic)
    #   is small enough to assume that result is significant
    #  WARNING: Can take a several minutes to run
    fit.logistic.bayes.glm <- stan_glm(formula,
                                        data=train.batch,
                                        family=binomial("logit"),
                                        prior=bayes.prior,
                                        prior_intercept=bayes.prior)
    
    return(fit.logistic.bayes.glm)
}

# Fit first Bayesian model
fit.logistic.bayes1.glm <- fitBayesLogistic(bayes1.formula <- y ~ (x1 + x2 + x3 + x4 + x5)^3 + x6 + 
                                                poly(x1, degree=3, raw=TRUE)[,2:3] + 
                                                poly(x2, degree=3, raw=TRUE)[,2:3] + 
                                                poly(x3, degree=3, raw=TRUE)[,2:3] + 
                                                poly(x4, degree=3, raw=TRUE)[,2:3] + 
                                                poly(x5, degree=3, raw=TRUE)[,2:3])

summary(fit.logistic.bayes1.glm)

# Fit second Bayesian model
fit.logistic.bayes2.glm <- fitBayesLogistic(bayes2.formula <- y ~ (x1 + x2 + x3 + x4 + x5)^3 + 
                                                (x1 + x6)^2 +
                                                (x2 + x6)^2 + 
                                                (x3 + x6)^2 + 
                                                (x4 + x6)^2 + 
                                                (x5 + x6)^2 + 
                                                poly(x1, degree=3, raw=TRUE)[,2:3] + 
                                                poly(x2, degree=3, raw=TRUE)[,2:3] + 
                                                poly(x3, degree=3, raw=TRUE)[,2:3] + 
                                                poly(x4, degree=3, raw=TRUE)[,2:3] + 
                                                poly(x5, degree=3, raw=TRUE)[,2:3])

summary(fit.logistic.bayes2.glm)

# Fit third Bayesian model
fit.logistic.bayes3.glm <- fitBayesLogistic(y ~ (x1 + x2 + x3 + x4 + x5)^2 + x6)

summary(fit.logistic.bayes3.glm)

# Fit fourth Bayesian model
fit.logistic.bayes4.glm <- fitBayesLogistic(y ~ (x1 + x2 + x3 + x4 + x5)^3 + x6)

summary(fit.logistic.bayes4.glm)

# Fit fifth Bayesian model
fit.logistic.bayes5.glm <- fitBayesLogistic(y ~ (x1 + x2 + x3 + x4 + x5 + x6)^2)

summary(fit.logistic.bayes5.glm)

# Fit sixth Bayesian model
fit.logistic.bayes6.glm <- fitBayesLogistic(y ~ (x1 + x2 + x3 + x4 + x5 + x6)^3)

summary(fit.logistic.bayes6.glm)


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

(validate.step1.auc <- as.numeric(validate.step1.roc$auc))

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

validate.lasso1.roc <- roc(validate.batch$y ~ validate.lasso1.prob1, plot = FALSE, print.auc = TRUE)

(validate.lasso1.auc <- as.numeric(validate.lasso1.roc$auc))

plot(validate.lasso1.roc, main = "ROC Curve for \n Lasso Logistic Regression 1", print.auc = TRUE)


## Model 3 (Bayesian model averaging)

validate.bayes1.prob <- predict(fit.logistic.bayes1.glm, newdata = validate.batch, type = "response")
validate.bayes1.roc <- roc(validate.batch$y ~ validate.bayes1.prob, plot = FALSE, print.auc = TRUE)
(validate.bayes1.auc <- as.numeric(validate.bayes1.roc$auc))

validate.bayes2.prob <- predict(fit.logistic.bayes2.glm, newdata = validate.batch, type = "response")
validate.bayes2.roc <- roc(validate.batch$y ~ validate.bayes2.prob, plot = FALSE, print.auc = TRUE)
(validate.bayes2.auc <- as.numeric(validate.bayes2.roc$auc))

validate.bayes3.prob <- predict(fit.logistic.bayes3.glm, newdata = validate.batch, type = "response")
validate.bayes3.roc <- roc(validate.batch$y ~ validate.bayes3.prob, plot = FALSE, print.auc = TRUE)
(validate.bayes3.auc <- as.numeric(validate.bayes3.roc$auc))

validate.bayes4.prob <- predict(fit.logistic.bayes4.glm, newdata = validate.batch, type = "response")
validate.bayes4.roc <- roc(validate.batch$y ~ validate.bayes4.prob, plot = FALSE, print.auc = TRUE)
(validate.bayes4.auc <- as.numeric(validate.bayes4.roc$auc))

validate.bayes5.prob <- predict(fit.logistic.bayes5.glm, newdata = validate.batch, type = "response")
validate.bayes5.roc <- roc(validate.batch$y ~ validate.bayes5.prob, plot = FALSE, print.auc = TRUE)
(validate.bayes5.auc <- as.numeric(validate.bayes5.roc$auc))

validate.bayes6.prob <- predict(fit.logistic.bayes6.glm, newdata = validate.batch, type = "response")
validate.bayes6.roc <- roc(validate.batch$y ~ validate.bayes6.prob, plot = FALSE, print.auc = TRUE)
(validate.bayes6.auc <- as.numeric(validate.bayes6.roc$auc))

plot(validate.bayes1.roc, print.auc = TRUE, print.auc.y = 0.6)
plot(validate.bayes2.roc, print.auc = TRUE, print.auc.y = 0.5, add = TRUE)
plot(validate.bayes3.roc, print.auc = TRUE, print.auc.y = 0.4, add = TRUE)
plot(validate.bayes4.roc, print.auc = TRUE, print.auc.y = 0.3, add = TRUE)
plot(validate.bayes5.roc, print.auc = TRUE, print.auc.y = 0.2, add = TRUE)
plot(validate.bayes6.roc, print.auc = TRUE, print.auc.y = 0.1, add = TRUE)
title("ROC Curves for Bayesian \n Inference Logistic Regressions", line=2)

# Average all Bayesian models, assuming that each are equally likely
validate.bayes.avg.prob <- (
    validate.bayes1.prob + validate.bayes2.prob + 
    validate.bayes3.prob + validate.bayes4.prob + 
    validate.bayes5.prob + validate.bayes6.prob
    ) / 6
validate.bayes.avg.roc <- roc(validate.batch$y ~ validate.bayes.avg.prob, plot = FALSE, print.auc = TRUE)
(validate.bayes.avg.auc <- as.numeric(validate.bayes.roc$auc))

plot(validate.bayes.avg.roc, main = "ROC Curve for Averaged \n Bayesian Inference Logistic Regression", print.auc = TRUE)


########################################
###### MODEL PREDICTION FOR TEST DATA
######   (that has unknown outcome)
########################################

# Select model that outputs highest validation AUC
model.index <- which.max(c(
    validate.step1.auc,
    validate.lasso1.auc,
    validate.bayes1.auc,
    validate.bayes2.auc,
    validate.bayes3.auc,
    validate.bayes4.auc,
    validate.bayes5.auc,
    validate.bayes6.auc,
    validate.bayes.avg.auc
))

if (model.index == 9) {
    # Create probabilities for each Bayesian model and average under equal likelihood assumption
    test.bayes1.prob <- predict(fit.logistic.bayes1.glm, newdata = test.data, type = "response")
    test.bayes2.prob <- predict(fit.logistic.bayes2.glm, newdata = test.data, type = "response")
    test.bayes3.prob <- predict(fit.logistic.bayes3.glm, newdata = test.data, type = "response")
    test.bayes4.prob <- predict(fit.logistic.bayes4.glm, newdata = test.data, type = "response")
    test.bayes5.prob <- predict(fit.logistic.bayes5.glm, newdata = test.data, type = "response")
    test.bayes6.prob <- predict(fit.logistic.bayes6.glm, newdata = test.data, type = "response")
    
    test_prob <- (
        test.bayes1.prob + test.bayes2.prob + 
        test.bayes3.prob + test.bayes4.prob + 
        test.bayes5.prob + test.bayes6.prob
    ) / 6
} else {
    # Use chosen logistic regression model
    #  and apply it to the "test.data" (that has unknown outcome)
    fit.logistic.model <- list(
        fit.logistic.step1,
        fit.logistic.lasso1,
        fit.logistic.bayes1.glm,
        fit.logistic.bayes2.glm,
        fit.logistic.bayes3.glm,
        fit.logistic.bayes4.glm,
        fit.logistic.bayes5.glm,
        fit.logistic.bayes6.glm,
        "bayesian (skip)"
    )[[model.index]]
    
    # Create probabilities for the "test.data":
    test_prob <- predict(fit.logistic.model, newdata = test.data, type = "response")
}


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

