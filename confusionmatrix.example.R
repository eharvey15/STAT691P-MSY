

# Remove all saved values from R

rm(list=ls())

# Import necessary libraries

library(caret)

library(mvtnorm)

library(readxl)

library(tidyr)


########################
# HIGH AUC ACCURACY ----
########################

# Specify data types for imports
highAUC.types <- c(
    "numeric",  # rownum
    "factor"    # y (predicted response)
)

# Import simulated data from High AUC model
highAUC.predict <- read.table(
    "./data/predict.HighAUC.testdata.txt",
    colClasses = highAUC.types,
    header = TRUE
)

# Import true data for High AUC
highAUC.true <- read.table(
    "./true.y.HighAUC.testing.txt",
    colClasses = highAUC.types,
    header = TRUE
)

# Compare predictions to true response values
(outcome1.test <- confusionMatrix(highAUC.predict$predicty, highAUC.true$y))


############################################
# HIGH AUC PREDICTOR SIGNIFICANCE STUDY ----
############################################

# Set initial seed
set.seed(248765)

# Import true coefficient values
highAUC.coefs.true <- read_xlsx("./HighAUCTrueModelSummary.xlsx", skip = 9, col_names = c("term", "value"))
highAUC.coefs.true <- highAUC.coefs.true %>%
    separate(term, sep = " = ", into = c("term.single", "term.expanded"))
highAUC.coefs.true$term.expanded <- str_replace_all(
    str_replace_all(highAUC.coefs.true$term.expanded
                    , "\\*", ":"),
    "\\((.*)\\)\\^([2-3])", "poly(\\1, degree = 3, raw = TRUE)[, \\2]")

highAUC.coefs.true <- highAUC.coefs.true %>%
    mutate(term = ifelse(is.na(highAUC.coefs.true$term.expanded),
                         highAUC.coefs.true$term.single,
                         highAUC.coefs.true$term.expanded))

highAUC.formula.true <- formula(paste(
    "highAUC.true$y",
    paste(highAUC.coefs.true$term, collapse = " + "),
    sep = " ~ "))

# Specify chosen high AUC model used for predictions (result of backwards stepwise regression from A7)
highAUC.formula.predict <- formula(highAUC.resim.y ~ x1 + x2 + x3 + x4 + x5 + x6 + 
                                       poly(x1, degree = 3, raw = TRUE)[, 2:3] + 
                                       poly(x2, degree = 3, raw = TRUE)[, 2:3] + 
                                       x1:x2 + x1:x3 + x1:x5 + x2:x3 + x2:x4 + 
                                       x2:x5 + x3:x4 + x4:x5 + 
                                       x1:x2:x3 + x2:x3:x4 + x2:x4:x5)

# Re-simulate x1-x6 since only true response is known
# As given in A8 for High AUC
samplePredictors <- function(N) {
    predictors = matrix(nrow = N, ncol = 6)
    
    # Create 3 correlated predictors with variances of 1 and covariances of 0.2
    x <- matrix(NA, N, 3)
    
    # Specify the variance-covariance matrix
    var.diagonal <- 1
    cov.off.diagonal <- 0.2 * var.diagonal  # covariance of correlated predictors
    mean.vec <- c(1, 1.5, 1.75)
    
    sigma.mat <- matrix(cov.off.diagonal, 3, 3)
    for (i in 1:3){
        sigma.mat[i,i] <- var.diagonal
    }
    x <- round(rmvnorm(N, mean=mean.vec, sigma=sigma.mat), 6)
    
    predictors[,1] <- x[,1]
    predictors[,2] <- x[,2]
    predictors[,3] <- x[,3]
    
    # Create independent normally distributed predictors
    predictors[,4] <- round(rnorm(N, mean=2, sd=1), 6)
    predictors[,5] <- round(rnorm(N, mean=-2, sd=1), 6)
    
    # Create binomial predictor
    predictors[,6] <- rbinom(N, 1, prob=0.5)
    
    return(predictors)
}

matchedTrueCoefValue <- function(term) {
    # Ordering of coefficients is not preserved, so match by name
    if (!(term %in% highAUC.coefs.true$term))
        return(NA)
    highAUC.coefs.true$value[highAUC.coefs.true$term == term]
}

coefAbsError <- function(coefs, as.pcnt=FALSE) {
    sapply(names(coefs), function(x) {
        trueval <- matchedTrueCoefValue(x)
        if (is.na(trueval))
            return(NA)
        if (as.pcnt)
            return(100 * abs(coefs[[x]] - trueval) / abs(trueval))
        return(abs(coefs[[x]] - trueval))
    })
}

extendedSummary <- function(fit, digits=4) {
    round(cbind(
        "True" = sapply(names(fit$coefficients), function(x) { matchedTrueCoefValue(x) }),
        summary(fit)$coefficients,
        "Est. Error" = coefAbsError(fit$coefficients),
        "% Est. Error" = coefAbsError(fit$coefficients, as.pcnt = TRUE)
    ), digits = digits)
}

# Compare the coefficients here with the values that were simulated
N <- nrow(highAUC.true)
predictors <- samplePredictors(N)
x1 <- predictors[,1]
x2 <- predictors[,2]
x3 <- predictors[,3]
x4 <- predictors[,4]
x5 <- predictors[,5]
x6 <- predictors[,6]

resim.data <- data.frame(
    x1 = x1,
    x2 = x2,
    x3 = x3,
    x4 = x4,
    x5 = x5,
    x6 = x6
)

# Fit a glm, but substitute coefficients to construct control glm based on true data
highAUC.logistic.true <- glm(update.formula(highAUC.formula.true, ~ . -1), family=binomial("logit"))
highAUC.logistic.true$coefficients <- sapply(names(highAUC.logistic.true$coefficients), function(x) { matchedTrueCoefValue(x) })

# Resim "true" response so that a all aspects of control are known, then fit against with last week's prediction model
head(highAUC.resim.prob) <- predict(highAUC.logistic.true, newdata = resim.data, type = "response")
highAUC.resim.y <- as.numeric(highAUC.resim.prob > 0.5)

# Specify estimated coefficients used for response predictions
highAUC.logistic.predict <- glm(update.formula(highAUC.formula.predict, ~ . -1), family=binomial("logit"))
extendedSummary(highAUC.logistic.predict)


#######################
# LOW AUC ACCURACY ----
#######################

# Specify data types for imports
lowAUC.types <- c(
    "numeric",  # rownum
    "factor"    # y (predicted response)
)

# Import simulated data from Low AUC model
lowAUC.predict <- read.table(
    "./data/predict.LowAUC.testdata.txt",
    colClasses = lowAUC.types,
    header = TRUE
)

# Import true data for Low AUC
lowAUC.true <- read.table(
    "./true.y.LowAUC.testing.txt",
    colClasses = lowAUC.types,
    header = TRUE
)

# Compare predictions to true response values
(outcome2.test <- confusionMatrix(lowAUC.predict$predicty, lowAUC.true$y))