

# Remove all saved values from R

rm(list=ls())

# Import necessary libraries

library(caret)


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