









# remove all saved values from R

rm(list=ls())

# load libraries

# These libraries need to be installed

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

# set random number seed

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



# The following reads in the data.  
##### NEED TO CHANGE SUBDIRECTORY FOR DATA LOCATION #####

##### NEED TO CHANGE FILE NAME FOR YOUR DATA SET #####

flight.data <- read.csv("c:/temp/flights2022.f23.csv",
colClasses=flight.data.types,na.strings=missing.values)


# copy the data set to a new name, so that the original data set
# can be used again:


flight.data.new <- flight.data



# plot the data


# create PDF file for output
##### NEED TO CHANGE SUBDIRECTORY FOR FILE LOCATION #####
##### CAN CHANGE FILE NAME FOR YOUR DATA SET #####

pdf("c:/temp/airdataexplore.f23.pdf")


barplot(table(flight.data.new$delay),
        names.arg = c("On Time", "Delayed"),
        main="Variable: delay \n(Flight On Time Status)", col="black",
ylab="Number of Flights")



# barplot: "las" creates horizontal axis labels that
# are perpendicular to the axis

# Airline (Carrier)

barplot(table(flight.data.new$carrier), las=2,
        main="Variable: Airline", col="red",
ylab="Number of Flights")


# Month for Flight

barplot(table(flight.data.new$month), las=2,
        main="Variable: Month of Flight",
col="darkblue",ylab="Number of Flights")


# Day of Week for Flight

barplot(table(flight.data.new$day), las=2,
        main="Variable: Day of Week \n 1=Monday, 7=Sunday",
col="darkgreen")


# Departure Time for Flight: time of scheduled flight departure 
# in the form of number of minutes starting at 12:01 a.m. 
# (note that 11:59 p.m. = 1439 minutes)
# For example: 300 = 5:00 a.m., since 300/60 = 5 hours since 12:00 a.m.



hist(flight.data.new$depart, main="Departure Time \nNumber of Minutes Starting at 12:01 a.m.", xlab = NULL, col="red")

# Elapsed Time in Minutes for Flight

hist(flight.data.new$duration, main="Elapsed Time in Minutes for Flight", xlab = NULL, col="darkviolet")


# plot a mosaic plot for two variables: here it is delay by carrier

# mosaicplot: "las" creates horizontal axis labels that
# are perpendicular to the axis carrier



mosaicplot(flight.data.new$carrier ~ flight.data.new$delay, las=2,cex.axis=0.7,
           main="Delay by Airline", shade=FALSE, 
           color=c("blue","red"), xlab="Airline", ylab="Delay: 1=Yes (red), 0=No (blue)")

# plot a mosaic plot for two variables: here it is delay by month

mosaicplot(flight.data.new$month ~ flight.data.new$delay, cex.axis=1,
           main="Delay by Month", shade=FALSE, 
           color=c("blue","red"), xlab="Month", ylab="Delay: 1=Yes, 0=No")

# plot a mosaic plot for two variables: here it is delay by week

mosaicplot(flight.data.new$day ~ flight.data.new$delay, cex.axis=1,
           main="Delay by Day of Week", shade=FALSE, 
           color=c("blue","red"), xlab="Day of Week: 1=Monday, 7=Sunday", ylab="Delay: 1=Yes, 0=No")


# plot a boxplot of delay by departure time

boxplot(flight.data.new$depart ~ flight.data.new$delay, 
        main="Flight Delay Status by Departure Time",
        xlab="Delay: 0=No, 1=Yes", ylab="Departure Time")



boxplot(flight.data.new$duration ~ flight.data.new$delay, 
        main="Flight Delay Status by \n Duration Time of Flight (Minutes)",
        xlab="Delay: 0=No, 1=Yes", ylab="Duration Time (Minutes)")


# plot 2 correlograms: one with ovals, one with numbers.


corrgram.data <- flight.data.new
## change features of factor type to numeric type for inclusion on correlogram

corrgram.data$delay <- as.numeric(corrgram.data$delay)


# define variables to use in correlogram

corrgram.vars <- c("delay", "depart", "duration")

# create data matrix of just variables for correlogram

corrgram.data.final <- corrgram.data[,corrgram.vars]

# calculate correlation between variables. "pairwise.complete.obs" is
# for using only data that is not missing for both variables

corr.matrix.flight <- cor(corrgram.data.final,use="pairwise.complete.obs")

# define colors for plot
colorvals <- colorRamp(c("#CC0000","white","#3366CC"),space="Lab")

# plot correlogram, using ovals to represent correlation

plotcorr(corr.matrix.flight, col=rgb(colorvals((corr.matrix.flight+1)/2), maxColorValue=255))


# plot correlogram, using numbers for correlation instead of ovals

corrgram(corr.matrix.flight, type="cor",upper.panel=panel.conf,
lower.panel=panel.conf)

# close the graphic file.

graphics.off()














