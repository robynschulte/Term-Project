##################################################
#
# QMB 6358: Software Tools for Business Analytics
#
# Term Project
#
# Name: Robyn Schulte and Jesmir Lopez
# College of Business
# University of Central Florida
#
# Date: 11/05/2020
#
##################################################
#
# Term Project - Married at First Sight
#
##################################################

# Set path for working directory.
wd_path <- "C:/Users/robyn/OneDrive/Documents/QMB6358/Term_Project/Term-Project"
# Modify the above line according to the specific path on your computer,
# as in:
# wd_path <- 'C:/Users/name/of/your/path'

# Set the working directory to this path.
setwd(wd_path)

# Verify that the path was assigned correctly.
getwd()

############################################################
# Read in data set
############################################################



data_set <- read.csv(file = "mafs.csv")



############################################################
# Summary of full data set
############################################################



summary(data_set)


###########################################################
# Summary of variable we are explaining/predicting
###########################################################


columns_to_keep_1 <- c('Married..1..Divorced..0.')
dependent_df <- data_set[,columns_to_keep_1]
summary(dependent_df)


##########################################################
# Plot a histogram
##########################################################



hist(data_set$Married..1..Divorced..0.)


#########################################################
# Summary statistics for variables
#########################################################



columns_to_keep <- c('Age.Difference', 'DrPepperSchwartz', 'DrLoganLevkoff', 'DrJosephCilona', 'ChaplainGregEpstein', 'PastorCalvinRoberson', 'RachelDeAlto', 'DrJessicaGriffin', 'DrVivianaColes')
mafs_df <- data_set[,columns_to_keep]
summary(mafs_df)


########################################################
# Regression model
########################################################


logit_model_1 <- glm(data = data_set,
                     formula = Married..1..Divorced..0. ~ DrPepperSchwartz + DrLoganLevkoff +
                       DrJosephCilona + Age.Difference,
                     family = 'binomial')
########################################################
# Summary of regression model
########################################################

summary(logit_model_1)


#######################################################
