##################################################
#
#
# Term Project
# Robyn Schulte and Jesmir Lopez
#
##################################################
#
#
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory.
# wd_path <- '/path/to/your/folder'
wd_path <- "C:/Users/robyn/OneDrive/Documents/QMB6358/Term_Project/Term-Project"
setwd(wd_path)


# It's not necessary to set the directory, since we are working in the
# main directory already: that's where the shell script is running.


# Set data directory.
data_dir <- 'Data'

# Set directory for storing figures.
fig_dir <- 'Figures'

# Set directory for storing tables.
tab_dir <- 'Tables'

# Set directory for storing text.
text_dir <- 'Text'


# Load libraries.

# The xtable library creates tex scripts for tables.
# install.packages("xtable")
library(xtable)
# The texreg library creates tex scripts for tables from
# regression models.
# install.packages("texreg")
library(texreg)


##################################################
# Load Data
##################################################

# Read the newly saved dataset.
data_file_path <- sprintf('C:/Users/robyn/OneDrive/Documents/QMB6358/Term_Project/Term-Project/mafs.csv', data_dir)
mafs_data <- read.csv(file = data_file_path)

# Inspect the data.
summary(mafs_data)


##################################################
# Create Tables
##################################################


#--------------------------------------------------
# Summarize numeric variables.
#--------------------------------------------------

# Summarize numeric variables over the entire sample.
num_var_list <- colnames(mafs_data)[lapply(mafs_data, class) == 'numeric']
summary(mafs_data[, num_var_list])


out_tab <- data.frame(matrix(nrow = 18, ncol = length(num_var_list) + 1))
colnames(out_tab) <- c('Statistic', num_var_list)
out_tab[, 'Statistic'] <- c('Min.', 'Mean', 'S.D.', 'Max.')
for (col_name in num_var_list) {
  out_tab[1, col_name] <- min(mafs_data[, col_name])
  out_tab[2, col_name] <- mean(mafs_data[, col_name])
  out_tab[3, col_name] <- sd(mafs_data[, col_name])
  out_tab[4, col_name] <- max(mafs_data[, col_name])
}


# Convert the table to a LaTex table.
out_xtable <- xtable(out_tab[, ],
                     digits = 2, label = 'tab:summary',
                     caption = 'Summary of Numeric Variables')

# Output to TeX file.
tab_file_name <- sprintf('%s/summary.tex', tab_dir)
cat(print(out_xtable), file = tab_file_name, append = FALSE)

#--------------------------------------------------
# Summarize categorical variables
#--------------------------------------------------




# Check the marriages that succeeded with Dr Pepper Schwartz's support:
table(mafs_data[, 'DrPepperSchwartz'])
table(mafs_data[, 'DrPepperSchwartz'], mafs_data[, 'Married..1..Divorced..0.'])


# Create a table of counts of variables by state and earthquake incidence.
out_tab <- table(mafs_data[, 'Age.Difference'], mafs_data[, 'Married..1..Divorced..0.'])


# Add some column names.
rownames(out_tab) <- c('Other', 'California')
colnames(out_tab) <- c('None', 'Earthquake')



# Convert the table to a LaTex table.
out_xtable <- xtable(out_tab[, ],
                     digits = 2, label = 'tab:earthquakes',
                     caption = 'Earthquake Incidence by State')

# Output to TeX file.
tab_file_name <- sprintf('%s/earthquakes.tex', tab_dir)
cat(print(out_xtable), file = tab_file_name, append = FALSE)



#--------------------------------------------------
# Create table with correlation matrix
#--------------------------------------------------

# Calculate a correlation matrix for selected variables.
corr_var_names <- c('Age.Difference', 'DrPepperSchwartz', 'DrLoganLevkoff', 'DrJosephCilona', 'ChaplainGregEpstein', 'PastorCalvinRoberson','RachelDeAlto', 'DrJessicaGriffin', 'DrVivianaColes','Married..1..Divorced..0.')
corr_matrix <- cor(mafs_data[, corr_var_names])
print(round(corr_matrix, 9))


# Convert the table to a LaTex table.
out_xtable <- xtable(corr_matrix[, ],
                     digits = 9, label = 'tab:corr',
                     caption = 'Correlation Matrix')

# Output to TeX file.
tab_file_name <- sprintf('%s/correlation.tex', tab_dir)
cat(print(out_xtable), file = tab_file_name, append = FALSE)




##################################################
# Plot Figures
##################################################


# Plot a scattergraph of Age Difference and Married vs. Divorced.
plot(mafs_data[, 'Age.Difference'],
     mafs_data[, 'Married..1..Divorced..0.'],
     main = c('MarriedDivorced vs. AgeDifference'),
     xlab = 'AgeDifference',
     ylab = 'MarriedDivorced',
     col = 'blue')
# Not very exciting.
# Maybe another plot will look better.





##################################################
# Estimating the Regression Model
# Model 1: All Variables Included
##################################################

# Note the formula object:
# Y ~ X_1 + X_2 + X_3


# Estimate a regression model.
lm_full_model <- lm(data = mafs_data,
                    formula = Age.Difference ~ DrPepperSchwartz + DrLoganLevkoff + DrJosephCilona + ChaplainGregEpstein + PastorCalvinRoberson + RachelDeAlto + DrJessicaGriffin + DrVivianaColes + Married..1..Divorced..0.)

# Output the results to screen.
summary(lm_full_model)


##################################################
# Output table with regression estimates.
##################################################

# The texreg package makes a LaTeX table from the regression results.

# Print the output to a LaTeX file.
tab_file_name <- 'lm_model_1.tex'
out_file_name <- sprintf('%s/%s', tab_dir, tab_file_name)
texreg(lm_full_model,
       digits = 3,
       file = out_file_name,
       label = 'tab:lm_model_1',
       caption = "Regression Model 1")




##################################################
# Output text describing regression model.
##################################################

# See what's inside the lm_full_model object:
class(lm_full_model)
attributes(lm_full_model)

lm_full_model$coefficients
lm_full_model$coefficients['Age.Difference']
lm_full_model$coefficients[2]

coef(lm_full_model)



# Model predictions:
summary(predict(lm_full_model))
mafs_data[, 'predictions'] <- predict(lm_full_model)

# Other statistics are stored in the model.
attributes(summary(lm_full_model))

# The summary also returns statistics, such as R-squared.
lm_full_model_summ <- summary(lm_full_model)
lm_full_model_summ$adj.r.squared


# Create a script for a write-up about the parameters and statistics
# in the model.
# (I admit that this level of automation is a bit much
# but it highlights the possibilities.)

text_file_name <- 'regression.tex'
out_file_name <- sprintf('%s/%s', text_dir, text_file_name)
# Start a new file with append = FALSE.
cat('\n%% Regression model description:\n\n',
    file = out_file_name, append = FALSE)

# Append new lines of text with append = TRUE.
cat('\n\nThe regression model predicts married vs divorced as follows \n',
    file = out_file_name, append = TRUE)
cat('(all figures in millions).\n',
    file = out_file_name, append = TRUE)
cat('For every one dollar increase in average income, housing prices are expected \n',
    file = out_file_name, append = TRUE)
cat(sprintf('to rise by %1.3f. \n', lm_full_model$coefficients['income']),
    file = out_file_name, append = TRUE)
cat('If the home is located in California, housing prices are expected \n',
    file = out_file_name, append = TRUE)
cat(sprintf('to be %1.3f higher. \n', lm_full_model$coefficients['in_cali']),
    file = out_file_name, append = TRUE)
cat('If there was an earthquake in the zip code, housing prices are expected \n',
    file = out_file_name, append = TRUE)
cat(sprintf('to be %1.3f lower. \n', lm_full_model$coefficients['earthquake']),
    file = out_file_name, append = TRUE)

# Include a summary of the quality of fit of the model.
cat('Overall, this model provides a fairly good description ',
    file = out_file_name, append = TRUE)
cat(sprintf('with an $R^2$ of %1.3f.\n\n', lm_full_model_summ$adj.r.squared),
    file = out_file_name, append = TRUE)


##################################################
# Plot regression results for selected model.
##################################################


# Calculate the predictions from the fitted model.
mafs_data[, 'predictions'] <- predict(lm_full_model,
                                         newdata = mafs_data)

summary(mafs_data[, c('Age.Difference', 'predictions')])

plot(mafs_data[, c('Age.Difference', 'predictions')],
     main = 'Regression Model Predictions',
     xlab = 'AgeDifference',
     ylab = 'Prediction')


# So far the plot has printed to screen.
# Now use the setEPS and postscript functions to save the figure to a file.

fig_file_name <- 'predictions.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)

# Plot the actual house prices against the regression model predictions.
plot(mafs_data[, 'DrPepperSchwartz'], mafs_data[, 'predictions'],
     main = 'Regression Model Predictions',
     xlab = 'Age Difference',
     ylab = 'Prediction', pch = 16)
points(mafs_data[mafs_data[, 'Age.Difference'] == 1, 'DrPepperSchwartz'],
       mafs_data[mafs_data[, 'Age.Difference'] == 1, 'predictions'],
       col = 'green', pch = 16)
points(mafs_data[mafs_data[, 'Married..1..Divorced..0.'] == 1, 'DrPepperSchwartz'],
       mafs_data[mafs_data[, 'Married..1..Divorced..0.'] == 1, 'predictions'],
       col = 'red', pch = 16)

dev.off()


##################################################
# Add some regression lines to compare
# the predictions to the actual observations.
##################################################

# Plot the actual house prices against the regression model predictions.
plot(mafs_data[, 'DrLoganLevkoff'], mafs_data[, 'DrPepperSchwartz'],
     main = 'Regression Model Predictions',
     xlab = 'Age Difference',
     ylab = 'Dr Pepper Schwartz', pch = 16)
points(mafs_data[mafs_data[, 'Age.Difference'] == 1, 'DrLoganLevkoff'],
       housing_data[housing_data[, 'Age.Difference'] == 1, 'DrPepperSchwartz'],
       col = 'green', pch = 16)
points(housing_data[housing_data[, 'Married..1..Divorced..0.'] == 1, 'DrLoganLevkoff'],
       housing_data[housing_data[, 'Married..1..Divorced..0.'] == 1, 'DrPepperSchwartz'],
       col = 'red', pch = 16)



# Use the lines() command to append to the above figure.
# You will need to create a vector of values on the line
# using the regression coefficients from the estimated model.

summary(lm_full_model)

coef(lm_full_model)
beta_0_hat <- coef(lm_full_model)['(Intercept)']
beta_drlogan_hat <- coef(lm_full_model)['DrLoganLevkoff']
beta_age_hat <- coef(lm_full_model)['Age.Difference']
beta_married_hat <- coef(lm_full_model)['Married..1..Divorced..0.']

# Draw a line for zip codes outside California.
age_grid <- seq(0.07, 0.13, by = 0.01)
reg_line_not_drlogan <- beta_0_hat + beta_age_hat*age_grid

lines(age_grid, reg_line_not_drlogan,
      lwd = 3, col = 'black')


# Repeat for California without earthquakes (green)
reg_line_drlogan <- beta_0_hat +
  beta_age_hat*age_grid +
  beta_age_hat

lines(age_grid, reg_line_drlogan,
      lwd = 3, col = 'green')


# Repeat for California with earthquakes (red).
reg_line_married <- beta_0_hat +
  beta_age_hat*age_grid +
  beta_age_hat + beta_married_hat

lines(age_grid, reg_line_married,
      lwd = 3, col = 'red')


# Again, so far the plot has printed to screen.
# Now use the setEPS and postscript functions to save the figure to a file.

fig_file_name <- 'regression.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)

plot(mafs_data[, 'Age.Difference'], mafs_data[, 'DrPepperSchwartz'],
     main = 'Regression Model Predictions',
     xlab = 'Age Difference',
     ylab = 'Dr Pepper Schwartz', pch = 16)
points(mafs_data[mafs_data[, 'DrLoganLevkoff'] == 1, 'AgeDifference'],
       mafs_data[mafs_data[, 'DrLoganLevkoff'] == 1, 'DrPepperSchwartz'],
       col = 'green', pch = 16)
points(mafs_data[mafs_data[, 'Married..1..Divorced..0.'] == 1, 'AgeDifference'],
       mafs_data[mafs_data[, 'Married..1..Divorced..0.'] == 1, 'DrPepperSchwartz'],
       col = 'red', pch = 16)
# Plot regression lines.
lines(income_grid, reg_line_not_drlogan,
      lwd = 3, col = 'black')
lines(income_grid, reg_line_drlogan,
      lwd = 3, col = 'green')
lines(income_grid, reg_line_married,
      lwd = 3, col = 'red')
# The dev.off() closes the file for the plot.
dev.off()


##################################################
# End
##################################################

