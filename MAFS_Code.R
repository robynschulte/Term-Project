##################################################
#
# QMB 6358: Software Tools for Business Analytics
#
# Term Project
# Robyn Schulte and Jesmir Lopez
##################################################
#
# AgeDifference_Reg gives an example of OLS regression
#   using data imported from a spreadsheet.
#   It automatically generates figures and tables for a
#   pdf document built with LaTeX.
#
# Dependencies:
#   The xtable library to create tex scripts for tables.
#   The texreg library to create tex scripts for tables from
#   regression models.
#
##################################################


##################################################
# Preparing the Workspace
##################################################

# Clear workspace.
rm(list=ls(all=TRUE))

# Set working directory.

wd_path <- "C:/Users/robyn/OneDrive/Documents/QMB6358/Term_Project/Term-Project"
setwd(wd_path)


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
install.packages("xtable")
library(xtable)
# The texreg library creates tex scripts for tables from
# regression models.
install.packages("texreg")
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


out_tab <- data.frame(matrix(nrow = 4, ncol = length(num_var_list) + 1))
colnames(out_tab) <- c('Statistic', num_var_list)
out_tab[, 'Statistic'] <- c('Min.', 'Mean', 'S.D.', 'Max.')
for (col_name in num_var_list) {
  out_tab[1, col_name] <- min(mafs_data[, col_name])
  out_tab[2, col_name] <- mean(mafs_data[, col_name])
  out_tab[3, col_name] <- sd(mafs_data[, col_name])
  out_tab[4, col_name] <- max(mafs_data[, col_name])
}


# Convert the table to a LaTex table.
out_xtable <- xtable(out_tab[2, ],
                     digits = 2, label = 'tab:summary',
                     caption = 'Summary of Numeric Variables')

# Output to TeX file.
tab_file_name <- sprintf('%s/summary.tex', tab_dir)
cat(print(out_xtable), file = tab_file_name, append = FALSE)

#--------------------------------------------------
# Summarize categorical variables
#--------------------------------------------------




# Check that successful marriages were approved by Dr Pepper Schwartz:
table(mafs_data[, 'DrPepperSchwartz'])
table(mafs_data[, 'DrPepperSchwartz'], mafs_data[, 'MarriedvsDivorced'])


# Create a table of Dr Pepper Schwartz approval and Married vs Divorced.
out_tab <- table(mafs_data[, 'DrPepperSchwartz'], mafs_data[, 'MarriedvsDivorced'])


# Add some column names.
rownames(out_tab) <- c('DrPepperSchwartz')
colnames(out_tab) <- c('other', 'MarriedvsDivorced')


# Convert the table to a LaTex table.
out_xtable <- xtable(out_tab[, ],
                     digits = 2, label = 'tab:MarriedvsDivorced',
                     caption = 'MarriedvsDivorced')

# Output to TeX file.
tab_file_name <- sprintf('%s/MarriedvsDivorced.tex', tab_dir)
cat(print(out_xtable), file = tab_file_name, append = FALSE)



#--------------------------------------------------
# Create table with correlation matrix
#--------------------------------------------------

# Calculate a correlation matrix for selected variables.
corr_var_names <- c('AgeDifference', 'DrLoganLevkoff', 'DrPepperSchwartz', 'MarriedvsDivorced')
corr_matrix <- cor(mafs_data[, corr_var_names])
print(round(corr_matrix, 3))


# Convert the table to a LaTex table.
out_xtable <- xtable(corr_matrix[, ],
                     digits = 3, label = 'tab:corr',
                     caption = 'Correlation Matrix')

# Output to TeX file.
tab_file_name <- sprintf('%s/correlation.tex', tab_dir)
cat(print(out_xtable), file = tab_file_name, append = FALSE)




##################################################
# Plot Figures
##################################################


# Plot a scattergraph of DrLoganLevkoff and housing prices.
plot(mafs_data[, 'DrLoganLevkoff'],
     mafs_data[, 'AgeDifference'],
     main = c('Age Difference vs. DrLoganLevkoff'),
     xlab = 'DrLoganLevkoff',
     ylab = 'Age Difference',
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
                    formula = AgeDifference ~ DrLoganLevkoff + DrPepperSchwartz + MarriedvsDivorced)

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
lm_full_model$coefficients['DrLoganLevkoff']
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
cat('\n\nThe regression model predicts age difference as follows \n',
    file = out_file_name, append = TRUE)
cat('For every approval by DrLoganLevkoff, age difference is expected \n',
    file = out_file_name, append = TRUE)
cat(sprintf('to rise by %1.3f. \n', lm_full_model$coefficients['DrLoganLevkoff']),
    file = out_file_name, append = TRUE)
cat('If the couple is approved by DrPepperSchwartz, age difference are expected \n',
    file = out_file_name, append = TRUE)
cat(sprintf('to be %1.3f higher. \n', lm_full_model$coefficients['DrPepperSchwartz']),
    file = out_file_name, append = TRUE)
cat('If there was a successful couple approved by Dr Pepper Schwartz, age difference is expected \n',
    file = out_file_name, append = TRUE)
cat(sprintf('to be %1.3f lower. \n', lm_full_model$coefficients['MarriedvsDivorced']),
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

summary(mafs_data[, c('AgeDifference', 'predictions')])

plot(mafs_data[, c('AgeDifference', 'predictions')],
     main = 'Regression Model Predictions',
     xlab = 'Age Difference',
     ylab = 'Prediction')


# So far the plot has printed to screen.
# Now use the setEPS and postscript functions to save the figure to a file.

fig_file_name <- 'predictions.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)

# Plot the actual house prices against the regression model predictions.
plot(mafs_data[, 'AgeDifference'], mafs_data[, 'predictions'],
     main = 'Regression Model Predictions',
     xlab = 'Age Difference',
     ylab = 'Prediction', pch = 16)
points(mafs_data[mafs_data[, 'DrPepperSchwartz'] == 1, 'AgeDifference'],
       mafs_data[mafs_data[, 'DrPepperSchwartz'] == 1, 'predictions'],
       col = 'green', pch = 16)
points(mafs_data[mafs_data[, 'MarriedvsDivorced'] == 1, 'AgeDifference'],
       mafs_data[mafs_data[, 'MarriedvsDivorced'] == 1, 'predictions'],
       col = 'red', pch = 16)

dev.off()


##################################################
# Add some regression lines to compare
# the predictions to the actual observations.
##################################################

# Plot the actual house prices against the regression model predictions.
plot(mafs_data[, 'DrLoganLevkoff'], mafs_data[, 'AgeDifference'],
     main = 'Regression Model Predictions',
     xlab = 'DrLoganLevkoff',
     ylab = 'Age Difference', pch = 16)
points(mafs_data[mafs_data[, 'DrPepperSchwartz'] == 1, 'DrLoganLevkoff'],
       mafs_data[mafs_data[, 'DrPepperSchwartz'] == 1, 'AgeDifference'],
       col = 'green', pch = 16)
points(mafs_data[mafs_data[, 'MarriedvsDivorced'] == 1, 'DrLoganLevkoff'],
       mafs_data[mafs_data[, 'MarriedvsDivorced'] == 1, 'AgeDifference'],
       col = 'red', pch = 16)



# Use the lines() command to append to the above figure.
# You will need to create a vector of values on the line
# using the regression coefficients from the estimated model.

summary(lm_full_model)

coef(lm_full_model)
beta_0_hat <- coef(lm_full_model)['(Intercept)']
beta_DrLoganLevkoff_hat <- coef(lm_full_model)['DrLoganLevkoff']
beta_DrPepperSchwartz_hat <- coef(lm_full_model)['DrPepperSchwartz']
beta_MarriedvsDivorced_hat <- coef(lm_full_model)['MarriedvsDivorced']

# Draw a line for zip codes outside California.
DrLoganLevkoff_grid <- seq(0.07, 0.13, by = 0.01)
reg_line_not_DrPepperSchwartz <- beta_0_hat + beta_DrLoganLevkoff_hat*DrLoganLevkoff_grid

lines(DrLoganLevkoff_grid, reg_line_not_DrPepperSchwartz,
      lwd = 3, col = 'black')


# Repeat for California without MarriedvsDivorceds (green)
reg_line_DrPepperSchwartz <- beta_0_hat +
  beta_DrLoganLevkoff_hat*DrLoganLevkoff_grid +
  beta_DrPepperSchwartz_hat

lines(DrLoganLevkoff_grid, reg_line_DrPepperSchwartz,
      lwd = 3, col = 'green')


# Repeat for California with MarriedvsDivorceds (red).
reg_line_MarriedvsDivorced <- beta_0_hat +
  beta_DrLoganLevkoff_hat*DrLoganLevkoff_grid +
  beta_DrPepperSchwartz_hat + beta_MarriedvsDivorced_hat

lines(DrLoganLevkoff_grid, reg_line_MarriedvsDivorced,
      lwd = 3, col = 'red')


# Again, so far the plot has printed to screen.
# Now use the setEPS and postscript functions to save the figure to a file.

fig_file_name <- 'regression.eps'
out_file_name <- sprintf('%s/%s', fig_dir, fig_file_name)
setEPS()
postscript(out_file_name)

plot(mafs_data[, 'DrLoganLevkoff'], mafs_data[, 'AgeDifference'],
     main = 'Regression Model Predictions',
     xlab = 'DrLoganLevkoff',
     ylab = 'Age Difference', pch = 16)
points(mafs_data[mafs_data[, 'DrPepperSchwartz'] == 1, 'DrLoganLevkoff'],
       mafs_data[mafs_data[, 'DrPepperSchwartz'] == 1, 'AgeDifference'],
       col = 'green', pch = 16)
points(mafs_data[mafs_data[, 'MarriedvsDivorced'] == 1, 'DrLoganLevkoff'],
       mafs_data[mafs_data[, 'MarriedvsDivorced'] == 1, 'AgeDifference'],
       col = 'red', pch = 16)
# Plot regression lines.
lines(DrLoganLevkoff_grid, reg_line_not_DrPepperSchwartz,
      lwd = 3, col = 'black')
lines(DrLoganLevkoff_grid, reg_line_DrPepperSchwartz,
      lwd = 3, col = 'green')
lines(DrLoganLevkoff_grid, reg_line_MarriedvsDivorced,
      lwd = 3, col = 'red')
# The dev.off() closes the file for the plot.
dev.off()


##################################################
# End
##################################################
