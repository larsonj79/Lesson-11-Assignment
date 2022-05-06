# Lesson 11 Assignment - Regression Part 1

# Your assignment is to write the commands instructed in the comments below. To run your
# commands, simply hit Ctrl+Enter (command+return on a MAC) when the cursor is on that 
# command line. You can also type commands directly into the Console below, but you must
# save them in this file for your assignment.

# Do not change these four lines or GradeScope will not work
library(readxl)
library(dplyr)
library(ggplot2)
fs <- read_excel("Father-son-height.xlsx")
fs <- data.frame(fs)

## Father Son Height

#We'll start by examining the famous dataset collected by Pearson (of 'Pearson 
# correlation' fame) of fathers' and sons' heights in 1903. In the code chunk above 
# is the code to load the dataset under the name fs (for father-son).

#1. Create a scatter plot with son heights on the y axis. Use the classic theme.
# Save the plot as plotfs.
plotfs <- ggplot(fs, aes(x = Father, y = Son)) +
  geom_point()

#2. Using dplyr, create a report of the following metrics for both fathers and sons:
# mean (meanf and means), standard deviation (sdf and sds), maximum (maxf and maxs),
# minimum (minf and mins). Call the report summstats.
summstats <- fs %>% 
  summarize(meanf = mean(Father),
             means = mean(Son),
             sdf = sd(Father),
             sds = sd(Son),
             maxf = max(Father),
             maxs = max(Son),
             minf = min(Father),
             mins = min(Son))
  
#3. Are sons shorter than the fathers?
# *ENTER ANSWER HERE*

#4. Are sons' heights less variant than fathers' heights?
# *ENTER ANSWER HERE*

#5. Redo the scatter plot from question 1 but this time add this line of code to 
# the plot *geom_abline(slope = 1.025627, intercept = -.737, color = "red")*. The 
# slope of 1.025627 is sds / sdf, the standard deviation of son heights divided 
# by the standard deviation of father heights. Save the plot as plotfs2.
plotfs2 <- ggplot(fs, aes(x = Father, y = Son)) +
  geom_point() +
  geom_abline(slope = 1.025627, intercept = -.737, color = "red")

  
# This line is not the regression line, but pretend for a second that it is the 
# regression line. 

#6. What would be the interpretation of the slope coefficient?
# *ENTER ANSWER HERE*

# This slope coefficient has a nice interpretation: every additional inch of
# height a father has translates to about an additional inch we can expect his
# son to be.

#7. At first glance, this red line seems to "fit" the points nicely. But take 
# a look at fathers 70 inches and taller. If we used the red line to guess their 
# sons' heights, would our guesses more often be too high or too low? Or would 
# they be too high and too low an equal number of times?
# *ENTER ANSWER HERE*
  
#8. Now look at fathers 65 inches and shorter. If we used the red line to guess
# their sons' heights, would our guess more often be too high or too low? Or 
# would the be too high and too low an equal number of times?
# *ENTER ANSWER HERE*

# The answers to those previous questions illustrate regression to the mean. If
# you use a father's height to guess his son's height, your guesses will be 
# systematically wrong for tall fathers and for short fathers.

# Now let's calculate the real regression line. The formula for the regression
# line slope is shown in the file SlopeFormula.png.

#9. Apply this formula to report the slope of the regression line. (You've already 
# calculated sdf and sds, the stand deviation of fathers and sons, respectively. 
# Calculate the correlation between the two height variables.) Save the slope
# as htslope.
cor(fs$Father, fs$Son)
htslope = cor(fs$Father, fs$Son) * summstats$sds / summstats$sdf

#10. What is the y-intercept of the regression line? (Remember, the line goes 
# through the means. Isolate the y-intercept, b0, from the basic regression 
# equation, y = b0 + b1*x, by substituting the mean height of fathers in to the 
# x value, the mean height of sons into y, and doing basic algebra.) Save the 
# intercept as htint.
htint <- summstats$means - htslope * summstats$meanf

#11. Plot the heights again, with the same red line from question 5, and add the 
# regression line using *geom_abline(slope = , intercept = )*. (Do not specify 
# a color so it is plotted in black.) Save the plot as plotfs3.
plotfs3 <- ggplot(fs, aes(x = Father, y = Son)) +
  geom_point() +
  geom_abline(slope = 1.025627, intercept = -.737, color = "red") +
  geom_abline(slope = htslope, intercept = htint)


#12. Look again at fathers 70 inches and taller. If we use the regression line 
# to guess their sons' heights, will our guesses more often be too high, too low, 
# or too high and too low an equal number of times?
# *ENTER ANSWER HERE*
                                                                  
#13. Now look at fathers 65 inches and shorter. If we use the regression line to 
# guess their sons' heights, will our guesses more often be too high, too low, 
# or too high and too low an equal number of times?
# *ENTER ANSWER HERE*

#14. How many fathers are 72 inches or taller? Save the answer as f72.
f72 <- sum(fs$Father >= 72)

#15. How many sons are 72 inches or taller? Save the answer as s72.
s72 <- sum(fs$Son >= 72)

#16. What is our best guess for the height of a man whose father was 72 inches?
# Save the answer as sonest72.
sonest72 <- htint + htslope*72

#17. Men who are 72 inches tall have sons with an average height that is lower 
# than 72 inches. How is it that we end up with so many sons 72 inches tall and 
# taller if tall men have sons with average heights lower than theirs?
# *ENTER ANSWER HERE*

#18. Let's use R to do the regression model for us. Save the regression model 
# as htreg. (Remember, we are predicting sons' heights from fathers' heights.)
htreg <- lm(Son ~ Father, data = fs)

#19. Output the summary of the regression model.
summary(htreg)

# In the coefficients table, the column labeled Pr(>|t|) is the p-value of a 
# hypothesis test. The null hypothesis for the hypothesis test is that the 
# coefficient = 0. For the hypothesis test on the slope coefficient, a 
# coefficient of 0 would mean there is no linear relationship between fathers' 
# heights and sons' heights and that any observed relationship is due to random 
# chance. 
                                                                
#20. Is the slope coefficient of .514 due to random chance?
# *ENTER ANSWER HERE*
                                                                  
# We will now use this regression model to predict the heights of the sons of 
# five men: Shawn Bradley (90 inches tall), Shaquille O'Neal (85), Magic Johnson 
# (81), Kareem Abdul_Jabbar (86), and Boban Marjanović (88).

#21. Create a matrix of dimension 5 by 1 (5 rows, 1 column) with the heights of 
# those five NBA players. Call it nbaht.
nbaht <- matrix(c(90, 85, 81, 86, 88), nrow = 5)

# In order to input those values into the regression model, they need to come 
# from a data frame with the same variable names as the x-variable in our 
# regression model. 

#22. Convert nbaht to a data frame and give its lone variable the name, "Father".
nbaht <- data.frame(nbaht)
names(nbaht) <- "Father"

#23. Predict the heights of the sons of these NBA players using the *predict()* 
# function. Save the predictions as predson.
predson <- predict(htreg, newdata = nbaht)

#24. Refer back to the maximum height of the fathers in the data set. By using 
# this data to predict the heights of these NBA players' sons, we are engaging 
# in what dangerous practice?
# *ENTER ANSWER HERE*


## Regression to the mean
# **OPTIONAL** *EXTRA CREDIT**
# Give me an example of regression to the mean you've observed in society, in 
# the media, or in your own life. Bonus points if it's a phenomenon that people 
# often ascribe some alternate meaning to.
# * ENTER ANSWER HERE *