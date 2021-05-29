# CA2- PREDICTIVE MODELLING

# Heart Attack Predictive Modelling 

# Heart Attack data set (heart.csv) contains information about patients and 
# their chance of having a heart attack.

# Does Age, Sex, Resting Blood Pressure, Cholesterol, Fasting Blood Pressure, 
# Maximum Heart Rate, Exercise Induced Angina, Number of Major Vessels, 
# and ST Depression Induced by Exercise contribute to a patients chance of 
# having a heart attack?

# The dependent variable is the chance of a heart attack.
# The independent variables are Age, Sex, Resting Blood Pressure, 
# Cholesterol, Fasting Blood Pressure, Maximum Heart Rate, Exercise Induced
# Angina, Number of Major Vessels, and ST Depression Induced by Exercise.
#------------------------------------------------------------------------------

# IMPORT DATASET AND CHECK FOR MISSING VALUES

# Read in the heart.csv data set into a new data frame called heart_attack
heart_attack <- read.csv("heart.csv")
str(heart_attack)

# See if this dataset has any missing data
incomplete_data <- heart_attack[!complete.cases(heart_attack),]
incomplete_data
# or visualize the data for missing variables
library("mice")
md.pattern((heart_attack))

# Both complete.cases and mice md.pattern reveal that there is no missing data
# in the dataset, so there is no need to worry about removing na's

#------------------------------------------------------------------------------

# CLEAN UP AND CHANGE VARIABLE NAMES, REMOVE UNNEEDED VARIABLES, AND 
# CONVERT ANY VARIABLES IF NECESSART
 

# Clean up and change the variable names
names(heart_attack)
colnames(heart_attack)[colnames(heart_attack) 
                       == "age"] <- "Age"
colnames(heart_attack)[colnames(heart_attack) 
                       == "sex"] <- "Sex"
colnames(heart_attack)[colnames(heart_attack) 
                       == "trtbps"] <- "RestingBloodPressure"
colnames(heart_attack)[colnames(heart_attack) 
                       == "chol"] <- "Cholesterol"
colnames(heart_attack)[colnames(heart_attack) 
                       == "fbs"] <- "FastingBloodSugar"
colnames(heart_attack)[colnames(heart_attack) 
                       == "thalachh"] <- "MaximumHeartRate"
colnames(heart_attack)[colnames(heart_attack) 
                       == "exng"] <- "ExerciseInducedAngina"
colnames(heart_attack)[colnames(heart_attack) 
                       == "caa"] <- "NumberofMajorVessels"
colnames(heart_attack)[colnames(heart_attack) 
                       == "output"] <- "ChanceofHeartAttack"
colnames(heart_attack)[colnames(heart_attack) 
                       == "oldpeak"] <- "STDepressionInducedbyExercise"
colnames(heart_attack)[colnames(heart_attack) 
                       == "restecg"] <- "RestingElectrocardiocgraphicResults"
colnames(heart_attack)[colnames(heart_attack) 
                       == "cp"] <- "ChestPainType"
colnames(heart_attack)[colnames(heart_attack) 
                       == "slp"] <- "SlopeofPeakExerciseSTSegment"
colnames(heart_attack)[colnames(heart_attack) 
                       == "thall"] <- "Thalaseemia"
colnames(heart_attack)


# Remove any variables that are not needed to answer the research question
attach(heart_attack)
heart_attack <- 
  subset(heart_attack, select = -c(ChestPainType, 
                                   RestingElectrocardiocgraphicResults,
                                   SlopeofPeakExerciseSTSegment, 
                                   Thalaseemia))
head(heart_attack)

# Check variable types 
str(heart_attack)
# It is important to note that none of these variables need to be converted. 
# Sex, Fasting Blood Sugar, Exercise Induced Angina, Chance of Heart Attack are 
# all categorical dichotomous variables. However, they do not need to be 
# converted to dummy variables because they already exist in the state of 0 and 1.
# The rest of the variables are continous intervals that do not need conversion.

#------------------------------------------------------------------------------

# STEP 1: CHECK THE MODEL ASSUMPTIONS

    # linearity - is there a linear relationship among the variables?
    # normality - are the residuals normally distributed?
    # homoscedasticity - do the residuals have a constant variance?
    # no collinearity - are variables not linear correlations of each other? 
    # independence - are residuals independent and not correlated?

#------------------------------------------------------------------------------

# CHECK FOR LINEARITY

# If the relationship appears to be linear, the assumption is validated.

variables_of_interest <- c("Age", 
                           "Sex",
                           "RestingBloodPressure",
                           "Cholesterol",
                           "FastingBloodSugar",
                           "MaximumHeartRate",
                           "ExerciseInducedAngina",
                           "NumberofMajorVessels",
                           "ChanceofHeartAttack",
                           "STDepressionInducedbyExercise")
                           
# Pairs function gives a general view of the linearity of each independent
# variable with the dependent variable
pairs(heart_attack[variables_of_interest])

# Can check each relationship more closely by using scatter plots
# x-axis - dependent variable (ChanceofHearthAttack)
# y-axis - independent variable

# ChanceofHeartAttack vs Age
attach(heart_attack)
scatter.smooth(x = ChanceofHeartAttack, 
               y = Age,
               main = "ChanceofHeartAttack ~ Age",
               xlab = "Chance of Heart Attack",
               ylab = "Age")
# The scatter plot reveals there may be little to no correlation (negative)
# Check the correlation numerically
cor(ChanceofHeartAttack, Age)
# The correlation test shows the the correlation between the 
# ChanceofHeartAttack and Age variables = -0.2254387, indicating a low, negative 
# correlation between the two variables
# This relationship is not validated, however still going to continue with
# Age variable because it may be of importance for the model creation


#ChanceofHeartAttack vs Sex
scatter.smooth(x = ChanceofHeartAttack, 
               y = Sex,
               main = "ChanceofHeartAttack ~ Sex",
               xlab = "Chance of Heart Attack",
               ylab = "Sex")
# The scatter plot reveals there may be little to no correlation (negative)
# Check the correlation numerically
cor(ChanceofHeartAttack, Sex)
# The correlation test shows the the correlation between the 
# ChanceofHeartAttack and Sex variables =  -0.2809366, indicating a low, 
# negative correlation between the two variables
# This relationship is not validated, however still going to continue with
# Sex variable because it may be of importance for the model creation

#ChanceofHeartAttack vs ExerciseInducedAngina
scatter.smooth(x = ChanceofHeartAttack, 
               y = ExerciseInducedAngina,
               main = "ChanceofHeartAttack ~ ExerciseInducedAngina",
               xlab = "Chance of Heart Attack",
               ylab = "ExerciseInducedAngina")
# The scatter plot reveals there appears to be some sort of correlation between
# the variables (negative)
# Check the correlation numerically
cor(ChanceofHeartAttack, ExerciseInducedAngina)
# The correlation test shows the the correlation between the 
# ChanceofHeartAttack and ExerciseInducedAngina variables =  -0.4367571, 
# indicating a moderate, negative correlation between the two variables
# This relationship is validated, and therefore the ExerciseInducedAngina
# variable should be used

#ChanceofHeartAttack vs NumberofMajorVessels
scatter.smooth(x = ChanceofHeartAttack, 
               y = NumberofMajorVessels,
               main = "ChanceofHeartAttack ~ NumberofMajorVessels",
               xlab = "Chance of Heart Attack",
               ylab = "NumberofMajorVessels (0-4")
# The scatter plot reveals there appears to be some sort of correlation between
# the variables (negative)
# Check the correlation numerically
cor(ChanceofHeartAttack, NumberofMajorVessels)
# The correlation test shows the the correlation between the 
# ChanceofHeartAttack and NumberofMajorVessels variables =  -0.391724, 
# indicating a moderate, negative correlation between the two variables
# This relationship is validated, and therefore the NumberofMajorVessels
# variable should be used

# ChanceofHeartAttack vs RestingBloodPressure
scatter.smooth(x = ChanceofHeartAttack, 
               y = RestingBloodPressure,
               main = "ChanceofHeartAttack ~ RestingBloodPressure",
               xlab = "Chance of Heart Attack",
               ylab = "Resting Blood Pressure (mm HG)")
# The scatter plot reveals there may be little to no correlation between the 
# two variables. It looks as though it may be negative, it is hard to tell.
# Let's check the correlation numerically to be more sure
cor(ChanceofHeartAttack, RestingBloodPressure)
# The correlation test shows the the correlation between the 
# ChanceofHeartAttack and RestingBloodPressure variables = -0.1449311, 
# indicating a very low, negative correlation between the two variables
# This relationship is not validated and therefore, the RestingBloodPressure
# variable should not be used 

# ChanceofHeartAttack vs Cholesterol 
scatter.smooth(x = ChanceofHeartAttack, 
               y = Cholesterol,
               main = "ChanceofHeartAttack ~ Cholesterol",
               xlab = "Chance of Heart Attack",
               ylab = "Cholesterol (mg/dl)")
# The scatter plot reveals there may be little to no correlation between the 
# two variables. It looks as though it may be negative, it is hard to tell.
# Let's check the correlation numerically to be more sure
cor(ChanceofHeartAttack, Cholesterol)
# The correlation test shows the the correlation between the 
# ChanceofHeartAttack and Cholesterol variables = -0.08523911, indicating a very 
# low, negative correlation between the two variables
# This relationship is not validated, however still going to continue with
# Cholesterol variable because it may be of importance for the model creation

# ChanceofHeartAttack vs FastingBloodSugar
scatter.smooth(x = ChanceofHeartAttack, 
               y = FastingBloodSugar,
               main = "ChanceofHeartAttack ~ FastingBloodSugar",
               xlab = "Chance of Heart Attack",
               ylab = "FastingBloodSugar (> 120 mg/dl)")
# The scatter plot reveals there may no correlation between the 
# two variables. It looks as though it may be negative, it is hard to tell.
# Let's check the correlation numerically to be more sure
cor(ChanceofHeartAttack, FastingBloodSugar)
# The correlation test shows the the correlation between the 
# ChanceofHeartAttack and FastingBloodSugar variables = -0.02804576, indicating
# a very low, negative correlation between the two variables
# This relationship is not validated and therefore, the FastingBloodSugar
# variable should not be used 

# ChanceofHeartAttack vs MaximumHeartRate
scatter.smooth(x = ChanceofHeartAttack, 
               y = MaximumHeartRate,
               main = "ChanceofHeartAttack ~ MaximumHeartRate",
               xlab = "Chance of Heart Attack",
               ylab = "MaximumHeartRate")
# The scatter plot reveals there may a moderate correlation between the 
# two variables (positive)
# Let's check the correlation numerically 
cor(ChanceofHeartAttack, MaximumHeartRate)
# The correlation test shows the the correlation between the 
# ChanceofHeartAttack and MaximumHeartRate variables =  0.4217409, indicating
# a moderate, positive correlation between the two variables
# This relationship is validated and therefore, the MaximumHeartRate
# variable should be used 

# ChanceofHeartAttack vs STDepressionInducedbyExercise
scatter.smooth(x = ChanceofHeartAttack, 
               y = STDepressionInducedbyExercise,
               main = "ChanceofHeartAttack ~ STDepressionInducedbyExercise",
               xlab = "Chance of Heart Attack",
               ylab = "STDepressionInducedbyExercise")
# The scatter plot reveals there may a moderate correlation between the 
# two variables (negative)
# Let's check the correlation numerically 
cor(ChanceofHeartAttack, STDepressionInducedbyExercise)
# The correlation test shows the the correlation between the 
# ChanceofHeartAttack and STDepressionInducedbyExercise variables =  -0.430696, 
# indicating a moderate, negative correlation between the two variables
# This relationship is validated and therefore, the STDepressionInducedbyExercise
# variable should be used 


# Another way to display the correlations between the dependent variable 
# and the independent variables is is through the paste function
paste("Correlation for ChanceofHeartAttack and Age: ", 
      cor(ChanceofHeartAttack, Age ))
paste("Correlation for ChanceofHeartAttack and Sex: ", 
      cor(ChanceofHeartAttack, Sex))
paste("Correlation for ChanceofHeartAttack and RestingBloodPressure(mm Hg): ", 
      cor(ChanceofHeartAttack, RestingBloodPressure))
paste("Correlation for ChanceofHeartAttack and Cholesterol(mg/dl): ", 
      cor(ChanceofHeartAttack, Cholesterol))
paste("Correlation for ChanceofHeartAttack and FastingBloodSugar(>120 mg/dl): ", 
      cor(ChanceofHeartAttack, FastingBloodSugar))
paste("Correlation for ChanceofHeartAttack and MaximumHeartRate: ", 
      cor(ChanceofHeartAttack, MaximumHeartRate))
paste("Correlation for ChanceofHeartAttack and ExerciseInducedAngina: ", 
      cor(ChanceofHeartAttack, ExerciseInducedAngina))
paste("Correlation for ChanceofHeartAttack and NumberofMajorVessels: ", 
      cor(ChanceofHeartAttack, NumberofMajorVessels))
paste("Correlation for ChanceofHeartAttack and STDepressionInducedbyExercise: ", 
      cor(ChanceofHeartAttack, STDepressionInducedbyExercise))

# It is confirmed, that the independent variables 'RestingBloodPressure', 
#  and 'FastingBloodSugar' should be dropped from analysis because 
# of their low correlations with the dependent variable. However, although the
# 'Age', 'Sex', and 'Cholesterol' variables had low correlations, but they are 
# being kept because there may be some significance between them 
# and the ChanceofHeartAttack variable
heart_attack <- 
  subset(heart_attack, select = -c(RestingBloodPressure,
                                   FastingBloodSugar))
head(heart_attack)

#--------------------------------------------------------------------------------

# CHECK FOR OUTLIERS

# Outliers wont be representative of the true data that it should be representing

opar <- par(no.readonly = TRUE)
par(mfrow = c(3,2)) # Charts shown in 4 rows by 2 cols

boxplot(ChanceofHeartAttack, 
        main = "ChanceofHeartAttack", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(ChanceofHeartAttack)$out))

boxplot(Age, 
        main = "Age", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Age)$out))
boxplot(Sex, 
        main = "Sex", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Sex)$out))

boxplot(Cholesterol, 
        main = "Cholesterol", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Cholesterol)$out))

boxplot(MaximumHeartRate, 
        main = "MaximumHeartRate", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(MaximumHeartRate)$out))

boxplot(ExerciseInducedAngina, 
        main = "ExercisedInducedAngina", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(ExerciseInducedAngina)$out))

boxplot(STDepressionInducedbyExercise, 
        main = "STDepressionInducedbyExercise", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(STDepressionInducedbyExercise)$out))

boxplot(NumberofMajorVessels, 
        main = "NumberofMajorVessels", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(NumberofMajorVessels)$out))

detach(heart_attack)
# Reset parameters
par(opar)

# Boxplots show that there are outliers in 'Cholestrol', MaximumHeartRate', 
# 'STDepressionInducedbyExercise', and'NumberofMajorVessels'
# These outliers are not a representative of the rest of the data
# going to skew results, need to remove.
# However, not going to remove 'NumberofMajorVessels' because it shows that the
# outliers are at 3's and 4's where the only values possible are 1,2,3,4.

# Going to use boxplot.stats() function to extract the MaximumHeartRate outliers
outlier_values <- boxplot.stats(MaximumHeartRate)$out
paste("MaximumHeartRate outliers: ", paste(outlier_values, collapse = ", "))

# Remove MaximumHeartRate outliers
heart_attack <- subset(heart_attack, 
                 MaximumHeartRate != 71)

# Repeat for the Cholesterol variable
outlier_values <- boxplot.stats(Cholesterol)$out
paste("Cholesterol outliers: ", 
      paste(outlier_values, collapse = ", "))

# Remove Cholesterol outliers
heart_attack <- subset(heart_attack, 
                       Cholesterol != 417
                       & Cholesterol != 564
                       & Cholesterol != 394
                       & Cholesterol != 407
                       & Cholesterol!= 409)

# Repeat for the STDepressionInducedbyExercise variable
outlier_values <- boxplot.stats(STDepressionInducedbyExercise)$out
paste("STDepressionInducedbyExercise outliers: ", 
      paste(outlier_values, collapse = ", "))

# Remove STDepressionInducedbyExercise outliers
heart_attack <- subset(heart_attack, 
                 STDepressionInducedbyExercise != 4.2 
                & STDepressionInducedbyExercise != 6.2
                & STDepressionInducedbyExercise != 5.6
                & STDepressionInducedbyExercise != 4.2 
                & STDepressionInducedbyExercise != 4.4)

# Prove it worked 
# Reattach for updated new variable
attach(heart_attack)

opar <- par(no.readonly = TRUE)
par(mfrow = c(3,2)) # Charts shown in 4 rows by 2 cols

boxplot(MaximumHeartRate, 
        main = "MaximumHeartRate", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(MaximumHeartRate)$out))

boxplot(Cholesterol, 
        main = "Cholesterol", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(Cholesterol)$out))

boxplot(STDepressionInducedbyExercise, 
        main = "STDepressionInducedbyExercise", 
        sub = paste("Outlier rows: ",
                    boxplot.stats(STDepressionInducedbyExercise)$out))

detach(heart_attack)

# No more outliers

#-------------------------------------------------------------------------------

# CHECK NORMALITY

# Will use the e1071 library to show skewness values in a plot

# Skewness will tell us if it is normally dist.
# Skewness - highly skewed <-1 or >1
# moderately skewed = -1 to -0.5 and 0.5 to 1
# approximately symmetrical -0.5 to 0.5
attach(heart_attack)
library(e1071)
opar <- par(no.readonly = TRUE)
par(mfrow = c(3,2))

#ChanceofHeartAttack normality check
plot(density(ChanceofHeartAttack), 
     main = "Density Plot: Chance of Heart Attack",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(ChanceofHeartAttack), 2)))
# Fill the area under the plot
polygon(density(ChanceofHeartAttack), col = "red")


paste("Skewness for ChanceofHeartAttack: ", 
      round(e1071::skewness(ChanceofHeartAttack), 2))
# Skewness for ChanceofHeartAttck:  -0.22
# This variable is approximately symmetrical

# Age normality check
plot(density(Age), 
     main = "Density Plot: Age",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(Age), 2)))
# Fill the area under the plot
polygon(density(Age), col = "red")

paste("Skewness for Age: ", 
      round(e1071::skewness(Age), 2))
# Skewness for Age:  -0.18
# This variable is approximately symmetrical

# Sex normality check
plot(density(Sex), 
     main = "Density Plot: Sex",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(Sex), 2)))
# Fill the area under the plot
polygon(density(Sex), col = "red")

paste("Skewness for Sex: ", 
      round(e1071::skewness(Sex), 2))
# Skewness for Sex:  -0.76
# This variable is moderately skewed

# Cholesterol normality check
plot(density(Cholesterol), 
     main = "Density Plot: Cholesterol",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(Cholesterol), 2)))
# Fill the area under the plot
polygon(density(Cholesterol), col = "red")

paste("Skewness for Cholesterol: ", 
      round(e1071::skewness(Cholesterol), 2))
# Skewness for Cholesterol:  0.19
# This variable is approximately symmetrical 

# MaximumHeartRate normality check
plot(density(MaximumHeartRate), 
     main = "Density Plot: MaximumHeartRate",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(MaximumHeartRate), 2)))
# Fill the area under the plot
polygon(density(MaximumHeartRate), col = "red")

paste("Skewness for MaximumHeartRate: ", 
      round(e1071::skewness(MaximumHeartRate), 2))
# Skewness for MaximumHeartRate:  -0.45
# This variable is approximately symmetrical

# ExerciseInducedAngina normality check
plot(density(ExerciseInducedAngina), 
     main = "Density Plot: ExerciseInducedAngina",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(ExerciseInducedAngina), 2)))
# Fill the area under the plot
polygon(density(ExerciseInducedAngina), col = "red")

paste("Skewness for ExerciseInducedAngina: ", 
      round(e1071::skewness(ExerciseInducedAngina), 2))
# Skewness for ExerciseInducedAngina:  0.75
# This variable is moderately skewed

# STDepressionInducedbyExercise normality check
plot(density(STDepressionInducedbyExercise), 
     main = "Density Plot: STDepressionInducedbyExercise",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(STDepressionInducedbyExercise), 2)))
# Fill the area under the plot
polygon(density(STDepressionInducedbyExercise), col = "red")

paste("Skewness for STDepressionInducedbyExercise: ", 
      round(e1071::skewness(STDepressionInducedbyExercise), 2))
# Skewness for STDepressionInducedbyExercise:  0.95
# This variable is moderately skewed

# NumberofMajorVessels normality check
plot(density(NumberofMajorVessels), 
     main = "Density Plot: NumberofMajorVessels",
     ylab = "Frequency",
     sub = paste("Skewness",
                 round(e1071::skewness(NumberofMajorVessels), 2)))
# Fill the area under the plot
polygon(density(NumberofMajorVessels), col = "red")

paste("Skewness for NumberofMajorVessels: ", 
      round(e1071::skewness(NumberofMajorVessels), 2))
# Skewness for NumberofMajorVessels:  1.33
# This variable is highly skewed

# Can also check normality using histogram and qqnorm() function
# Will show example of histogram and qqnorm with Age variable
# No need to do it for all variables because their normalities were already
# checked above by plotting skewness

opar <- par(no.readonly = TRUE)
par(mfrow = c(1, 2)) # divide graph area in 2 columns
hist(Age, 
     main = "Normality proportion of Age", 
     xlab = "Age")

qqnorm(Age)
qqline(Age)
par <- opar
# As it can be seen, both the histogram and QQplot show that this variable
# is approximately symmetrical 

# Not going to go forward and used NumberofMajorVessels variable because it is
# highly skewed
# Sex, STDepressionInducedbyExerciseare, and ExerciseInducedAngina are moderately
# skewed. Instead of removing the variables, will use the powerTransform() 
# function to normalize both of these variables. 

# Update heart_attack dataframe, with NumberofMajorVessels removed
heart_attack <- 
  subset(heart_attack, select = -c(NumberofMajorVessels))

head(heart_attack)

# -----------------------------------------------------------------------------

# BUILD THE MULTIPLE LINEAR REGRESSION (MLR) MODEL

# lm(Dependent variable ~ independent variables)
# Trying to explain a patients chance of a heart attack by using the Age, Sex,
# MaximumHeartRate, ExerciseInducedAngina, STDepressionInducedbyExercise
# variables in the data set 
attach(heart_attack)
mlr_model <- lm(ChanceofHeartAttack ~ 
                  Age + 
                  Sex + 
                  Cholesterol +
                  MaximumHeartRate +
                  ExerciseInducedAngina + 
                  STDepressionInducedbyExercise, 
                data = heart_attack)
summary(mlr_model)

# Results from mlr_model

# It is first important to note that R-squared measures how much of the 
# variation in the outcome can be explained by the variation in the predictor.
# However, R-squared always increases with the more number of predictors, so it 
# should not be used to choose which predictors should be used in the model
# The multiple R-squared value for this model is 0.3892
# This means that all of the predictor variables account for 38.92% of the 
# variance in a patients chance of a heart attack.

# The Sex, MaximumHeart Rate, ExerciseInducedAngina, and 
# STDepressionInducedbyExercise variables are all statistically significant 
# as indicated by either ** or *** in the results. Cholesterol is also
# somewhat statistically significant as indicated by *. 

# The Residual Standard Error = 0.3934 on 285 degrees of freedom
# This is a low standard error, which means the predictions are pretty 
# good 

# Look at the output of the model in a different light
confint(mlr_model)

 # The Results
# These results reveal a confidence interval (at 95%) for each of the variables
# For example, you can be 95% confident that the interval 
# [ -0.009612318 , 0.0015642962] contains the true change in Age 
# for a 1% change in chance of a heart attack. 

#------------------------------------------------------------------------------
 
# PREDICTION

# Create training and testing data 
set.seed(1)
no_rows_data <- nrow(heart_attack)
data_sample <- sample(1: no_rows_data, size = round(0.7 * no_rows_data), 
                      replace = FALSE)
data_sample

training_data <- heart_attack[data_sample, ]
testing_data <- heart_attack[-data_sample, ]


# Build the model based on training data
mlr_model <- lm(ChanceofHeartAttack ~ 
                  Age + 
                  Sex + 
                  Cholesterol +
                  MaximumHeartRate +
                  ExerciseInducedAngina + 
                  STDepressionInducedbyExercise, 
                data = training_data)
summary(mlr_model)

# The multiple R-squared value for this model is now 0.3785
# This means that all of the predictor variables account for 37.85% of the 
# variance in a patients chance of a heart attack in the training data.

# The Sex, MaximumHeart Rate, ExerciseInducedAngina, and 
# STDepressionInducedbyExercise variables are all still statistically significant 
# as indicated by either ** or *** in the results. And Cholesterol slightly
# significant indicated by *. 

# The Residual Standard Error =  0.3992 on 197 degrees of freedom
# This is a low standard error, which means the predictions are pretty 
# good 

#-------------------------------------------------------------------------------

# HOMOSCEDASTICITY

# Use the ncvTest() to test for homoscedasticity
library(car)
ncvTest(mlr_model)

# The p-value for this test is p = 0.72968.
# Because the p-value is greater than the cut-off of 0.05, the residuals
# have a constant variance (homoscedasticity)
# Therefore, this assumption has been met.
# If this test was significant (p-value less than 0.05) then it would suggest
# heteroscedasticity

#-------------------------------------------------------------------------------

# GLOBAL VALIDATION OF LINEAR MODEL ASSUMPTION

install.packages("gvlma")
library(gvlma)
gvmodel <- gvlma(mlr_model)
summary(gvmodel)

# The Global stat line reveals that the data meets all of the assumptions for
# OLS regression model (with a p-value of 0.2959)

#-------------------------------------------------------------------------------

# MULTICOLLINEARITY CHECK

# Use variance inflation factor (VIF) to test to see if there is any indications
# of a multicollinearity problem
library(car)
vif(mlr_model)

# Check if there is any collinearity problems with the variables if the value 
# is greater than two
sqrt(vif(mlr_model)) > 2
# The results reveal that there is no concern with the collinearity of any of 
# the variables and therefore, this assumption is met.

#-------------------------------------------------------------------------------

# INDEPENDENCE CHECK 

library(car)
durbinWatsonTest(mlr_model)
# Autocorrelation of 0.03550665 
# This means there is a positive autocorrelation 

#-------------------------------------------------------------------------------

# TRANSFORMING VARIABLES 

# Doing this because three variables did not meet the normality assumption
library(car)
summary(powerTransform(training_data$ChanceofHeartAttack))
# Error in running the powerTransfrom() function because 'first argument must be 
# strictly positive'.
# Unable to solve. 
#-------------------------------------------------------------------------------


# COMPARING MODELS USING AIC

attach(heart_attack)
sqrt_transform_chanceofheartattack <- sqrt(training_data$ChanceofHeartAttack)
training_data$ChanceofHeartAttack_sqrt <- sqrt_transform_chanceofheartattack

fit_model1 <- lm(ChanceofHeartAttack ~ 
                   Age + 
                   Sex + 
                   Cholesterol +
                   MaximumHeartRate +
                   ExerciseInducedAngina + 
                   STDepressionInducedbyExercise,
                 data=training_data)
fit_model2 <- lm(ChanceofHeartAttack_sqrt ~ 
                   Age + 
                   Sex + 
                   Cholesterol +
                   MaximumHeartRate +
                   ExerciseInducedAngina + 
                   STDepressionInducedbyExercise,
                 data=training_data)
AIC(fit_model1,fit_model2)


#-------------------------------------------------------------------------------

# STEPWISE REGRESSION

# One way of selecting a final set of predictor variables from a larger 
# set of candidate variables

# All independent variables
library(MASS)
mlr_model_test <- lm(ChanceofHeartAttack ~ 
                       Age +
                       Sex + 
                       Cholesterol +
                       MaximumHeartRate +
                       ExerciseInducedAngina + 
                       STDepressionInducedbyExercise, 
                     data=training_data)
stepAIC(mlr_model_test, direction="backward")
# AIC = 369.09

# No Age variable
mlr_model_test <- lm(ChanceofHeartAttack ~ 
                       Sex + 
                       Cholesterol +
                       MaximumHeartRate +
                       ExerciseInducedAngina + 
                       STDepressionInducedbyExercise, 
                     data=training_data)
stepAIC(mlr_model_test, direction="backward")
# AIC = 369.09
# Doesn't change the AIC value, keep Age variable in the model

# No Sex variable 
mlr_model_test <- lm(ChanceofHeartAttack ~ 
                       Age +
                       Cholesterol +
                       MaximumHeartRate +
                       ExerciseInducedAngina + 
                       STDepressionInducedbyExercise, 
                     data=training_data)
stepAIC(mlr_model_test, direction="backward")
# AIC = 353.53
# Decreasing the AIC value, keep Sex variable out of the model

# Keeping Sex variable out of model and 
# No MaximumHeartRate variable
mlr_model_test <- lm(ChanceofHeartAttack ~ 
                       Age +
                       Cholesterol +
                       ExerciseInducedAngina + 
                       STDepressionInducedbyExercise, 
                     data=training_data)
stepAIC(mlr_model_test, direction="backward")
# AIC = 343.61
# Decreasing the AIC value, keep MaximumHeartRateVariable out of model

# Keeping Sex and MaximumHeartRate variables out and 
# No ExerciseInducedAngina
mlr_model_test <- lm(ChanceofHeartAttack ~ 
                       Age +
                       Cholesterol +
                       STDepressionInducedbyExercise, 
                     data=training_data)
stepAIC(mlr_model_test, direction="backward")
# AIC = 322.43
# Decreasing the AIC value, keep ExerciseInducedAngina out of model

# Keeping Sex, MaximumHeartRate, and ExerciseInducedAngina variables out and
# No STDepressionInducedbyAngina
mlr_model_test <- lm(ChanceofHeartAttack ~ Age + Cholesterol, 
                     data=training_data)
stepAIC(mlr_model_test, direction="backward")
# AIC = 289.3
# Decreasing the AIC value, keep STDepressionInducedAngina out of the model

# Keeping Sex, MaximumHeartRate, ExerciseInducedAngina,
# and STDepressionInducedbyAngina variables out and
# No Cholesterol
mlr_model_test <- lm(ChanceofHeartAttack ~ Age, data=training_data)
stepAIC(mlr_model_test, direction="backward")
# AIG = 289.3 
# Doesn't change the AIC value, keep Cholesterol variable in the mode

# -----------------------------------------------------------------------------

# ALL SUBSETS REGRESSION

# One of selecting a final set of predictor variables from a larger 
# set of candidate variables
# This regression should produce the same results as the stepwise regression
# to validate and double confirm the findings

install.packages("leaps")
library(leaps)
leaps <-regsubsets(ChanceofHeartAttack ~  
                       Age +
                       Sex + 
                       Cholesterol +
                       MaximumHeartRate +
                       ExerciseInducedAngina + 
                       STDepressionInducedbyExercise, data=training_data, nbest=4)
plot(leaps, scale="adjr2")

# This plot shows that Age and Cholesterol are still the best
# Age's R-squared value = 0.35
# Cholesterol's R-squared value = 0.36
# Whereas Sex's R-squared value = 0.087
# MaximumHeartRate's R-squared value = 0.25
# ExerciseInducedAngina's R-squared value = 0.20
# STDepressionInducedbyExercise's R-squared value = 0.18

# -----------------------------------------------------------------------------

# PREDICTED ACCURACY 

mlr_model_test <- lm(ChanceofHeartAttack ~ 
                       Age +
                       Cholesterol,
                     data=training_data)

predicted_chanceofheartattack <- predict(mlr_model_test, testing_data)


actuals_predictions <- data.frame(cbind(actuals = testing_data$ChanceofHeartAttack,
                                        predicted = predicted_chanceofheartattack))
head(actuals_predictions)
# The predicted values vs the actuals vary in accuracy. 
# For example, for the actual value of 1, the predicted value is 0.7125076
# Whereas for the actual value of 1, the predicted value is 0.4431866 which
# is far more off

# Correlation Accuracy 
correlation_accuracy <- cor(actuals_predictions)
correlation_accuracy
# 32.67% correlation accuracy

# Min-Max Accuracy
min_max_accuracy <- mean(apply(actuals_predictions, 1, min) /
                           apply(actuals_predictions, 1, max))
min_max_accuracy
# MAPE =  0.3254685

# Residual Standard Error (RSE)
sigma(mlr_model_test)/ mean(testing_data$ChanceofHeartAttack)
# The lower the RSE, the better fit the model
# RSE = 0.8797742 
# Which means the model is not a good fit of the data 


#-------------------------------------------------------------------------------

# RUN SOME OUTPUT WITH THE FINAL MODEL

# Run a summary to find the range values
summary(heart_attack)

# Age - Age of Patient (29-77)
# Cholesterol - Cholesterol in mg/dl fetch by BMI sensor (126-360)

# Inputting a patients age and cholesterol to see what prediction it makes
# about this patients chance of having a heart attack
# From the data, this patient has a high chance of a heart attack
# Let's see what the prediction says
output <- data.frame(Age= c(63), Cholesterol = c(233))
predicted_chanceofheartattack <- predict(mlr_model_test, output)
predicted_chanceofheartattack
# The prediction is saying that patient has a moderate chance of a heart attack
# (0.4677456). This is not surprising considering this models correlation
# accuracy of 32.67% and high RSE of 0.8797742. 

# Let's try another example
# This patient, from the data, has less of a chance of a heart attack
# Let's see what the prediction says
output <- data.frame(Age= c(67), Cholesterol = c(286))
predicted_chanceofheartattack <- predict(mlr_model_test, output)
predicted_chanceofheartattack
# The prediction says the patient has less of a chance of a heart attack
# (0.389436). This is not too far off.

# This patient, from the data, has more of a chance of a heart attack
output <- data.frame(Age= c(58), Cholesterol = c(240))
predicted_chanceofheartattack <- predict(mlr_model_test, output)
predicted_chanceofheartattack
# The prediction is saying that patient has a moderate chance of a heart attack
# (.5137463). Again, this is not surprising considering this models correlation 
# accuracy of 32.67% and high RSE of 0.8797742.

