# Home-Assignment-Theresa-Ruwe
Regression models with fixed and random effects
### Zoltan Home Assignment Part 1 ###

### Load data sample 
data_sample_1 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_1.csv")

### Load packages

require(psych)
require(lm.beta)
require(dplyr)
require(gsheet)
require(car)
require(ggplot2)
require(lsr)
require(scatterplot3d)

### Data and model descriptives: Check for invalid data 

who(TRUE) # Variables are mostly numeric (except sex which is factorial)
describe(data_sample_1)
summary(data_sample_1) # No missing values, sex has one wrong answer (look at sample, ID = 15 is the wron one)

# Exclude ID = 15 because of the wrong value in sex
data_sample_1 -> data_sample_new # Create a copy of the data set to calculate with 
data_sample_new=data_sample_1[!data_sample_1$sex==3,] # Exclude case with value "3" on sex scale 

# Histograms to show the distribution 
hist(data_sample_new$pain, breaks = 20) 
hist(data_sample_new$cortisol_serum, breaks = 20) 
hist(data_sample_new$STAI_trait, breaks = 20) 
hist(data_sample_new$cortisol_saliva, breaks = 20) 
hist(data_sample_new$mindfulness, breaks = 20) 
hist(data_sample_new$weight, breaks = 20) 
par(mfrow=3:2)

# Exclude ID = 24, 25, 66 because of wrong value in mindfulness
data_sample_new -> data_sample_new1
data_sample_new1=data_sample_new[!data_sample_new$mindfulness<=1,]

# Boxplots to check for outliers 
boxplot(data_sample_new1$pain, main = "Boxplot Pain") 
boxplot(data_sample_new1$cortisol_serum, main = "Boxplot Cortisol Serum")
boxplot(data_sample_new1$cortisol_saliva, main = "Boxplot Cortisol Saliva")
boxplot(data_sample_new1$STAI_trait, main = "Boxplot STAI")  
boxplot(data_sample_new1$mindfulness, main = "Boxplot Mindfulness")
boxplot(data_sample_new1$weight, main = "Boxplot Weight") 
par(mfrow=2:3)

# Descriptive statistics for new data set
who(TRUE)
describe(data_sample_new1)
summary(data_sample_new1)

### Fit the regression model 1 (sex & age)
mod1 <- lm(pain ~ age + sex, data = data_sample_new1) # Create model 1
summary(mod1) # Summarize model 1  
mod1
AIC(mod1)
lm.beta(mod1) # Which predictor has most influence?  
confint(mod1)
## Check for influential outliers 
cooks.distance(mod1) # Values here not salient in other checks, outliers are not being removed because
# in Psychology and from looking at the data those extreme values are fine 
# Plotting Cook's Distance
plot(mod1, which = 4) # See that none of the data are too different to be excluded 
plot(mod1, which = 5)

# Plot regression model in a 3D model 
scatterplot3d(data_sample_new1$pain ~ data_sample_new1$age + data_sample_new1$sex) 

### Fit the regression model 2 (sex, age, STAI, pain catrastophizing, mindfulness, cortisol_saliva, cortisol_serum)
mod2 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum + cortisol_saliva, data = data_sample_new1)
summary(mod2)
mod2
AIC(mod2)
lm.beta(mod2)
confint(mod2)
## Check for influential outliers 
cooks.distance(mod2) # Values here not salient in other checks, outliers are not being removed because
# in Psychology and from looking at the data those extreme values are fine 
# Plotting Cook's Distance 
plot(mod2, which = 5) 
plot(mod2, which = 4) 

### Compare models 
# Using anova 
anova(mod1, mod2) 

# Using AIC 
AIC(mod1)
AIC(mod2) # Smaller here, so there's more information in the second one; difference is bigger than 2, so the models are different from each other 

# Furthermore, adjusted R^2 is bigger in second model
summary(mod1) # 0.1015
summary(mod2) # 0.4693

### Decision for the second model being better than the first 

### Check assumptions of linear regression for final model (mod2) (p. 474 Navarro)
# 1. Normality of the residuals 
hist( x = residuals(mod2), xlab = "Value of residual", breaks = 20) # Histogram of Residuals looks normally distributed
shapiro.test(residuals(mod2)) # p-value > .05 speaks for a normal distribution 
plot(mod2, which = 2) # Or use the QQplot to see if residuals are normally distributed
# Residuals are normally distributed

# 2. Linearity of the relationship 
plot(mod2, which = 1) # Line is kind of horizontal, so we can assume linearity of the relationship (Predicted against Residuals)
pred <- predict( object = mod2 ) # Predicted against actual values 
plot( x = pred, y = data_sample_new1$pain, xlab = "Fitted Values", ylab = "Observed Values")
residualPlots( model = mod2 ) # Residual Plot for each predictor
# Relationships look linear

# 3. Checking Homoscedasticty (homogenity of variance)
plot(mod2, which = 3) # Line is kind of horizontal but to make sure, we check assumption more formal again
ncvTest(mod2) # More formal approach to check assumption: p > .05, so we can assume homogenity 
# We can assume homogenity of variance

# 4. Checking Collinearity 
vif(mod = mod2) # For the cortisol measures, the VIF is >5, so one of the variables has to be removed: Check which model is better: serum vs. saliva
## Choose cortisol_serum which makes more sense in theory. No need to make two new models 
## Two new models lead to overfitting, less fit for reality; go with what literature tells us 
## Make at least logical explanation (just use the one from the assignment)

# Create a third model with cortisol_serum only 
mod3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_new1)
summary(mod3)
mod3
AIC(mod3)
lm.beta(mod3)
confint(mod3)
## Check for influential outliers 
cooks.distance(mod3) # Values here not salient in other checks, outliers are not being removed because
# in Psychology and from looking at the data those extreme values are fine 
# Plotting Cook's Distance 
plot(mod3, which = 5) 
plot(mod3, which = 4) 

### Checking assumptions for mod3 now (as this is our final model)
# 1. Normality of the residuals 
hist( x = residuals(mod3), xlab = "Value of residual", breaks = 20) # Histogram of Residuals looks normally distributed
shapiro.test(residuals(mod3)) # p-value > .05 speaks for a normal distribution 
plot(mod3, which = 2) # Or use the QQplot to see if residuals are normally distributed
# Residuals are normally distributed

# 2. Linearity of the relationship 
plot(mod3, which = 1) # Line is kind of horizontal, so we can assume linearity of the relationship (Predicted against Residuals)
pred1 <- predict( object = mod3 ) # Predicted against actual values 
plot( x = pred1, y = data_sample_new1$pain, xlab = "Fitted Values", ylab = "Observed Values")
residualPlots( model = mod3 ) # Residual Plot for each predictor
# Relationships look linear

# 3. Checking Homoscedasticty (homogenity of variance)
plot(mod3, which = 3) # Line is kind of horizontal, to make sure, check assumption with more formal approach
ncvTest(mod3) # More formal approach to check assumption: p > .05, so we can assume homogenity 
# We can assume homogenity of variance

# 4. Checking Collinearity 
vif(mod = mod3) # For all measures, the VIF is <3
# We can assume that the variables are not collinear 

### All assumptions are checked and fine! mod3 is the final model ###

### Compare model 1 with model 3 to see if substantial new information was gained
# Using anova
anova(mod1, mod3)
# Using AIC
AIC(mod1)
AIC(mod3)
# Adjusted R^2
summary(mod1)
summary(mod3)

### Zoltan Home Assignment Part 2 ### 

### Create a new model (initial model, mod4): age, sex, weight, STAI, pain catastrophizing, mindfulness, serum_cortisol
mod4_initial <- lm(pain ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_new1)

### Descriptive statistics 
who(TRUE)
describe(data_sample_new1)
summary(data_sample_new1)
# Using same sample as before (with excluded cases)

### Model diagnostics on initial (full) model

# Look for influential outliers (Cook's Distance)
summary(mod4_initial)
mod4_initial
AIC(mod4_initial)
lm.beta(mod4_initial)
confint(mod4_initial)
## Check for influential outliers 
cooks.distance(mod4_initial) # Values here not salient in other checks, outliers are not being removed because
# in Psychology and from looking at the data those extreme values are fine 
# Plotting Cook's Distance 
plot(mod4_initial, which = 5) 
plot(mod4_initial, which = 4) 

# 1. Normality of the residuals 
hist( x = residuals(mod4_initial), xlab = "Value of residual", breaks = 20) # Histogram of Residuals looks normally distributed
shapiro.test(residuals(mod4_initial)) # p-value > .05 speaks for a normal distribution 
plot(mod4_initial, which = 2) # Or use the QQplot to see if residuals are normally distributed
# Residuals are normally distributed

# 2. Linearity of the relationship 
plot(mod4_initial, which = 1) # Line is kind of horizontal, so we can assume linearity of the relationship (Predicted against Residuals)
pred2 <- predict( object = mod4_initial ) # Predicted against actual values 
plot( x = pred2, y = data_sample_new1$pain, xlab = "Fitted Values", ylab = "Observed Values")
residualPlots( model = mod4_initial ) # Residual Plot for each predictor
# Relationships look linear

# 3. Checking Homoscedasticty (homogenity of variance)
plot(mod4_initial, which = 3) # Line is close to being horizontal but should be fine, to make sure, check more formally again
ncvTest(mod4_initial) # More formal approach to check assumption: p > .05, so we can assume homogenity 
# We can assume homogenity of variance

# 4. Checking Collinearity 
vif(mod = mod4_initial) # For all measures, the VIF is <3
# We can assume that the variables are not collinear 

### Assumptions are fine - we can continue using mod4_initial

### Creating the backward model 
backward_mod <- step(mod4_initial, direction = "backward") # Look which one it is in output 

# Analysis shows that age, pain_cat, mindfulness and cortisol_serum are the best predictors for pain (AIC = 11.8)

summary(backward_mod)
backward_mod
AIC(backward_mod)
lm.beta(backward_mod)
confint(backward_mod)
## Check for influential outliers 
cooks.distance(backward_mod) # Values here not salient in other checks, outliers are not being removed because
# in Psychology and from looking at the data those extreme values are fine 
# Plotting Cook's Distance 
plot(backward_mod, which = 5) 
plot(backward_mod, which = 4) 

### Comparing the models 
## theory_based and backward
mod3 <- lm(pain ~ age + sex + STAI_trait + pain_cat + mindfulness + cortisol_serum, data = data_sample_new1)
mod3 -> theory_based_mod

# Using AIC
AIC(theory_based_mod) # 460.0452  
AIC(backward_mod) # 456.5106; Lower AIC and more than 2 points difference: This is the better model
# Using Anova
anova(theory_based_mod, backward_mod) # Significant difference in variance between two models 
# Comparing adjusted R^2
summary(theory_based_mod) # 0.4315
summary(backward_mod) # 0.4374; explains more 

## backward and mod4_initial
# Using AIC
AIC(backward_mod) # 456.5106; Smaller value and difference >2
AIC(mod4_initial) # 461.1939
# Using anova
anova(backward_mod, mod4_initial) # No significant difference
# Comparing R^2 
summary(backward_mod) # 0.4374: explains a bit more, but not significantly 
summary(mod4_initial) #  0.4308

### Test both models on new data 
data_sample_2 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_2.csv") # Upload new data set

## Check data 
who(TRUE)
summary(data_sample_2)
describe(data_sample_2)

# Exclude ID = 26, 30, 43, 78, 93, 158 because of wrong value in mindfulness
data_sample_2 -> data_sample_2_new
data_sample_2_new=data_sample_2[!data_sample_2$mindfulness<=1,]

# Histgoram to illustrate the respective distributions
hist(data_sample_2_new$pain, breaks = 20)
hist(data_sample_2_new$age, breaks = 20)
hist(data_sample_2_new$STAI_trait, breaks = 20)
hist(data_sample_2_new$pain_cat, breaks = 20)
hist(data_sample_2_new$cortisol_serum, breaks = 20)
hist(data_sample_2_new$cortisol_saliva, breaks = 20)
hist(data_sample_2_new$mindfulness, breaks = 20)
hist(data_sample_2_new$weight, breaks = 20)

# Boxplots to look for outliers 
boxplot(data_sample_2_new$pain)
boxplot(data_sample_2_new$age)
boxplot(data_sample_2_new$STAI_trait) 
boxplot(data_sample_2_new$pain_cat)
boxplot(data_sample_2_new$cortisol_serum) 
boxplot(data_sample_2_new$cortisol_saliva) 
boxplot(data_sample_2_new$mindfulness) 
boxplot(data_sample_2_new$weight) 

### Make predictions on pain using the regression models/equations 
predict_back<-predict(backward_mod, newdata = data_sample_2_new)
predict_back
predict_theory<-predict(theory_based_mod, newdata = data_sample_2_new)
predict_theory

# Regression Equation Backward Model
backward_mod # Y = 4.99377 + (-0.08922*X1) + (0.05318*X2) + (-0.27109*X3) + (0.56686*X4)
theory_based_mod # Y = 4.959014 + (-0.087976*X1) + (0.121050*X2) + (0.002519*X3) + (0.048727*X4) + ( -0.282277*X5) + (0.565881*X6)

### Compare the models (which model makes less mistakes on test data)
# Compare actual and predicted values
predict_theory <- predict( object = theory_based_mod, newdata = data_sample_2_new)
plot( x = predict_theory, y = data_sample_2_new$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")

predict_back <- predict( object = backward_mod, newdata = data_sample_2_new)
plot( x = predict_back, y = data_sample_2_new$pain, 
      xlab = "Fitted Values", ylab = "Observed Values")

# RSS Errors to see which model predicts better
RSS_theory = sum((data_sample_2_new$pain - predict_theory)^2)
RSS_theory # 230.0634
RSS_backward = sum((data_sample_2_new$pain - predict_back)^2)
RSS_backward # 232.2393
# theory based model makes less mistakes

# Run anova
anova(theory_based_mod, backward_mod) # No significant difference between the two 

### Zoltan Home Assingment Part 3 ### 

### Load function to extract standardized beta coefficients from mer models, such as one produced by lmer
stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object,"y"))
  sdx <- apply(getME(object,"X"), 2, sd)
  sc <- fixef(object)*sdx/sdy
  se.fixef <- coef(summary(object))[,"Std. Error"]
  se <- se.fixef*sdx/sdy
  return(data.frame(stdcoef=sc, stdse=se))
}

### Load packages 
require(lme4) 
require(lmerTest) 
require(cAIC4)

### Load data file
data_sample_3 = read.csv("https://raw.githubusercontent.com/kekecsz/PSYP13_Data_analysis_class/master/home_sample_3.csv")
who(TRUE) # fine
describe(data_sample_3) 
summary(data_sample_3) 

# Histograms to show the distribution 
hist(data_sample_3$pain1, breaks = 20) 
hist(data_sample_3$pain2, breaks = 20) 
hist(data_sample_3$pain3, breaks = 20) 
hist(data_sample_3$pain4, breaks = 20) 
# None is normally distributed

# Boxplots to check for outliers 
boxplot(data_sample_3$pain1) 
boxplot(data_sample_3$pain2) 
boxplot(data_sample_3$pain3) 
boxplot(data_sample_3$pain4) 

### Show variables
names(data_sample_3)

## And designate which are the repeated measures
repeated_variables = c("pain1",	"pain2", "pain3",	"pain4")

### Correlation of repeated variables
cor(data_sample_3[,repeated_variables]) # Correlation lower the more time between ratings

### Transform data from wide to long format (Pain ratings not repeated but their own measures)
data_sample_3_long = melt(data_sample_3, measure.vars=repeated_variables, variable.name = "time", value.name = "Pain_Rating")
# Order data frame by participants to make it look more intuitive
data_sample_3_long = data_sample_3_long[order(data_sample_3_long[,"ID"]),]
# Change the time variable to a numerical vector
data_sample_3_long$time = as.numeric(data_sample_3_long$time)

### How does data in long format look like
data_sample_3_long

### Build the mixed effect models
# Intercept model containing fixed effects and a random intercept for participant ID 
mod_int = lmer(Pain_Rating ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + time + (1|ID), data = data_sample_3_long)
# Slope model containing fixed effects, a random intercept as well as a random slope 
mod_slope = lmer(Pain_Rating ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + time + (time|ID), data = data_sample_3_long) 

mod_int
mod_slope

summary(mod_int)
summary(mod_slope)

stdCoef.merMod(mod_int)
stdCoef.merMod(mod_slope)

### Compare models to decide which one to use
## Plot the regression line (prediction)
# Save predictions of models to variables
data_sample_3_long$pred_int = predict(mod_int)
data_sample_3_long$pred_slope = predict(mod_slope)
# Random intercept model
ggplot(data_sample_3_long, aes(y = Pain_Rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='red', aes(y=pred_int, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# random slope and intercept model
ggplot(data_sample_3_long, aes(y = Pain_Rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='blue', aes(y=pred_slope, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# Relationship could be curved instead of linear 
# Slope model looks a bit better

## Use cAIC to compare the models
cAIC(mod_int)$caic # 211.3871
cAIC(mod_slope)$caic # 175.8321
# mod_slope has the way better cAIC

## Use anova
anova(mod_int, mod_slope)
# Significantly different from each other

### Use random slope model as this is sifnificantly better

### Try to see if the curved relationship is better than the linear one by adding a quadratic term 
mod_slope_quadr = lmer(Pain_Rating ~ age + sex + weight + STAI_trait + pain_cat + mindfulness + cortisol_serum + time + I(time^2) + (time|ID), data = data_sample_3_long) 
summary(mod_slope_quadr)
stdCoef.merMod(mod_slope_quadr)
confint(mod_slope_quadr)
mod_slope_quadr
r2beta(mod_slope_quadr)
?r2beta

## Plot the results
# Save prediction to a new variable
data_sample_3_long$pred_slope_quadr = predict(mod_slope_quadr)
# Plotting
ggplot(data_sample_3_long, aes(y = Pain_Rating, x = time, group = ID))+
  geom_point(size = 3)+
  geom_line(color='green', aes(y=pred_slope_quadr, x=time))+
  facet_wrap( ~ ID, ncol = 5)
# Looks like a better fit than before

## Compare models
# cAIC
cAIC(mod_slope)$caic # 175.8321
cAIC(mod_slope_quadr)$caic # 121.9584
# mod_slope_quadr has the better cAIC value

# Use anova
anova(mod_slope, mod_slope_quadr)
# There's a significant difference between the two models

### Based on the results the random slope model including the quadratic term of time is the best choice

### Model diagnostics 
## Checking for influential outliers
influence(mod_slope_quadr, group = "ID")$alt.fixed # No influential effects: Values are always quite similar to each other 
influence(mod_slope_quadr, obs = T)$alt.fixed

## Checking assumptions
# Normality
qqmath(mod_slope_quadr, id=0.05) # QQ plot
# Output looks quite normal

# Linearity
# Linearity of prediction and standardized residuals
plot(mod_slope_quadr) # How is it supposed to look? 
# Linearity of each predictor and the standardized residual
# visualize the linearity of connection of each predictor
predictors=c("age", "STAI_trait", "weight", "pain_cat", "mindfulness", "cortisol_serum", "time")
par(mfrow=c(3,2))for(i in 1:length(predictors)){
  predictor_to_test = data_sample_3_long[,predictors[i]]
  print(
    ggplot(data.frame(x = predictor_to_test,pearson=residuals(mod_slope_quadr,type="pearson")),
           aes(x=x,y=pearson)) +
      geom_point() +
      geom_smooth(method = 'loess') +
      theme_bw())}
# relationships for all variables linear

# Homoscedasticty 
# Look for funnel shape on this graph
plot(mod_slope_quadr) # No funnel shape
# Levene testing for Heteroscedasticity
summary(lm(residuals(mod_slope_quadr)^2 ~ data_sample_3_long[,"ID"]))
# p>.05, not significant so there is no heteroscedasticity but we can assume homoscedasticity

# Multicollinearity
pairs.panels(data_sample_3_long[,c("sex", "age", "STAI_trait", "weight", "pain_cat", "mindfulness", "cortisol_serum", "time")], col = "red", lm = T)













