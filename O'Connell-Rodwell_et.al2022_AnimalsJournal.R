#### Hormonal and Age Driven Responses of Male African Elephants (Loxodonta africana) to Estrous Call Playbacks Inform Conservation Management Tools ####
#### Authors: Caitlin E. O'Connell-Rodwell, Monica N. Sandri, Jodie L. Berezin, Jaquelyn M. Munevar, Colleen Kinzley, Jason D. Wood, Maggie Wisniewska, and Werner Kilian ####
#### The scripts below can be used to replicate all statistical modeling and conclusions reported in the manuscript using the appropriate data sets. ####
#### Questions regarding the data or scripts can be directed to: Monica N. Sandri at monica.n.sandri[at]gmail.com and Jodie L. Berezin at jberezin[at]smith.edu####

### Set Working Directory ###
getwd()

### Libraries ###
library(dplyr)
library(tidyverse)
library(rstatix)
library(readr)
library(stringr)
library(ggpubr)
library(car)
library(stats)
library(irr)
library(PairedData)
library(ggplot2)
library(performance)
library(mgcv)

##### Model 1: Testosterone Analysis #1 - Testing mean differences of testosterone levels between age classes in the non-musth 1Q-3Q male elephant group #####
### Data import and preparation ###

# load Mushara sample data frame with n = 19 males in the 1Q-3Q non-musth group
testosterone1Q_3Q <- read_csv("add in data name") 
# testosterone data not publicly available due to an ongoing hormone study of this population. See R Markdown PDF for results of these analyses.

# adjusting variable types
testosterone1Q_3Q$AgeClass <- as.factor(as.character(testosterone1Q_3Q$AgeClass))

# log-transform testosterone averages (in order to meet all assumptions)

testosterone1Q_3Q.log <- testosterone1Q_3Q
testosterone1Q_3Q.log$Log_T <- log(testosterone1Q_3Q.log$TestosteroneAVG)

### Analysis and results ###

# ANOVA

test1Q_3Q_anova <- aov(Log_T ~ AgeClass, testosterone1Q_3Q.log)
summary(test1Q_3Q_anova) 
# not significant p = 0.241, suggesting that non-musth males yet to meet their reproductive prime (1Q-3Q age class) are hormonally similar as well. 

# Tukey post-hoc test
TukeyHSD(test1Q_3Q_anova) 
# no significant between group comparisons detected

### Check assumptions ###

## Check for outliers 
log.t.outliers.1Q.3Q <- testosterone1Q_3Q.log %>% 
  group_by(AgeClass) %>%
  identify_outliers(Log_T) #no outliers detected

## Check for normality

# Build the linear model
model_logt_1Q_3Q  <- lm(Log_T ~ AgeClass, data = testosterone1Q_3Q.log)

# Create a QQ plot of residuals
ggqqplot(residuals(model_logt_1Q_3Q)) #QQ Plot looks normal

plot(test1Q_3Q_anova, 2) #Normal with a slightly heavier lower tail, but nothing too concerning

# Histogram
hist(residuals(model_logt_1Q_3Q)) # slightly right skewed, but looks great overall

# Compute Shapiro-Wilk Test of Normality
shapiro_test(residuals(model_logt_1Q_3Q)) # p-value = 0.72729, so we can assume normal distribution of the residuals.


## Check for homogeneity of variance
plot(test1Q_3Q_anova, 1) # homogeneous

leveneTest(Log_T ~ AgeClass, data = testosterone1Q_3Q.log) # p-value = 0.7183, so can assume homogeneity!


##### Model 2: Testosterone Analysis #2 - Testing mean differences of testosterone levels between three male elephant demographic groups #####
### Data import and preparation ###

# load data frame
testosterone <- read_csv("add in data name") 
# testosterone data not publicly available due to an ongoing hormone study of this population. See R Markdown PDF for results of these analyses.

# adjusting variable types
testosterone$ElephantID <- as.factor(as.character(testosterone$ElephantID))
testosterone$MaleType <- as.factor(as.character(testosterone$MaleType))

# log-transform testosterone averages (in order to meet all assumptions)

testosterone.log <- testosterone
testosterone.log$Log_T <- log(testosterone.log$TestosteroneAVG)

### Analysis and results ###

# ANOVA
t.anova <- aov(Log_T ~ MaleType, data=testosterone.log)

summary(t.anova) 
#significant difference between male types p = 3.37e-05 *** 

# Tukey post-hoc test
TukeyHSD(t.anova) 
# significant difference between musth 4Q and non-musth 4Q (p=0.0000269), musth 4Q and non-musth 1Q-3Q (p=0.0156206), non-musth 4Q-non-musth 1Q-3Q group (p=0.0058896)

### Check assumptions ###

## Check for outliers 
t.log.outliers <- testosterone.log %>% 
  group_by(MaleType) %>%
  identify_outliers(Log_T) # three outliers detected, but will keep if normality assumptions are not violated

## Check for normality

# Build the linear model
model_t_log <- lm(Log_T ~ MaleType, data = testosterone.log)

# Create a QQ plot of residuals
ggqqplot(residuals(model_t_log)) # QQ Plot looks normal!

plot(t.anova, 2) # Normal with three outliers, but closer to the line of best fit

# Histogram
hist(residuals(model_t_log)) # normal!

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model_t_log)) # p-value = 0.9956728, so we can assume normal distribution of the residuals

## Check for homogeneity of variance

plot(t.anova, 1) # looks good, and appears unbiased and homoscedastic

leveneTest(Log_T ~ MaleType, data = testosterone.log) # p-value = 0.1514, so can assume homogeneity


##### Model 3: Inter-rater reliability of response scores collected in the field and ex-situ from observational records #####
### Data import and preparation ###

# load data frame
irr.raters <- read_csv("add in data name") #1.IRR - Table S1

### Analysis and results ###

icc(irr.raters, model = "twoway", type = "agreement", unit = "average")
# ICC = 0.908


##### Model 4: Linear Regressions - Testing the predictive power of testosterone and male group on response score #####
### Data import and preparation ###

# load data frame
T.malegroup <- read_csv("add in data name") 
# testosterone data not publicly available due to an ongoing hormone study of this population. See R Markdown PDF for results of these analyses.

# adjusting variable types
T.malegroup$MaleType <- as.factor(as.character(T.malegroup$MaleType))

#### Model 4a: Simple Linear Regression ####
### Analysis and results ###

# Simple linear regression
simple_reg_model <- gam(ResponseScore ~ Testosterone, data = T.malegroup) # used "gam" function from mgcv package for easy comparisons with GAM models, outlined in detail in accompanying R Markdown PDF.
summary(simple_reg_model)
# testosterone not a significant predictor of response score (p = 0.509)

### Check assumptions ###

check_model(simple_reg_model, check=c("vif", "qq", "normality", "linearity", "homogeneity", "outliers"))
# Mild violations of linearity and homogeneity, as well as an extreme outlier.

## Outlier Evaluation
simple_reg_model_diagnostics <- ls.diag(simple_reg_model)

# Leverage
simple_reg_model_diagnostics$hat
# Leverage should be considered moderately unusual when above 2(k+1)/n and very unusual when above 3(k+1)/n, where k is the number of predictors and n is the number of data points
# In the case of our model, Moderate: 2(2+1)/19 = 0.3156; Very: 3(2+1)/19 = 0.4737
# We have one data point over this threshold with a leverage value of 0.5998

# Standardized Residuals
simple_reg_model_diagnostics$std.res
# Standardized residuals should be considered moderately unusual when beyond +/- 2, and very unusual when beyond +/- 3
# We have one data point at -2.5686

# Cook's D
simple_reg_model_diagnostics$cooks
# Cook's D should be considered moderately unusual when beyond 0.5 and very unusual when beyond 1
# We have one data point with a value of 4.9449

## upon evaluation of which data points represented these values, it was revealed that all three were the same data point. Because of the extreme nature of this data point, we have removed it from proceeding linear regression models outlined in this section (section 4).

### Analysis re-run without the outlier ##
T.malegroup.nooutlier <- read_csv("add in data name")
# testosterone data not publicly available due to an ongoing hormone study of this population. See R Markdown PDF for results of these analyses.

# adjusting variable types
T.malegroup.nooutlier$MaleType <- as.factor(as.character(T.malegroup.nooutlier$MaleType))

## Simple Linear Regression
simple_reg_nooutlier <- gam(ResponseScore ~ Testosterone, data = T.malegroup.nooutlier)
summary(simple_reg_nooutlier)
# testosterone a significant predictor of response score (p = 0.00911)

### Check assumptions ###
check_model(simple_reg_nooutlier, check=c("vif", "qq", "normality", "linearity", "homogeneity", "outliers"))
# slight violations of linearity and homogeneity of variance. Another outlier detected but it was left in due to lack of severity (see R Markdown PDF for details).

#### Model 4b: Additive Linear Regression ####
T.malegroup.nooutlier # same data set as Models 4a

### Analysis and results ###
additive_model <- gam(ResponseScore ~ Testosterone + MaleType, T.malegroup.nooutlier)
summary(additive_model)
# MaleGroup:Non-musth4Q significant (p = 0.0068)

### Check assumptions ###
check_model(additive_model, check=c("vif", "qq", "normality", "linearity", "homogeneity", "outliers"))
# moderate violations of linearity and homogeneity of variance
# Attempts were made to address these violations including log transformations of the response and/or predictor variables, quadratic models, inversing testosterone and utilization of smoothing with GAM models. None of these improved the assumptions violations, therefore we kept the linear additive model as the final model. See R Markdown PDF for details.

### Model Comparisons: Simple linear regression and additive linear regression ###
anova(simple_reg_nooutlier, additive_model)
# additive model significantly better at explaining variation in response score (p = 0.0085)


##### Model 5: Assessing differences in response score across three male elephant groups #####
### Data import and preparation ###

# load data frame
response.scores <- read_csv("add in data name") #2. Response Scores - Table S2

# adjust variable types
response.scores$MaleType <- as.factor(as.character(response.scores$MaleType))

### Analysis and results ###

# ANOVA
response_anova <- aov(ResponseScore ~ MaleType, response.scores)

summary(response_anova) 
# significant difference between male types: p=0.00522**

# Tukey post-hoc test
TukeyHSD(response_anova) 
# significant differences between musth 4Q and non-musth 4Q (p=0.009), and between the non-musth 1Q-3Q group and non-musth 4Q (p=0.012)

### Check assumptions ###

## Check for outliers 
response.scores %>% 
  group_by(MaleType) %>%
  identify_outliers(ResponseScore)
# two outliers detected, but were kept given that ANOVAs are robust against outliers as long as assumptions of normality are not violated (see below)

##Check for normality

# Build the linear model
model_RS <- lm(ResponseScore ~ MaleType, data = response.scores)

# Create a QQ Plot of Residuals
ggqqplot(residuals(model_RS)) # QQ Plot looks normal

plot(model_RS, 2) # normal with a couple outliers

# Histogram
hist(residuals(model_RS)) # somewhat irregular

# Compute Shapiro-Wilk test of normality
shapiro_test(residuals(model_RS)) # p-value = 0.1419, so we can assume normal distribution of the residuals

## Check for homogeneity of variance

plot(model_RS, 1) # appears unbiased and homoscedastic

leveneTest(ResponseScore ~ MaleType, data = response.scores) # p-value = 0.6093, so assume homogeneity

##### Model 6: Assessing if males elephants increase their rate of defecation after hearing estrous playbacks #####
### Data import and preparation ###

# load data frame
defecation.rates <- read_csv("add in data name") # 3.Defecation Rates of n = 18 males - Table S3

# adjust variable types
defecation.rates$Time <- factor(defecation.rates$Time, levels = c("before", "after"))


### Analysis and results ###

# Wilcoxon signed-rank test
def.rates.test <- wilcox.test(defecation.rates$DefecationRate ~ defecation.rates$Time, 
                                paired = TRUE, 
                                alternative = "lesser", 
                                exact = TRUE) 
# alternative = less used to test if defecation rates were lower before the estrous playback
# exact p-value recommended for small sample sizes
# significant difference in rate between time periods (p=0.002003)

# Effect size
def.score.effect <- wilcox_effsize(DefecationRate ~ Time, 
                                    data = defecation.rates, 
                                    paired = TRUE, 
                                    alternative = "less", 
                                    exact = TRUE)
# effect size r = 0.65182 (large magnitude)

### Check assumptions ###

# Compute the difference between paired sets

defecation_rate_diff <- with(defecation.rates,
                             DefecationRate[Time == "after"] - DefecationRate[Time == "before"])

#Shapiro-Wilk normality test for the differences

shapiro.test(defecation_rate_diff) # p-value = 0.0001019, so can't assume normality

# Histogram
hist(defecation_rate_diff) # fairly normal, but slightly right skewed

# QQ plot
qqnorm(defecation_rate_diff) #looks very abnormal

# Check for outliers
def.outliers <- defecation.rates %>% 
  group_by(Time) %>%
  identify_outliers(DefecationRate) # multiple outliers detected, though this may be due to the small data set

##### Model 7: Assessing if the rate of musth behaviors increased after hearing the estrous playbacks in musth 4Qs #####
#### Model 7a: All musth males ####
### Data import and preparation ###

# load data frame
musth.behav.all <- read_csv("insert data set") # 4.Musth Male Behaviors (for all musth 4Qs, regardless of response score)

# adjust variable types
musth.behav.all <- factor(musth.behav.all$Time, levels = c("before", "after"))

### Analysis and results ###

# Wilcoxon signed-rank test
musth.all.test <- wilcox.test(MusthRate ~ Time, 
                              data = musth.behav.all, 
                              paired = TRUE, 
                              exact = TRUE, 
                              alternative = "less") 
# alternative = less used to test if musth behavior rates were lower before the estrous playback
# exact p-value recommended for small sample sizes
# not significant (p=0.5)

# Effect size (small)
musth.all.effect <- wilcox_effsize(MusthLog ~ Time, 
                                   data = musth.behav.all, 
                                   paired = TRUE,
                                   exact = TRUE, 
                                   alternative = "less") 
#effect size r = 0.06030227 (small magnitude)

### Checking assumptions ###

# Compute the difference between paired sets

musth_rate_diff <- with(musth.behav.all,
                        MusthRate[TimePeriod == "before"] - MusthRate[TimePeriod == "after"])

# Shapiro-Wilk normality test for the differences

shapiro.test(musth_rate_diff) # p-value = 0.2092 indicating normality

# QQ plot

qqnorm(musth_rate_diff) # somewhat abnormal

# Historgram

hist(musth_rate_diff) # appears left skewed 

# Check for outliers
musth.rate.outliers <- musth.behav.all %>% 
  group_by(TimePeriod) %>%
  identify_outliers(MusthRate) #no outliers detected

#### Model 7b: Evaluating musth behavior rates in musth males that responded with a mean score of 1 during playback trials####
### Data import and preparation ###

# load data frame
musth.behav.subgroup <- read_csv("add in file name") # 5.Musth Male Behaviors Subgroup

# adjust variable types
musth.behav.subgroup <- factor(musth.behav.subgroup$Time, levels = c("before", "after"))


### Analysis and results ###

# Wilcoxon signed-rank test
musth.subgroup.test <- wilcox.test(MusthRate ~ Time, 
                                   data = musth.behav.subgroup, 
                                   paired = TRUE, 
                                   exact = TRUE, 
                                   alternative = "less") 
# alternative = less used to test if musth behavior rates were lower before the estrous playback
# exact p-value recommended for small sample sizes
# not significant (p=0.3125)

# Effect size
musth.subgroup.effect <- wilcox_effsize(RateLog ~ Time, 
                                        data = musth.behav.subgroup, 
                                        paired = TRUE, 
                                        alternative = "less", 
                                        exact = TRUE)  
# effect size r = 0.3651484 (moderate magnitude)

### Checking assumptions ###

# Compute the difference between paired sets

musth_rate_diff_2 <- with(musth.behav.subgroup,
                            MusthRate[TimePeriod == "before"] - MusthRate[TimePeriod == "after"])

# Shapiro-Wilk normality test for the differences

shapiro.test(musth_rate_diff_2) # p-value = 0.009017 so can't assume normality

# QQ plot

qqnorm(musth_rate_diff_2) # though there are very few data points, plot does not look normal

# Historgram

hist(musth_rate_diff_2) # looks left skewed 

# Check for outliers

musth1.rate.outliers <- musth.behav.subgroup %>% 
  group_by(TimePeriod) %>%
  identify_outliers(MusthRate) #no outliers detected
