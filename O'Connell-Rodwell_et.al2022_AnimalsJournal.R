#### Male African Elephant (Loxodonta africana) Behavioral Responses to Estrous Call Playbacks May Inform Conservation Management Tools ####
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


##### Model 1: Inter-rater reliability of response scores collected in the field and ex-situ from observational records #####
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


##### Model 2: Assessing differences in response score across three male elephant groups #####
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

##### Model 3: Assessing if males elephants increase their rate of defecation after hearing estrous playbacks #####
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

##### Model 4: Assessing if the rate of musth behaviors increased after hearing the estrous playbacks in musth 4Qs #####
#### Model 4a: All musth males ####
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

#### Model 4b: Evaluating musth behavior rates in musth males that responded with a mean score of 1 during playback trials####
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
