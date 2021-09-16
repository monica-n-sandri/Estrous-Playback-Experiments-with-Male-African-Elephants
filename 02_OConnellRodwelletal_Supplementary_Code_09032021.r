#### Reproductive Motivations in Male African Elephants May Serve as a Tool in Conflict Mitigation ####
#### Authors: Caitlin E. O'Connell-Rodwell, Monica N. Sandri, Jodie L. Berezin, Jaquelyn M. Munevar, Colleen Kinzley, Joyce H. Poole, and Jason D. Wood ####
#### The scripts below can be used to replicate all statistical modeling and conclusions reported in the manuscript using the appropriate data sets. ####
#### Questions regarding the data or scripts can be directed to: Monica N. Sandri at monica.n.sandri[at]gmail.com ####

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

##### Model 1: Testing mean differences of testosterone levels between three male elephant types #####
### Data import and preparation ###

# load data
testosterone <- read_csv("add in data name") 
# testosterone data are not yet publicly available due to an ongoing hormone study of this population.

# adjusting variable types
testosterone$ElephantID <- as.factor(as.character(testosterone$ElephantID))
testosterone$MaleType <- as.factor(as.character(testosterone$MaleType))

### Analysis and results ###

# ANOVA
t.anova <- aov(TestosteroneAVG ~ MaleType, data=testosterone)

summary(t.anova) 
#significant difference between male types p=9.64e-05*** 

# Tukey post hoc test
TukeyHSD(t.anova) 
# significant difference between musthadult-adult (p=0.0000573) and musthadult-subadult (p=0.0025)


##### Model 2: Inter-rater reliability of response scores collected in the field and ex-situ from observational records #####
### Data import and preparation ###

# load data
irr.raters <- read_csv("add in data name") #1.IRR

### Analysis and results ###

icc(irr.raters, model = "twoway", type = "agreement", unit = "average")
# ICC = 0.908


##### Model 3: Assessing differences in response score across three male elephant types #####
### Data import and preparation ###

# load data
response.scores <- read_csv("add in data name") #2. Response Scores (convert to .csv file first)

# adjust variable types
response.scores$MaleType <- as.factor(as.character(response.scores$MaleType))

### Analysis and results ###

# ANOVA
response_anova <- aov(ResponseScore ~ MaleType, response.scores)

summary(response_anova) 
# significant difference between male types: p=0.00522**

# Tukey post hoc
TukeyHSD(response_anova) 
# significant differences between musthadult-adult (p=0.009) and subadult-adult (p=0.012)


##### Model 4: Assessing if males of different response intensity increase their rate of defecation after hearing estrus playbacks #####
#### Model 4a: Males with a response score = 1 ####
### Data import and preparation ###

# load data
defecation.rates.1 <- read_csv("insert data set") # 3a.def rates =1 

# adjust variable types
defecation.rates.1$Time <- factor(defecation.rates.1$Time, levels = c("before", "after"))

### Analysis and results ###

def.rates.1.ttest <- t.test(DefecationRate(Log) ~ Time, data = defecation.rates.1, paired = TRUE, alternative = "less") # alternative = less used to test if defecation rates were lower before the estrus playback
# significant difference in rate between time periods (p=0.01089)

#### Model 4b: Males with a response < 1 ####
### Data import and preparation ###

# load data
defecation.rates.less1 <- read_csv("insert data set") # 3b.def rates <1

# adjust variable types
defecation.rates.less1$Time <- factor(defecation.rates.less1$Time, levels = c("before", "after"))

### Analysis and results ###

def.rates.less1.ttest <- t.test(DefecationRate(Log) ~ Time, data = defecation.rates.less1, paired = TRUE, alternative = "less") # alternative = less used to test if defecation rates were lower before the estrus playback
# not significant (p=0.1087)


##### Model 5: Assessing if the rate of musth behaviors increased after hearing the estrus playbacks in musthadults #####
#### Model 5a: All musth males ####
### Data import and preparation ###

# load data
musth.behav.all <- read_csv("insert data set") # 4a.musth behaviors with outlier musth adult (mean response score = 0.2815)

# adjust variable types
musth.behav.all <- factor(musth.behav.all$Time, levels = c("before", "after"))

# log transform the rates
musth.behav.all$RateLog <- log(musth.behav.all$MusthRate)
View(musth.behav.all)

### Analysis and results ###

musth_all_ttest <- t.test(RateLog ~ Time, data = musth.behav.all, paired = TRUE, alternative = "less") # alternative = less used to test if musth behavior rates were lower before the estrus playback
# not significant (p=0.725)

#### Model 5b: Subgroup of musth males ####
### Data import and preparation ###

# load data
musth.behav.subgroup <- read_csv("insert data set") # 4b.musth behaviors without outlier musth adult (mean response score = 0.2815)

# adjust variable types
musth.behav.subgroup <- factor(musth.behav.subgroup$Time, levels = c("before", "after"))

# log transform the rates
musth.behav.subgroup$RateLog <- log(musth.behav.subgroup$MusthRate)
View(musth.behav.subgroup)

### Analysis and results ###

musth_subgroup_ttest <- t.test(RateLog ~ Time, data = musth.behav.subgroup, paired = TRUE, alternative = "less") # alternative = less used to test if musth behavior rates were lower before the estrus playback
# not significant (p=0.1657), but trend shows slight increase in musth rate
