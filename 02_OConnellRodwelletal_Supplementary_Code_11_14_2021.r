#### Hormonal and Age Driven Responses of Male African Ele-phants (Loxodonta africana) to Estrous Call Playbacks Inform Conservation Management Tools ####
#### Authors: Caitlin E. O'Connell-Rodwell, Monica N. Sandri, Jodie L. Berezin, Jaquelyn M. Munevar, Colleen Kinzley, Jason D. Wood, Maggie Wisniewska, and Werner Kilian ####
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
library (irr)

##### Model 1: Testing mean differences of testosterone levels between three groups of male elephants #####
### Data import and preparation ###

# load data
testosterone <- read_csv("add in data name") 
# testosterone data not publicly available due to an ongoing hormone study of this population

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

# Tukey post hoc test
TukeyHSD(t.anova) 
# significant difference between musth 4Q and non-musth 4Q (p=0.0000269), musth 4Q and non-musth 1Q-3Q (p=0.0156206), non-musth 4Q-non-musth 1Q-3Q group (p=0.0058896)


##### Model 2: Inter-rater reliability of response scores collected in the field and ex-situ from observational records #####
### Data import and preparation ###

# load data
irr.raters <- read_csv("add in data name") #1.IRR (convert to .csv file first)

### Analysis and results ###

icc(irr.raters, model = "twoway", type = "agreement", unit = "average")
# ICC = 0.908


##### Model 3: Assessing differences in response score across three male elephant groups #####
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
# significant differences between musth 4Q and non-musth 4Q (p=0.009), and between the non-musth 1Q-3Q group and non-musth 4Q (p=0.012)


##### Model 4: Assessing if males of different response intensity increase their rate of defecation after hearing estrus playbacks #####
#### Model 4a: Males with a response score = 1 ####
### Data import and preparation ###

# load data
defecation.rates.1 <- read_csv("insert data set") # 3a.Defecation Rates Score=1 (convert to .csv file first)

# adjust variable types
defecation.rates.1$Time <- factor(defecation.rates.1$Time, levels = c("before", "after"))

# log-transform defecation rates
defecation.rates.log.1 <- defecation_rates1
defecation.rates.log.1$RateLog <- log(defecation.rates.log.1$DefecationRate)

### Analysis and results ###

# Wilcoxon signed-rank test
def.rates.1.test <- wilcox.test(defecation.rates.log.1$RateLog ~ defecation.rates.log.1$Time, 
                             paired = TRUE, 
                             alternative = "lesser", 
                             exact = TRUE) 
# alternative = less used to test if defecation rates were lower before the estrous playback
# exact p-value recommended for small sample sizes
# significant difference in rate between time periods (p=0.01953)

# Effect size
def.score1.effect <- wilcox_effsize(RateLog ~ Time, 
                                    data = defecation.rates.log.1, 
                                    paired = TRUE, 
                                    alternative = "less", 
                                    exact = TRUE)
# effect size r = 0.7426107 (large magnitude)

#### Model 4b: Males with a response < 1 ####
### Data import and preparation ###

# load data
defecation.rates.less1 <- read_csv("insert data set") # 3b.Defecation Rates Score<1 (convert to .csv file first)

# adjust variable types
defecation.rates.less1$Time <- factor(defecation.rates.less1$Time, levels = c("before", "after"))

#log-transform
def.less1.log <- defecation.rates.less1
def.less1.log$RateLog <- log(def.less1.log$DefecationRate)

### Analysis and results ###

# Wilcoxon signed-rank test
def.rates.less1.test <- wilcox.test(RateLog ~ $Time, 
                                     data = def.less1.log,
                                     paired = TRUE, 
                                     alternative = "less", 
                                     exact = TRUE) 
# alternative = less used to test if defecation rates were lower before the estrous playback
# exact p-value recommended for small sample sizes
# significant difference rate between time periods (p=0.03223)

#effect size 
def.score.less1.effect <- wilcox_effsize(RateLog ~ Time, 
                                         data = def.less1.log, 
                                         paired = TRUE, 
                                         alternative = "less", 
                                         exact = TRUE)
#effect size r = 0.596309	(large magnitude)

##### Model 5: Assessing if the rate of musth behaviors increased after hearing the estrous playbacks in musth 4Qs #####
#### Model 5a: All musth males ####
### Data import and preparation ###

# load data
musth.behav.all <- read_csv("insert data set") # 4a.Musth Male Behaviors - for all musth 4Qs, regardless of response score (convert to .csv file first)

# adjust variable types
musth.behav.all <- factor(musth.behav.all$Time, levels = c("before", "after"))

# log transform the rates
musth.behav.all.log <- musth.behav.all
musth.behav.all.log$RateLog <- log(musth.behav.all.log$MusthRate)

### Analysis and results ###

# Wilcoxon signed-rank test
musth.all.test <- wilcox.test(RateLog ~ Time, 
                              data = musth.behav.all.log, 
                              paired = TRUE, 
                              exact = TRUE, 
                              alternative = "less") 
# alternative = less used to test if musth behavior rates were lower before the estrous playback
# exact p-value recommended for small sample sizes
# not significant (p=0.5938)

# effect size (small)
musth.all.effect <- wilcox_effsize(RateLog ~ Time, 
                                   data = musth.behav.all.log, 
                                   paired = TRUE,
                                   exact = TRUE, 
                                   alternative = "less") 
#effect size r = 0.06030227 (small magnitude)

#### Model 5b: Subgroup of musth males ####
### Data import and preparation ###

# load data
musth.behav.subgroup <- read_csv("insert data set") # 4b.Musth Male Behaviors Subgroup - one musth 4Q removed with response score of 0.2815 (convert to .csv file first)

# adjust variable types
musth.behav.subgroup <- factor(musth.behav.subgroup$Time, levels = c("before", "after"))

# log transform the rates
musth.subgroup.log <- musth.behav.subgroup
musth.subgroup.log$RateLog <- log(musth.subgroup.log$MusthRate)

### Analysis and results ###

# Wilcoxon signed-rank test
musth.subgroup.test <- wilcox.test(RateLog ~ Time, 
                                   data = musth.subgroup.log, 
                                   paired = TRUE, 
                                   exact = TRUE, 
                                   alternative = "less") 
# alternative = less used to test if musth behavior rates were lower before the estrous playback
# exact p-value recommended for small sample sizes
# not significant (p=0.3125), but trend shows slight increase in musth rate after playback compared to Model 5a

# effect size
musth.subgroup.effect <- wilcox_effsize(RateLog ~ Time, 
                                        data = musth.subgroup.log, 
                                        paired = TRUE, 
                                        alternative = "less", 
                                        exact = TRUE)  
# effect size r = 0.3651484 (moderate magnitude)
