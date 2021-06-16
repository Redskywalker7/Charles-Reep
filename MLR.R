library(tidyverse)
library(lmtest)
library(normtest)
library(car)

dat <- read_csv("la_liga_dataset.csv")

##### MODEL WITH ALL FORMATIONS AS INDICATOR VARIABLES #####

dat$Formation <- factor(dat$Formation)
test_lm_form_only <- lm(formula = Total_xg ~ Formation, data = dat)

summary(test_lm_form_only)

##### trying to create a model with grouped variables #####
# group by attackers for offensive models and by defenders for defensive models

datt <- dat
# save a copy of the original dataframe just for the sake of experimentation

# GROUPING BASED ON DEFENDERS
datt <- datt %>% 
  mutate(formtype_def = case_when(str_detect(datt$Formation, "^3") ~ "3",
                                  str_detect(datt$Formation, "^4") ~ "4",
                                  str_detect(datt$Formation, "^5") ~ "5"))
# create a new column "formtype_def" assigning a number to each formation based on 
# the number of defenders used in the lineup



##### grouping based on attackers #####

### GROUPING BASED ON ATTACKERS ###

## create a df with all formations of a given string length: ##

# select all formations with nchar = 3
char3 <- as.tibble(datt$Formation[stringr::str_length(datt$Formation) == 3]) %>% 
  select(everything()) %>% 
  rename(Formation = value)
char3 <- semi_join(datt, char3, by = c("Formation" = "Formation"))

# select all formations with nchar = 4
char4 <- as.tibble(datt$Formation[stringr::str_length(datt$Formation) == 4]) %>% 
  select(everything()) %>% 
  rename(Formation = value)
char4 <- semi_join(datt, char4, by = c("Formation" = "Formation"))

# select all formations with nchar = 5
char5 <- as.tibble(datt$Formation[stringr::str_length(datt$Formation) == 5]) %>% 
  select(everything()) %>% 
  rename(Formation = value)
char5 <- semi_join(datt, char5, by = c("Formation" = "Formation"))

## create an attacking formtype variable for each one ##
# selects the 3rd character from the string of 3 numbers and creates the new 
# variable "formtype_att"
char3 <- char3 %>% 
  mutate(formtype_att = str_sub(char3$Formation, 3, 3))
char3$formtype_att <- as.numeric(char3$formtype_att)

# selects the 3rd and 4th values from the string of 4 numbers and creates a new 
# variable in the dataframe for each one (formtype_att1 and formtype_att2)
char4 <- char4 %>% 
  mutate(formtype_att1 = str_sub(char4$Formation, 3, 3),
         formtype_att2 = str_sub(char4$Formation, 4, 4))

# changes formtype_att1 and 2 to numeric values to sum them
char4$formtype_att1 <- as.numeric(char4$formtype_att1)
char4$formtype_att2 <- as.numeric(char4$formtype_att2)

# creates a new variable formtype_att by summing formtype_att1 and 2, then drops
# those two columns/variables from the data frame
char4 <- char4 %>%
  mutate(formtype_att = formtype_att1 + formtype_att2) %>% 
  select(everything(), -formtype_att1, -formtype_att2)


# selects the 4th and 5th values from the string of 5 numbers and creates a new 
# variable in the dataframe for each one (formtype_att1 and formtype_att2)
char5 <- char5 %>% 
  mutate(formtype_att1 = str_sub(char5$Formation, 4, 4),
         formtype_att2 = str_sub(char5$Formation, 5, 5))

# changes formtype_att1 and 2 to numeric values to sum them
char5$formtype_att1 <- as.numeric(char5$formtype_att1)
char5$formtype_att2 <- as.numeric(char5$formtype_att2)

# creates a new variable formtype_att by summing formtype_att1 and 2, then drops
# those two columns/variables from the data frame
char5 <- char5 %>%
  mutate(formtype_att = formtype_att1 + formtype_att2) %>% 
  select(everything(), -formtype_att1, -formtype_att2)


## combine them all back into one big dataframe
# join the 3 and 4 character dfs
char3and4 <- full_join(char3, char4) 

# join the 5 character df to the one with the 3 and 4 characters joined together
allchar <- full_join(char3and4, char5) 

allchar$formtype_att <- as.factor(allchar$formtype_att)
allchar$formtype_def <- as.factor(allchar$formtype_def)


##### OFFENSIVE MODEL #####
lm_grouped <- lm(Total_xg ~ formtype_att + 
                 Shots_tg + 
                 pass_accuracy + 
                 possession_ratio, 
                 data = allchar)
# i included quite a few terms in the model to see what combo made sense and 
# and also gave a good R^2
# with only formtype_att, it has a pretty low R^2
# but with lots of terms formtype has a low significance


summary(lm_grouped)
# ok this is looking a lot better than the previous models
# has a slightly higher R^2 with a log transform on Total_xg than without 
# (difference is about ~.01 or so, so idk if it's really worth sacrifcing some 
# interpretability for that small of a gain in R^2)

# checking assumptions
# linearity
avPlots(lm_grouped)
# still slightly weird, not quite what you want to see on the formtype plots
# but ok everywhere else

# linearity, independence, homoscedasticity/equal variance
plot(lm_grouped$fitted.values, lm_grouped$residuals)
hist(lm_grouped$fitted.values)
hist(lm_grouped$residuals)
plot(lm_grouped$fitted.values, MASS::stdres(lm_grouped))
# kinda sketchy, don't quite meet requirements
bptest(lm_grouped) # gets a REALLY low p-value, H_0: heteroscedastic
# that's not good lol, implies very unequal variances

# normality
hist(MASS::stdres(lm_grouped), freq = FALSE, xlim = c(-4, 4))
curve(dnorm, from = -4, to = 4, add = TRUE) # looks great

### variable selection/checking VIFs ### 
# all completely unsuccessful so far lol

# # Check for collinearity
# car::vif(lm_grouped) # no (?)
# 
# # try BIC
# # first get a dataframe that eliminates the char variables
# 
# dat_nochar <- datt %>% 
#   dplyr::select(Formation:Goals, -Timestamp, xg_per_minute, 
#                 pass_accuracy, possession_ratio, formtype) %>% 
#   print()
# 
# dat_nochar$Period <- as.factor(dat_nochar$Period)
# dat_nochar$formtype <- as.factor(dat_nochar$formtype)
# 
# varselect <- bestglm::bestglm(dat_nochar, IC = "AIC",
#                               method = "backward",
#                               TopModels = 10)




##### DEFENSIVE MODEL #####
lm_grouped_against <- lm(Total_xg_against ~ formtype_def + # `Team Name` 
                         Shots_tg_against + 
                         pass_accuracy_against + 
                         possession_ratio_against, 
                         data = allchar)

summary(lm_grouped_against)

# checking assumptions for the "against" model
# linearity
avPlots(lm_grouped_against)
# still slightly weird, not quite what you want to see on the formtype plots
# but ok everywhere else

# linearity, independence, homoscedasticity/equal variance
plot(lm_grouped_against$fitted.values, lm_grouped_against$residuals)
hist(lm_grouped_against$fitted.values)
hist(lm_grouped_against$residuals)
plot(lm_grouped_against$fitted.values, MASS::stdres(lm_grouped_against))
# kinda sketchy, don't quite meet requirements
bptest(lm_grouped_against) # gets a REALLY low p-value, H_0: heteroscedastic
# that's not good lol, implies very unequal variances

# normality
hist(MASS::stdres(lm_grouped_against), freq = FALSE, xlim = c(-4, 4))
curve(dnorm, from = -4, to = 4, add = TRUE) # looks great

qqnorm(MASS::stdres(lm_grouped_against)) # looks great for the most part
jb.norm.test(MASS::stdres(lm_grouped_against))
# not as bad as the offensive model, but still not great

##### NON-BARCELONA MODELS #####

# copy the dataset again
datb <- allchar

# get a dataframe of just the barcelona games
onlyb <- datb %>% 
  filter(`Team ID` == 217) %>% 
  # move the match id column to be the first one
  dplyr::select(`Match ID`, everything())

# move the match id column to be the first one
datb <- datb %>% 
  dplyr::select(`Match ID`, everything())

# get a dataframe of all the games barcelona was not in
notbar <- anti_join(datb, onlyb)

# COMPLETELY NON-BARCELONA MODELS
# completely non-barcelona offensive model
lm_grouped_notbar <- lm(Total_xg ~ formtype_att +
                   Shots_tg + 
                   pass_accuracy + 
                   possession_ratio, 
                 data = notbar)
summary(lm_grouped_notbar)

# checking assumptions for the "not barcelona" offensive model
# linearity
avPlots(lm_grouped_notbar)
# still slightly weird, not quite what you want to see on the formtype plots
# but ok everywhere else
# seem to be more loosely grouped though than the "with barcelona" model

# linearity, independence, homoscedasticity/equal variance
plot(lm_grouped_notbar$fitted.values, lm_grouped_notbar$residuals)
hist(lm_grouped_notbar$fitted.values)
hist(lm_grouped_notbar$residuals)
plot(lm_grouped_notbar$fitted.values, MASS::stdres(lm_grouped_notbar))
# kinda sketchy, don't quite meet requirements
bptest(lm_grouped_notbar) # gets a REALLY low p-value, H_0: heteroscedastic
# that's not good lol, implies very unequal variances

# normality
hist(MASS::stdres(lm_grouped_notbar), freq = FALSE, xlim = c(-4, 4))
curve(dnorm, from = -4, to = 4, add = TRUE) # looks great



# completely non-barcelona defensive model
lm_against_notbar <- lm(Total_xg_against ~ formtype_def + 
                           Shots_tg_against + 
                           pass_accuracy_against + 
                           possession_ratio_against, 
                         data = notbar)

summary(lm_against_notbar)

# checking assumptions for the "against" non-barcelona model
# linearity
avPlots(lm_against_notbar)
# still slightly weird, not quite what you want to see on the formtype plots
# but ok everywhere else

# linearity, independence, homoscedasticity/equal variance
plot(lm_against_notbar$fitted.values, lm_against_notbar$residuals)
hist(lm_against_notbar$fitted.values)
hist(lm_against_notbar$residuals)
plot(lm_against_notbar$fitted.values, MASS::stdres(lm_against_notbar))
# kinda sketchy, don't quite meet requirements
bptest(lm_against_notbar) # gets a REALLY low p-value, H_0: heteroscedastic
# that's not good lol, implies very unequal variances

# normality
hist(MASS::stdres(lm_against_notbar), freq = FALSE, xlim = c(-4, 4))
curve(dnorm, from = -4, to = 4, add = TRUE) # looks great

qqnorm(MASS::stdres(lm_against_notbar)) # looks great for the most part
jb.norm.test(MASS::stdres(lm_against_notbar))
# doesn't actually look that bad!!




# MODELS WITH BARCELONA'S OPPONENTS BUT NOT BARCELONA
datc <- allchar
datc <- datc %>% 
  filter(`Team ID` != 217)

# offensive model
lm_grouped_baropp <- lm(Total_xg ~ formtype_att +
                          Shots_tg + 
                          pass_accuracy + 
                          possession_ratio, 
                        data = datc)
summary(lm_grouped_baropp)

# checking assumptions
# linearity
avPlots(lm_grouped_baropp)
# still slightly weird, not quite what you want to see on the formtype plots
# but ok everywhere else

# linearity, independence, homoscedasticity/equal variance
plot(lm_grouped_baropp$fitted.values, lm_grouped_baropp$residuals)
hist(lm_grouped_baropp$fitted.values)
hist(lm_grouped_baropp$residuals)
plot(lm_grouped_baropp$fitted.values, MASS::stdres(lm_grouped_baropp))
# kinda sketchy, don't quite meet requirements
bptest(lm_grouped_baropp) # gets a REALLY low p-value, H_0: heteroscedastic
# that's not good lol, implies very unequal variances

# normality
hist(MASS::stdres(lm_grouped_baropp), freq = FALSE, xlim = c(-4, 4))
curve(dnorm, from = -4, to = 4, add = TRUE) # looks great




# defensive model
lm_against_baropp <- lm(Total_xg_against ~ formtype_def + 
                          Shots_tg_against + 
                          pass_accuracy_against + 
                          possession_ratio_against, 
                        data = datc)

summary(lm_against_baropp)

# check assumptions
# linearity
avPlots(lm_against_baropp)
# still slightly weird, not quite what you want to see on the formtype plots
# but ok everywhere else

# linearity, independence, homoscedasticity/equal variance
plot(lm_against_baropp$fitted.values, lm_against_baropp$residuals)
hist(lm_against_baropp$fitted.values)
hist(lm_against_baropp$residuals)
plot(lm_against_baropp$fitted.values, MASS::stdres(lm_against_baropp))
# kinda sketchy, don't quite meet requirements
bptest(lm_against_baropp) # gets a REALLY low p-value, H_0: heteroscedastic
# that's not good lol, implies very unequal variances

# normality
hist(MASS::stdres(lm_against_baropp), freq = FALSE, xlim = c(-4, 4))
curve(dnorm, from = -4, to = 4, add = TRUE) # looks great

qqnorm(MASS::stdres(lm_against_baropp)) # looks great for the most part
jb.norm.test(MASS::stdres(lm_against_baropp))
# doesn't actually look that bad!!




##### USING MEAN_XG AS A RESPONSE VARIABLE #####

# just the same as the offensive lm_grouped model but with 
# mean_xg as a response variable
lm_grouped_meanxg <- lm(Mean_xg ~ formtype_att +
                          Shots_tg + 
                          pass_accuracy + 
                          possession_ratio, 
                        data = allchar)
# i included quite a few terms in the model to see what combo made sense and 
# and also gave a good R^2

summary(lm_grouped_meanxg)
# what is going on??? The R^2 is tiny

