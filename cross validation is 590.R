##### CROSS-VALIDATION #####

# uses allchar dataset
dat <- read_csv("la_liga_dataset (1).csv")
dat$Formation <- factor(dat$Formation)

##### grouping based on formation #####
datt <- dat
# save a copy of the original dataframe just for the sake of experimentation

### GROUPING BASED ON DEFENDERS ###

datt <- datt %>% 
  mutate(formtype_def = case_when(str_detect(datt$Formation, "^3") ~ "3",
                                  str_detect(datt$Formation, "^4") ~ "4",
                                  str_detect(datt$Formation, "^5") ~ "5"))
# create a new column "formtype_def" assigning a number to each formation based on 
# the number of defenders used in the lineup

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



##### 
# actual cross validation things
#####

# no log transform

set.seed(1)
n.test = 4
n.cv = 1000
bias = rep(NA, n.cv)
rpmse = rep(NA, n.cv)

for(cv in 1:n.cv){
  test.obs = sample(1:nrow(allchar), n.test)
  test.allcharp = allchar[test.obs,]
  train.allcharp = allchar[-test.obs,]
  train.lm = lm(Total_xg ~ formtype_att + Shots_tg + 
                  pass_accuracy + possession_ratio, 
                data = train.allchar)
  test.preds = train.lm$coef[1] +
    train.lm$coef[2]*test.allchar$formtype_att + 
    train.lm$coef[3]*test.allchar$Shots_tg + 
    train.lm$coef[5]*test.allchar$pass_accuracy + 
    train.lm$coef[6]*test.allchar$possession_ratio
  test.preds = predict.lm(train.lm,newdata=test.allchar)
  bias[cv] = mean(test.preds-test.allchar$Total_xg)
  rpmse[cv] = sqrt(mean((test.preds-test.allchar$Total_xg)^2))
}

mean(bias)
mean(rpmse) # why do these get error messages??
range(allcharp$Total_xg)


# no log transforms but with a sketchy dataset (can you just 
# change formation to a numeric variable? I don't really 
# think so since you can't average them out)
# it gets you an average bias and RPMSE though but its
# probably not useful/usable

allcharp <- allchar
allcharp$formtype_att <- as.numeric(allcharp$formtype_att)


set.seed(17)
n.test = 4
n.cv = 1000
bias = rep(NA, n.cv)
rpmse = rep(NA, n.cv)

for(cv in 1:n.cv){
  test.obs = sample(1:nrow(allcharp), n.test)
  test.allcharp = allcharp[test.obs,]
  train.allcharp = allcharp[-test.obs,]
  train.lm = lm(Total_xg ~ formtype_att + Shots_tg + 
                  pass_accuracy + possession_ratio, 
                data = train.allcharp)
  test.preds = train.lm$coef[1] +
    train.lm$coef[2]*test.allcharp$formtype_att + 
    train.lm$coef[3]*test.allcharp$Shots_tg + 
    train.lm$coef[5]*test.allcharp$pass_accuracy + 
    train.lm$coef[6]*test.allcharp$possession_ratio
  test.preds = predict.lm(train.lm,newdata=test.allcharp)
  bias[cv] = mean(test.preds-test.allcharp$Total_xg)
  rpmse[cv] = sqrt(mean((test.preds-test.allcharp$Total_xg)^2))
}

mean(bias)
mean(rpmse) # why do these get error messages??
range(allcharp$Total_xg)

# trying to do it with a log transform on the data
for(i in 1:n.cv){
  test.obs = sample(1:nrow(allchar),n.test)
  test.allchar = allchar[test.obs,]
  train.allchar = allchar[-test.obs,]
  train.lm = lm(log(Total_xg+1) ~ formtype_att + Shots_tg + 
                  Goals + pass_accuracy + possession_ratio, 
                data = train.allchar)
  test.preds = exp(train.lm$coef[1])+
    train.lm$coef[2]*test.allchar$formtype_att + 
    train.lm$coef[3]*test.allchar$Shots_tg + 
    train.lm$coef[4]*test.allchar$Goals + 
    train.lm$coef[5]*test.allchar$pass_accuracy + 
    train.lm$coef[6]*test.allchar$possession_ratio
  test.preds = exp(predict.lm(train.lm,newdata=test.allchar))
  bias.log[i] = mean((test.preds-test.allchar$Total_xg))
  rpmse.log[i] = sqrt(mean((test.preds-test.allchar$Total_xg)^2))
}

mean(bias.log)
mean(rpmse.log)
range(allchar$Total_xg)
