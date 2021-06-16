# just the offensive/defensive models

##### OFFENSIVE MODEL #####
lm_grouped <- lm(Total_xg ~ formtype_att + 
                   Shots_tg + 
                   pass_accuracy + 
                   possession_ratio, 
                 data = allchar)


##### DEFENSIVE MODEL #####
lm_grouped_against <- lm(Total_xg_against ~ formtype_def + # `Team Name` 
                           Shots_tg_against + 
                           pass_accuracy_against + 
                           possession_ratio_against, 
                         data = allchar)
