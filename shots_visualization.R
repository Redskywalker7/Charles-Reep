install.packages('sportyR')
library(ggplot2)
library(sportyR)

data = read.csv('la_liga_shots_per_formation.csv')

# Change names of X.Coordinate and Y.coordinate to x and y respectively
names(data)[7:8] = c('x', 'y')

shots = data

shots = translate(shots, translate_x = -60, translate_y = -40)
formations = c('4231','352','433','442','41221','4222','41212',
               '32212','5122',	'4141',	'4321',	'343',	'3412',
               '32221',	'3232',	'32122','42211',	'3421',
               '451',	'4411',	'3511',	'3142')

for(formation in formations){
  form_for = shots[(shots$Formation == formation) &
                     (shots$For_or_Against == 'For') &
                     (shots$Outcome != 'Goal'),]
  form_for_goals = shots[(shots$Formation == formation) &
                           (shots$For_or_Against == 'For') &
                           (shots$Outcome == 'Goal'),]
  form_away = shots[(shots$Formation == formation) &
                      (shots$For_or_Against == 'Against')&
                      (shots$Outcome != 'Goal'),]
  form_away_goals = shots[(shots$Formation == formation) &
                            (shots$For_or_Against == 'Against')&
                            (shots$Outcome == 'Goal'),]
  
  form_away = reflect(form_away, over_y = TRUE)
  form_away_goals = reflect(form_away_goals, over_y = TRUE)
  
  colors <- c("Team Shots" = "green", "Team Goals" = "red", 
              "Opponent Shots" = "blue", "Opponent Goals" = "yellow")
  
  socc_stadium = geom_soccer(league = "FIFA") + labs(title=formation)
  
  print(socc_stadium + 
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(data = form_for, aes(x, y, color = "Team Shots")) +
    geom_point(data = form_for_goals, aes(x, y, color = 'Team Goals')) +
    geom_point(data = form_away, aes(x, y, color = 'Opponent Shots')) +
    geom_point(data = form_away_goals, aes(x, y, color = 'Opponent Goals')))
  
}
