#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggforce)
library(rgdal)
library(R2jags)
library(lmtest)
library(normtest)
library(car)
library(DT)


predict_total_xg <- function(input){
<<<<<<< HEAD
  attackers <- get_attackers(input$formation)
  defenders <- get_defenders(input$formation)
  dat <- read_csv("la_liga_dataset.csv")
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
  ##### DEFENSIVE MODEL #####
  lm_grouped_against <- lm(Total_xg_against ~ formtype_def + # `Team Name` 
                             Shots_tg_against + 
                             pass_accuracy_against + 
                             possession_ratio_against, 
                           data = allchar)
  attack_inputs <- data.frame(formtype_att=attackers, Shots_tg=input$SoT, 
                              pass_accuracy=input$Pa, possession_ratio=input$Pr)
  defender_inputs <- data.frame(formtype_def=defenders, Shots_tg_against=input$SoTa, 
                                pass_accuracy_against=input$PaA, possession_ratio_against=input$PrA)
  total_xg_for <- predict(lm_grouped, attack_inputs, interval = "predict")
  total_xg_against <- predict(lm_grouped_against, defender_inputs, interval = "predict")
  #total_xg_for <- 1
  #total_xg_against <- 2
  total_xg = list(total_xg_for, total_xg_against)
  return(total_xg)
  print(total_xg)
=======
    
    attackers <- get_attackers(input$formation)
    defenders <- get_defenders(input$formation)
    
    dat <- read_csv("la_liga_dataset.csv")
    
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
    
    ##### DEFENSIVE MODEL #####
    lm_grouped_against <- lm(Total_xg_against ~ formtype_def + # `Team Name` 
                               Shots_tg_against + 
                               pass_accuracy_against + 
                               possession_ratio_against, 
                             data = allchar)
    
    attack_inputs <- data.frame(formtype_att=attackers, Shots_tg=input$SoT, 
                                pass_accuracy=input$Pa, possession_ratio=input$Pr)
    defender_inputs <- data.frame(formtype_def=defenders, Shots_tg_against=input$SoTa, 
                                  pass_accuracy_against=input$PaA, possession_ratio_against=input$PrA)
    
    total_xg_for <- predict(lm_grouped, attack_inputs, interval = "predict")
    total_xg_against <- predict(lm_grouped_against, defender_inputs, interval = "predict")
    
    #total_xg_for <- 1
    #total_xg_against <- 2
    
    total_xg = list(total_xg_for, total_xg_against)
    return(total_xg)
>>>>>>> 78158c01f016c6dfb8175e669131670d3d75d781
}
get_attackers <- function(formation){
    # determine how many characters the formation is made of
    if(stringr::str_length(formation == 3)){
        attackers <- substring(formation, 3, 3)
        attackers <- factor(attackers)
    }
    else if(stringr::str_length(formation == 4)){
        strikers <- substring(formation, 4, 4)
        attack_mid <- substring(formation, 3, 3)
        
        strikers <- strtoi(strikers)
        attack_mid <- strtoi(attack_mid)
        
        attackers <- strikers + attack_mid
        attackers <- factor(attackers)
    }
    else if(stringr::str_length(formation == 5)){
        strikers <- substring(formation, 5, 5)
        attack_mid <- substring(formation, 4, 4)
        
        strikers <- strtoi(strikers)
        attack_mid <- strtoi(attack_mid)
        
        attackers <- strikers + attack_mid
        attackers <- factor(attackers)
    }
    else{
        "Defenders must be 3, 4, or 5"
    }
   
    return(attackers)
}

get_defenders <- function(formation){
    defenders <- substring(formation, 1, 1)
    defenders <- strtoi(defenders)
    defenders <- factor(defenders)
    
    return(defenders)
}

<<<<<<< HEAD
get_formation_reccomendation <- function(input){
  LaLigaData <- read.csv("la_liga_dataset.csv", header = TRUE, sep = ",")
=======
get_formation_recommendations <- function(input){
  
  LaLigaData <- read.csv("la_liga_dataset.csv", header = TRUE, sep = ",")
  
>>>>>>> 78158c01f016c6dfb8175e669131670d3d75d781
  Formation <-LaLigaData$Formation
  TeamNum <- LaLigaData$Team.ID
  N <- LaLigaData$X
  MeanXG <- LaLigaData$Mean_xg
  TotalXG <- LaLigaData$Total_xg
  PassAccuracy <- LaLigaData$pass_accuracy
  Goals <- LaLigaData$Goals
  PossRatio <- LaLigaData$possession_ratio
  TargetSt <- LaLigaData$Shots_tg
<<<<<<< HEAD
  La1 <- LaLigaData %>% 
    filter(Formation == "433")
  La2 <- LaLigaData %>% 
    filter(Formation == "442")
  La3 <- LaLigaData %>% 
    filter(Formation == "4321")
=======
  
  La1 <- LaLigaData %>% 
    filter(Formation == "433")
  
  La2 <- LaLigaData %>% 
    filter(Formation == "442")
  
  La3 <- LaLigaData %>% 
    filter(Formation == "4321")
  
>>>>>>> 78158c01f016c6dfb8175e669131670d3d75d781
  MeanXG1 <- La1$Mean_xg
  TotalXG1 <- La1$Total_xg
  Goals1 <- La1$Goals
  PassAcc1 <- La1$pass_accuracy
  Poss1 <- La1$possession_ratio
  TargetSt1 <- La1$Shots_tg
  MinuteXG1 <- La1$xg_per_minute
<<<<<<< HEAD
=======
  
>>>>>>> 78158c01f016c6dfb8175e669131670d3d75d781
  MeanXG2 <- La2$Mean_xg
  TotalXG2 <- La2$Total_xg
  Goals2 <- La2$Goals
  PassAcc2 <- La2$pass_accuracy
  Poss2 <- La2$possession_ratio
  TargetSt2 <- La2$Shots_tg
  MinuteXG2 <- La2$xg_per_minute
<<<<<<< HEAD
=======
  
>>>>>>> 78158c01f016c6dfb8175e669131670d3d75d781
  MeanXG3 <- La3$Mean_xg
  TotalXG3 <- La3$Total_xg
  Goals3 <- La3$Goals
  PassAcc3 <- La3$pass_accuracy
  Poss3 <- La3$possession_ratio
  TargetSt3 <- La3$Shots_tg
  MinuteXG3 <- La3$xg_per_minute
<<<<<<< HEAD
  #442 model and simulation
  LAModel1 <- "model {
=======
  
  #442 model and simulation
    LAModel1 <- "model {
>>>>>>> 78158c01f016c6dfb8175e669131670d3d75d781
    for(i in 1:199){
      Goals2[i] ~ dpois(lambda[i])
      log(lambda[i]) <- beta0 + beta1 * TargetSt2[i] + beta2 * MeanXG2[i]  + beta3 * Poss[i] + beta4 * PassAcc2[i]
  mean.default()
    }
    beta0 ~ dexp(1)
    beta1 ~ dnorm(0,1/100)
    beta2 ~ dnorm(0,1/100)
    beta3 ~ dnorm(0,1/100)
    beta4 ~ dnorm(0,1/100)
  }
  "
<<<<<<< HEAD
=======
  
>>>>>>> 78158c01f016c6dfb8175e669131670d3d75d781
  LA.sim1 <- jags(
    data=c('Goals2','TargetSt2','MeanXG2','Poss2','PassAcc2'),
    parameters.to.save=c('beta0','ppd','beta1','beta2','beta3','beta4'),
    model.file=textConnection(LAModel1),
    n.iter=12000,
    n.burnin=2000,
    n.chains=2,
    n.thin=2
  )
<<<<<<< HEAD
=======
  
>>>>>>> 78158c01f016c6dfb8175e669131670d3d75d781
  #433 model and simulation
  LAModel <- "model {
    for(i in 1:480){
      Goals1[i] ~ dpois(lambda[i])
      log(lambda[i]) <- beta0 + beta1 * TargetSt1[i] + beta2 * MeanXG1[i]  + beta3 * Poss1[i] + beta4 * PassAcc1[i]
      ppd[i] ~ dpois(lambda[i]) 
    }
    beta0 ~ dexp(1)
    beta1 ~ dnorm(0,1/100)
    beta2 ~ dnorm(0,1/100)
    beta3 ~ dnorm(0,1/100)
    beta4 ~ dnorm(0,1/100)
  }
  "
<<<<<<< HEAD
=======
  
>>>>>>> 78158c01f016c6dfb8175e669131670d3d75d781
  LA.sim <- jags(
    data=c('Goals1','TargetSt1','MeanXG1','Poss1','PassAcc1'),
    parameters.to.save=c('beta0','ppd','beta1','beta2','beta3','beta4'),
    model.file=textConnection(LAModel),
    n.iter=12000,
    n.burnin=2000,
    n.chains=2,
    n.thin=2
  )
<<<<<<< HEAD
=======
  
>>>>>>> 78158c01f016c6dfb8175e669131670d3d75d781
  #4231 model and simulation
  LAModel2 <- "model {
    for(i in 1:9){
      Goals3[i] ~ dpois(lambda[i])
      log(lambda[i]) <- beta0 + beta1 * TargetSt3[i] + beta2 * MeanXG3[i]  + beta3 * Poss3[i] + beta4 * PassAcc3[i]
      ppd[i] ~ dpois(lambda[i]) 
    }
    beta0 ~ dexp(1)
    beta1 ~ dnorm(0,1/100)
    beta2 ~ dnorm(0,1/100)
    beta3 ~ dnorm(0,1/100)
    beta4 ~ dnorm(0,1/100)
  }
  "
<<<<<<< HEAD
=======
  
>>>>>>> 78158c01f016c6dfb8175e669131670d3d75d781
  LA.sim2 <- jags(
    data=c('Goals3','TargetSt3','MeanXG3','Poss3','PassAcc3'),
    parameters.to.save=c('beta0','ppd','beta1','beta2','beta3','beta4'),
    model.file=textConnection(LAModel2),
    n.iter=102000,
    n.burnin=2000,
    n.chains=2,
    n.thin=2
  )
<<<<<<< HEAD
  #442 output
  mean(LA.sim1$BUGSoutput$sims.matrix[,7:205])
  #433 output
  mean(LA.sim$BUGSoutput$sims.matrix[,7:486])
=======
  
  #442 output
  mean(LA.sim1$BUGSoutput$sims.matrix[,7:205])
  
  #433 output
  mean(LA.sim$BUGSoutput$sims.matrix[,7:486])
  
>>>>>>> 78158c01f016c6dfb8175e669131670d3d75d781
  #4231 output
  mean(LA.sim2$BUGSoutput$sims.matrix[,7:15])
}

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                tags$head(
                  tags$style(HTML('#goButton{background-color:orange}'))
                  ),
                  
                navbarPage(
                    # theme = "cerulean",  # <--- To use a theme, uncomment this
                    "Futbol Formations",
                    tabPanel("Expected Goals",
                             sidebarPanel(
                                 fluidRow(
                                 tags$h3("Input your stats below:"),
                                 textInput("formation", "Enter formation:", value= NULL),
                                 numericInput("SoT", "Shots on Target:",  ""),
                                 numericInput("Pa", "Passing Accuracy:", ""),
                                 numericInput("Pr", "Possession Ratio:", ""),
                                 numericInput("MeanXg", "Mean XG:",  ""),
                                 numericInput("SoTa", "Opponent Shots on Target:", ""),
                                 numericInput("PaA", "Opponent Passing Accuracy:",  ""),
                                 numericInput("PrA", "Opponent Possession Ratio:",  ""),
                                 actionButton("goButton","Submit")
                                 
                             )), # sidebarPanel
                             mainPanel(
                                 hr(),
                                 h1("Team Expectations"),
                                 dataTableOutput("inputs"),
                                 hr(),
                                 column(1),
                                
                                 column(5,  h3("Your Expected Total XG"), verbatimTextOutput("team")),
                                 
                                 column(5, h3("Opponent Expected Total XG"), verbatimTextOutput("opp")),
                                 
                                 br(),
                                 h1(" "),
                                 fluidRow(
                                   br(),
                                   br(),
                                   br()
                                 ),
                                img(src="image (1).png", height = 475, width = 950)
                              
                                 #box(width = 12, title = "442")
                                 
                                 #fluidRow(
                                    # box(width = 6, title = "test", status = "primary", solidHeader = TRUE, "Box Content")
                                # )
                                 #img(src="image (1).png"),
                                 #h4("In memory of charles reep"),
                                 #verbatimTextOutput("txtout"),
                                 #tableOutput('table')
                                 
                             ) # mainPanel
                             
                    ), # Navbar 1, tabPanel
                    tabPanel("Heatmaps",  h1("Goals Scored For and Against in Various Formations"),
                             hr(),
                             
                             ),
                             
                    tabPanel("Visualizations", 
                            h1("Goals Scored For and Against in Various Formations"),
                            hr(),
                               column(4,
                               img(src="343.png", width = 400, height = 300),
                               h1(" "),
                               img(src="352.png", width = 400, height = 300),
                               h1(" "),
                               img(src="433.png", width = 400, height = 300),
                               h1(" "),
                               img(src="442.png", width = 400, height = 300)),
                             column(4,
                                    img(src="451.png", width = 400, height = 300),
                                    h1(" "),
                                    img(src="4141.png", width = 400, height = 300),
                                    h1(" "),
                                    img(src="4222.png", width = 400, height = 300),
                                    h1(" "),
                                    img(src="41221.png", width = 400, height = 300)),
                             column(4,
                                    img(src="4231.png", width = 400, height = 300),
                                    h1(" "),
                                    img(src="4411.png", width = 400, height = 300),
                                    h1(" "),
                                    img(src="41212.png", width = 400, height = 300)),
                             h1(" "),
                            hr(),
                            h1(" ")
                             )
                    
                ) # navbarPage
) # fluidPage


#predict_total_xg <- function (input){
  
#}
# Define server function  
server <- function(input, output) {
  
  data <- eventReactive(input$goButton, {
    matrix(
      c()
    )
  })
  
  observeEvent(
    eventExpr = input[["goButton"]],
    handlerExpr = {
      total_xg = predict_total_xg(input)
      total_xg <- unlist(total_xg)
      print(total_xg)
      team_xg <- total_xg[1]
      opp_xg <- total_xg[4]
      print(team_xg)
      print(opp_xg)
      output$team <-renderText(team_xg)
      output$opp <-renderText(opp_xg)
  
 
    #output$formation <- renderText({
    #    paste("Your PrA is:", input$formation)
   # })
    
    output$txtout <- renderText({
        paste( input$txt1, input$txt2, sep = " " )
    })}
  )
    # output$table <- renderTable(total_xg[1])
    
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)