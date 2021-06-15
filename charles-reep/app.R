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


predict_total_xg <- function(input){
    
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
    
    attack_inputs <- data.frame(formtype_att=attackers, Shots_tg=input$shots_on_target, 
                                pass_accuracy=input$pass_accuracy, possession_ratio=input$poss_ratio)
    defender_inputs <- data.frame(formtype_def=defenders)
    
    total_xg_for <- predict(lm_grouped, attack_inputs, interval = "predict")
    return(total_xg)
}

get_attackers <- function(formation){
    # determine how many characters the formation is made of
    if(stringr::str_length(formation == 3)){
        attackers <- substring(formation, 3, 3)
        attackers <- strtoi(attackers)
    }
    else if(stringr::str_length(formation == 4)){
        strikers <- substring(formation, 4, 4)
        attack_mid <- substring(formation, 3, 3)
        
        strikers <- strtoi(strikers)
        attack_mid <- strtoi(attack_mid)
        
        attackers <- strikers + attack_mid
    }
    else if(stringr::str_length(formation == 5)){
        strikers <- substring(formation, 5, 5)
        attack_mid <- substring(formation, 4, 4)
        
        strikers <- strtoi(strikers)
        attack_mid <- strtoi(attack_mid)
        
        attackers <- strikers + attack_mid
    }
    else{
        "Defenders must be 3, 4, or 5"
    }
   
    return(attackers)
}

get_defenders <- function(formation){
    defenders <- substring(formation, 1, 1)
    
    return(defenders)
}

lewisFormation <- function(id){
    LaLigaData <- read.csv("la_liga_dataset.csv", header = TRUE, sep = ",")
    La1 <- LaLigaData %>%
        filter(Formation == "433")
    La2 <- LaLigaData %>%
        filter(Formation == "442")
    La3 <- LaLigaData %>%
        filter(Formation == "4321")
    MeanXG1 <- La1$Mean_xg
    TotalXG1 <- La1$Total_xg
    Goals1 <- La1$Goals
    PassAcc1 <- La1$pass_accuracy
    Poss1 <- La1$possession_ratio
    TargetSt1 <- La1$Shots_tg
    MinuteXG1 <- La1$xg_per_minute
    MeanXG2 <- La2$Mean_xg
    TotalXG2 <- La2$Total_xg
    Goals2 <- La2$Goals
    PassAcc2 <- La2$pass_accuracy
    Poss2 <- La2$possession_ratio
    TargetSt2 <- La2$Shots_tg
    MinuteXG2 <- La2$xg_per_minute
    MeanXG3 <- La3$Mean_xg
    TotalXG3 <- La3$Total_xg
    Goals3 <- La3$Goals
    PassAcc3 <- La3$pass_accuracy
    Poss3 <- La3$possession_ratio
    TargetSt3 <- La3$Shots_tg
    MinuteXG3 <- La3$xg_per_minute
    LAModel <- "model {
        for(i in 1:480){
            Goals1[i] ~ dpois(lambda[i])
            log(lambda[i]) <- beta0 + beta1 * PassAcc1[i] + beta2 * MeanXG1[i] + beta3 * Poss1[i]
            ppd[i] ~ dpois(lambda[i])
        }
      beta0 ~ dexp(1)
      beta1 ~ dnorm(0,1/100)
      beta2 ~ dnorm(0,1/100)
      beta3 ~ dnorm(0,1/100)
    }"
    
    LA.sim <- jags(
        data=c('Goals1','PassAcc1','MeanXG1','Poss1'),
        parameters.to.save=c('beta0','ppd','beta1','beta2','beta3'),
        model.file=textConnection(LAModel),
        n.iter=5200,
        n.burnin=2000,
        n.chains=2,
        n.thin=1
    ) 
plot(LA.sim)
}

# Define UI
ui <- fluidPage(theme = shinytheme("superhero"),
                navbarPage(
                    # theme = "cerulean",  # <--- To use a theme, uncomment this
                    "My first app",
                    tabPanel("Navbar 1",
                             sidebarPanel(
                                 tags$h3("Input your stats below:"),
                                 selectInput("formation", label="Formation", choices = c("","343","352", "3142", "3232", "3322", "3421", "3511", "32122", "32212", "32221", "433", "442", "451", "4141", "4222", "4231", "4321", "4411", "41212", "41221", "42121", "42211", "541", "5122", "5221"), selected= NULL, multiple = F),
                                 textInput("SoT", "Shots on Target:", ""),
                                 textInput("Pa", "Passing Accuracy:", ""),
                                 textInput("Pr", "Possession Ratio:", ""),
                                 textInput("MeanXg", "Mean XG:", ""),
                                 textInput("SoTa", "Opponent Shots on Target:", ""),
                                 textInput("PaA", "Opponent Passing Accuracy:", ""),
                                 textInput("PrA", "Opponent Possession Ratio:", "")
                                 
                                  #idk what we are going to do for inputs? maybe just have static images at this point
                                 
                                 #textInput("txt2", "Surname:", ""),
                                 
                             ), # sidebarPanel
                             mainPanel(
                                 h1("Soccer Formations"),
                                 img(src="image (1).png"),
                                 h4("In memory of charles reep"),
                                 verbatimTextOutput("txtout"),
                                 tableOutput('table')
                                 
                             ) # mainPanel
                             
                    ), # Navbar 1, tabPanel
                    tabPanel("Navbar 2", "This panel is intentionally left blank"),
                             
                    tabPanel("Navbar 3", "This panel is intentionally left blank")
                    
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
    

    output$txtout <- renderText({
        paste( input$txt1, input$txt2, sep = " " )
    })
    output$table <- renderTable(predict_total_xg(input))
    
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)