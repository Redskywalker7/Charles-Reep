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

#data <- read.csv("data/la_liga_shots_per_formation.csv")

predict_total_xg <- function(input){
    
    attackers_and_defenders <- 
    
    total_xg <- predict(model, input)
    return(total_xg)
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
                                 tags$h3("Input:"), #idk what we are going to do for inputs? maybe just have static images at this point
                                 textInput("txt1", "Given Name:", ""),
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