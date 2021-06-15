#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


# Define UI for application that draws a histogram
ui <- dashboardPage(
               
                    # theme = "cerulean",  # <--- To use a theme, uncomment this
                    
                    dashboardHeader(title = "basic"),
                            dashboardSidebar(
                                 fluidRow(
                                     tags$h3("Input your stats below:"),
                                     selectInput("formation", label="Formation", choices = c("","343","352", "3142", "3232", "3322", "3421", "3511", "32122", "32212", "32221", "433", "442", "451", "4141", "4222", "4231", "4321", "4411", "41212", "41221", "42121", "42211", "541", "5122", "5221"), selected= NULL, multiple = F),
                                     textInput("SoT", "Shots on Target:", ""),
                                     textInput("Pa", "Passing Accuracy:", ""),
                                     textInput("Pr", "Possession Ratio:", ""),
                                     textInput("MeanXg", "Mean XG:", ""),
                                     textInput("SoTa", "Opponent Shots on Target:", ""),
                                     textInput("PaA", "Opponent Passing Accuracy:", ""),
                                     textInput("PrA", "Opponent Possession Ratio:", ""),
                                     submitButton("Process Stats")
                                     
                                 )), # sidebarPanel
                             dashboardBody(
                                 hr(),
                                 column(3, verbatimTextOutput("formation")),
                                 #box(width = 12, title = "442")
                                 h1("Soccer Formations"),
                                 #fluidRow(
                                 # box(width = 6, title = "test", status = "primary", solidHeader = TRUE, "Box Content")
                                 # )
                                 #img(src="image (1).png"),
                                 #h4("In memory of charles reep"),
                                 #verbatimTextOutput("txtout"),
                                 #tableOutput('table')
                                 
                             ) # mainPanel
                             
                    ) # Navbar 1, tabPanel
                    #tabPanel("Navbar 2", "This panel is intentionally left blank"),
                    
                    #tabPanel("Navbar 3", "This panel is intentionally left blank")
                    
                 # navbarPage


# Define server function  
server <- function(input, output) {
    
    output$formation <- renderText({
        paste("Your formation is:", input$formation)
    })
    
    output$txtout <- renderText({
        paste( input$txt1, input$txt2, sep = " " )
    })
    output$table <- renderTable(predict_total_xg(input))
    
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)