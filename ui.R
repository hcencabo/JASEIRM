library(shiny)

shinyUI(pageWithSidebar(
  headerPanel("SEIR Modeler"),
  sidebarPanel(
    
    #initial values input
    sliderInput(inputId = "tim", label = "Time", value = 10, min = 1, max = 365),
    sliderInput(inputId = "cr", label = "Contact Rate", value = 10, min = 0, max = 100),
    sliderInput(inputId = "tp", label = "Tramsmission Probability", value = 0.2, min = 0, max = 1),
    sliderInput(inputId = "ipd", label = "Infectious Period", value = 5, min = 0, max = 50),
    sliderInput(inputId = "lpd", label = "Latent Period", value = 2, min = 0, max = 50),
    sliderInput(inputId = "W", label = "Susceptible Population", value = 100, min = 0, max = 10917089),
    sliderInput(inputId = "X", label = "Intial Number of Infected", value = 1, min = 0, max = 300),
    sliderInput(inputId = "Y", label = "Intial Number of Exposed", value = 10, min = 0, max = 10000),
    
    tags$br()), 
  
  
  
  mainPanel(
    plotOutput("plot"),
    tags$br(), 
    h6("Created by: hcencabo"),
    h6("University of Southeastern Philippines")
    )
))

