library(deSolve)
library(shiny)


ui <- pageWithSidebar(
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
    #main panel items
    plotOutput("plot"),
    tags$br(), 
    h6("Created by: hcencabo"),
    h6("University of Southeastern Philippines"))
)

server <- function(input, output) {
  seir_ode<-function(time,Y,par){
    S<-Y[1]
    E<-Y[2]
    I<-Y[3]
    R<-Y[4]
    
    beta<-par[1]
    sigma<-par[2]
    gamma<-par[3]
    
    dYdt<-vector(length=3)
    dYdt[1]=(-beta * S * I)
    dYdt[2]=(beta * S * I) - (sigma * E)
    dYdt[3]=(sigma * E) - (gamma * I)
    
    return(list(dYdt))
  }
  
  
  # Solve system using lsoda. Make this reactive
  
  
  sol<-reactive({ #new reactive line
    req(input$cr, input$tp, input$ipd,input$lpd, input$W ,input$X ,input$Y )
    beta<-input$cr * input$tp
    sigma<-1/input$lpd
    gamma<-1/input$ipd
    N <- input$W + input$X + input$Y
    init <- c(input$W/N,input$X/N,input$Y/N)
    par = c(beta,sigma,gamma)
    t = seq(0,input$tim)
    
    lsoda(init,t,seir_ode,par)
    
  }) 
  
  output$plot <- renderPlot({
    sols <- as.data.frame(sol())
    with(sols, {
      plot(time,sols[,2],type="l",col="blue",ylim=c(0,1),ylab="Proportion", xlab="Day")
      lines(time,sols[,3],col="orange")
      lines(time,sols[,4],col="red")  
      lines(time,1-rowSums(sols[,2:4]),col="green")
    })
    legend("right",0.7,legend=c("S","E","I","R"),col=c("blue","orange","red","green"), lty=1,cex=0.8)
    
  })
  
  
}

shinyApp(ui = ui, server = server)



