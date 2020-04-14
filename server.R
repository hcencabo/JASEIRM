library(deSolve)
library(shiny)

shinyServer (function(input, output) {
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
})
