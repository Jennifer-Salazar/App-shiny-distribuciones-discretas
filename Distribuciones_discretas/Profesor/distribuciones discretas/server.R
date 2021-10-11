
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$Probabilidad <- renderPrint({
    
    if(input$Distribucion == "Binomial"){
      bin<-dbinom(x=input$bn, size=input$bN, prob = input$bP)
      print(bin)} 
    
    if(input$Distribucion == "Poisson"){
      poi<-dpois(x=input$pN, lambda=input$pl)
      print(poi)}
      
    
      if(input$Distribucion == "Hipergeometrica"){
      hip<-dhyper(x=input$hxh,m=input$hm, n=input$hnh, k=input$hk, log=F)
      print(hip)}
    
  })
  
  output$Grafico <- renderPlot({
    
    if(input$Distribucion=="Binomial"){
      bin<-dbinom(x=input$bn, size=input$bN, prob = input$bP)
      barplot(bin, ylim=c(0, 1), names.arg=input$bnk,
              xlab="x", ylab=expression(P(X==x)), col="deepskyblue3", las=1)
      grid()}
    
    if(input$Distribucion=="Poisson"){
      poi<-dpois(x=input$pN, lambda=input$pl)
      barplot(poi, ylim=c(0, 1), names.arg=input$pN,
              xlab="x", ylab=expression(P(X==x)), col="brown1", las=1)
      grid()}
    
    if(input$Distribucion=="Hipergeometrica"){
      hip<-dhyper(x=input$hxh,m=input$hm, n=input$hnh, k=input$hk, log=F)
      barplot(hip, ylim=c(0,1), names.arg = input$hxh, xlab="x", ylab=expression(P(X==x)),
              col="darkorange", las=1)
      grid()}
  })
})
