library(shiny)

shinyServer(function(input, output, session){
    
    #--------------------------------------------------------------
    
    output$miplot <- renderPlot({
        
        #----------------------------- Binomial  ---------------------------------
        
        if(input$Distribucion == "Binomial"){
            n <- input$n_binomial
            p <- input$p
            
            if(input$Propede == "Cuantil"){
                
                probabilidad <- input$Probabilidad
                
                cuantil <- qbinom(probabilidad, n, p)
                prob1 <- round(pbinom(cuantil, n, p), 4)
                prob2 <- round(pbinom(cuantil-1, n, p),4)
                
                colores <- rep("cyan4", n + 1)
                colores[c(cuantil,cuantil+1)] <- "#D95914"
                
                
                prob <- pbinom(0:n, size=n, prob=p)
                barplot(prob, ylim=c(0, 1), names.arg=0:n,
                        xlab=" ", ylab=expression(P(X<=x)), col=colores, las=1)
                grid()
                

                
                title(sub=bquote(P[.(probabilidad)]==.(cuantil)), cex.sub=2)
                
                
                output$Texto_prueba <-  renderUI({
                    
                    list(h4(paste("P(X ≤", cuantil-1, ")=" ,prob2, sep="")), 
                         h4(paste("P(X ≤", cuantil, ")=" ,prob1, sep="")))
                    
                    
                    
                })
                
                
                
                
            }
            
            else {
                
                cuantil <- input$Cuantil
                
                
                if(input$Acumulado == "acumulada"){
                    
                    colores <- rep("cyan4", n + 1)
                    colores[0:cuantil + 1] <- "#D95914"
                    
                    probabilidad <- pbinom(cuantil, size = n, prob = p)
                    prob <- pbinom(0:n, size=n, prob=p)
                    barplot(prob, ylim=c(0, 1), names.arg=0:n,
                            xlab=" ", ylab=expression(P(X<=x)), col=colores, las=1)
                    grid()
                    
                    title(sub=bquote(P(X <= .(cuantil))==.(probabilidad)), cex.sub=2)
                    
                }else if(input$Acumulado == "supervivencia"){
                    
                    
                    colores <- rep("cyan4", n + 1)
                    colores[(cuantil+2):(n+1)] <- "#D95914"
                    
                    probabilidad <- pbinom(cuantil, size = n, prob = p, lower.tail = F)
                    prob <- pbinom(-1:(n-1), size=n, prob=p, lower.tail = F)
                    barplot(prob, ylim=c(0, 1), names.arg=0:n,
                            xlab=" ", ylab=expression(P(X>x)), col=colores, las=1)
                    grid()
                    
                    title(sub=bquote(P(X > .(cuantil))==.(probabilidad)), cex.sub=2)
                    
                    
                }else{
                    
                    colores <- rep("cyan4", n + 1)
                    colores[cuantil + 1] <- "#D95914"
                    
                    probabilidad <- dbinom(cuantil, size = n, prob = p)
                    
                    prob <- dbinom(x=0:n, size=n, prob=p)
                    barplot(prob, ylim=c(0, 1), names.arg=0:n,
                            xlab=" ", ylab=expression(P(X==x)), col=colores, las=1)
                    grid()
                    title(sub=bquote(P(X == .(cuantil))==.(probabilidad)), cex.sub=2)
                }
                
                
            }}
        
        # #----------------------------- Poisson ---------------------------------

        if(input$Distribucion == "Poisson"){
            lambda <- input$lambda
            n <- qpois(0.99999, lambda)


            if(input$Propede == "Cuantil"){
                probabilidad <- input$Probabilidad
                
                
                cuantil <- qpois(probabilidad, lambda)
                prob1 <- round(ppois(cuantil, lambda), 4)
                prob2 <- round(ppois(cuantil-1, lambda),4)
                
                colores <- rep("cyan4", n + 1)
                colores[c(cuantil,cuantil+1)] <- "#D95914"

                
                prob <- ppois(0:n, lambda=lambda)
                barplot(prob, ylim=c(0, 1), names.arg=0:n,
                        xlab=" ", ylab=expression(P(X<=x)), col=colores, las=1)
                grid()
                
                
                
                title(sub=bquote(P[.(probabilidad)]==.(cuantil)), cex.sub=2)
                
                
                output$Texto_prueba <-  renderUI({
                    
                    list(h4(paste("P(X ≤", cuantil-1, ")=" ,prob2, sep="")), 
                         h4(paste("P(X ≤", cuantil, ")=" ,prob1, sep="")))
                    
                    
                    
                })
                

               
            }

            else {
                
                cuantil <- input$Cuantil
                
                
                if(input$Acumulado == "acumulada"){
                    
                    colores <- rep("cyan4", n + 1)
                    colores[0:cuantil + 1] <- "#D95914"
                    
                    probabilidad <- ppois(cuantil, lambda)
                    
                    prob <- ppois(0:n, lambda=lambda)
                    barplot(prob, ylim=c(0, 1), names.arg=0:n,
                            xlab="", ylab=expression(P(X<=x)), col=colores, las=1)
                    grid()
                    
                    title(sub=bquote(P(X <= .(cuantil))==.(probabilidad)), cex.sub=2)
                    
                    
                }else if(input$Acumulado == "supervivencia"){
                    
                    colores <- rep("cyan4", n + 1)
                    colores[(cuantil + 2):(n+1)] <- "#D95914"
                    
                    probabilidad <- ppois(cuantil, lambda, lower.tail = F)
                    
                    prob <- ppois(-1:(n-1), lambda=lambda, lower.tail = F)
                    barplot(prob, ylim=c(0, 1), names.arg=0:n,
                            xlab="", ylab=expression(P(X>x)), col=colores, las=1)
                    grid()
                    
                    title(sub=bquote(P(X > .(cuantil))==.(probabilidad)), cex.sub=2)
                   
                    
                    
                }else{
                    
                    colores <- rep("cyan4", n + 1)
                    colores[cuantil + 1] <- "#D95914"
                    
                    probabilidad <- dpois(cuantil, lambda)
                    prob <- dpois(x=0:n, lambda=lambda)
                    barplot(prob, ylim=c(0, 1), names.arg=0:n,
                            xlab="", ylab=expression(P(X==x)), col=colores, las=1)
                    grid()
                    
                    title(sub=bquote(P(X == .(cuantil))==.(probabilidad)), cex.sub=2)
                }
                
                

            }

        }

        # #------------------------- Hipergeometrica  ---------------------------------

        if(input$Distribucion == "Hipergeometrica"){
            k <- as.integer(input$k)
            N <- as.integer(input$N)
            n <- as.integer(input$n_hipergeometrica)
            

            if(input$Propede == "Cuantil"){
                
                probabilidad <- input$Probabilidad
                
                
                cuantil <- qhyper(probabilidad, m = k, n = N-k, k = n)
                prob1 <- round(phyper(q = cuantil, m = k, n = N-k, k = n), 4)
                prob2 <- round(phyper(q = cuantil-1, m = k, n = N-k, k = n),4)
       
                
                colores <- rep("cyan4", min(n, k) + 1)
                colores[c(cuantil,cuantil+1)] <- "#D95914"
                
                
                prob <- phyper(q = 0:min(n, k), m = k, n = N-k, k = n)
                
                barplot(prob, ylim=c(0, 1), names.arg=0:min(n, k),
                        xlab=" ", ylab=expression(P(X<=x)), col=colores, las=1)
                grid()
                
                
                title(sub=bquote(P[.(probabilidad)]==.(cuantil)), cex.sub=2)
                
                
                output$Texto_prueba <-  renderUI({
                    
                    list(h4(paste("P(X ≤", cuantil-1, ")=" ,prob2, sep="")), 
                         h4(paste("P(X ≤", cuantil, ")=" ,prob1, sep="")))
                    
                    
                    
                })
                


            }else{
                
                cuantil <- input$Cuantil
                
                
                if(input$Acumulado == "acumulada"){
                    
                    colores <- rep("cyan4", min(n, k) + 1)
                    colores[0:cuantil + 1] <- "#D95914"
                    
                    probabilidad <- phyper(q = cuantil, m = k, n = N-k, k = n)
                    
                    prob <- phyper(q = 0:min(n, k), m = k, n = N-k, k = n)
                    barplot(prob, ylim=c(0, 1), names.arg=0:min(n, k),
                            xlab=" ", ylab=expression(P(X<=x)), col=colores, las=1)
                    grid()
                    
                    title(sub=bquote(P(X <= .(cuantil))==.(probabilidad)), cex.sub=2)
                    
                }else if(input$Acumulado == "supervivencia"){
                    
                    
                    colores <- rep("cyan4", min(n, k) + 1)
                    colores[(cuantil + 2):(min(n, k) + 1)] <- "#D95914"
                    
                    probabilidad <- phyper(q = cuantil, m = k, n = N-k, k = n, lower.tail = F)
                    
                    prob <- phyper(q = -1:(min(n, k)-1), m = k, n = N-k, k = n, lower.tail = F)
                    barplot(prob, ylim=c(0, 1), names.arg=0:min(n, k),
                            xlab=" ", ylab=expression(P(X>x)), col=colores, las=1)
                    grid()
                    
                    title(sub=bquote(P(X > .(cuantil))==.(probabilidad)), cex.sub=2)
                    
                    
                    
                }else{
                    
                    colores <- rep("cyan4", min(n, k) + 1)
                    colores[cuantil + 1] <- "#D95914"
                    
                    probabilidad <- dhyper(x = cuantil, m = k, n = N-k, k = n)
                    
                    prob <- dhyper(x = 0:min(n, k), m = k, n = N-k, k = n)
                    barplot(prob, ylim=c(0, 1), names.arg=0:min(n, k),
                            xlab=" ", ylab=expression(P(X<=x)), col=colores, las=1)
                    grid()
                    
                    title(sub=bquote(P(X == .(cuantil))==.(probabilidad)), cex.sub=2)
                }

            }}
        

    })
})
