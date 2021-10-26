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
                
                # k <- 5  # numero de desviaciones
                # curve(dnorm(x, media, desvi), xlim=media+c(-k,k)*desvi, lwd=3,
                #       main='Distribución normal', ylab="", xlab="", axes=FALSE)
                # axis(1, at=seq(media-k*desvi, media+k*desvi, desvi), pos=0)
                # axis(2, las=1)
                # secuencia <- seq(media-k*desvi, percentil, length.out=10000)
                # cord.x <- c(media-k*desvi, secuencia, percentil)
                # cord.y <- c(0, dnorm(secuencia, media, desvi), 0)
                # polygon(cord.x, cord.y, col='steelblue')
                # shadowtext(x=percentil, y=0, round(percentil, 2), col="chartreuse", cex=2)
                # title(sub=bquote(P(X<.(percentil))==.(proba)), cex.sub=2)
                
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
                    colores[(cuantil+1):(n+2)] <- "#D95914"
                    
                    probabilidad <- pbinom(cuantil, size = n, prob = p, lower.tail = F)
                    prob <- pbinom(0:n, size=n, prob=p, lower.tail = F)
                    barplot(prob, ylim=c(0, 1), names.arg=1:(n+1),
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
                

                # curve(dt(x, df), xlim=c(-5,5), lwd=3,
                #       main='Distribución t-student', ylab="", xlab="",
                #       axes=FALSE)
                # axis(1, at=seq(-5, 5, by=0.5), pos=0)
                # axis(2, las=1)
                # secuencia <- seq(percentil, 5, length.out=10000)
                # cord.x <- c(percentil, secuencia, 5)
                # cord.y <- c(0, dt(secuencia, df=df), 0)
                # polygon(cord.x, cord.y, col='darkolivegreen3')
                # shadowtext(x=percentil, y=0.01, round(percentil, 2),
                #            col="chartreuse", cex=2)
                # title(sub=bquote(P(t>.(percentil))==.(proba)), cex.sub=2)
                # 
                # output$perce <- renderText(percentil)
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
                    
                    prob <- ppois(0:n, lambda=lambda)
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
                

                # max.x <- 3 * percentil
                # curve(df(x, df1, df2), xlim=c(0, max.x), lwd=3,
                #       main='Distribución F', ylab="", xlab="", axes=FALSE)
                # axis(1, at=seq(0, max.x, by=0.5), pos=0)
                # axis(2, las=1)
                # secuencia <- seq(percentil, max.x, length.out=10000)
                # cord.x <- c(percentil, secuencia, max.x)
                # cord.y <- c(0, df(secuencia, df1, df2), 0)
                # polygon(cord.x, cord.y, col='lightsalmon3')
                # shadowtext(x=percentil, y=0.01, round(percentil, 2),
                #            col="chartreuse", cex=2)
                # title(sub=bquote(P(F>.(percentil))==.(proba)), cex.sub=2)

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
                    
                    prob <- phyper(q = 0:min(n, k), m = k, n = N-k, k = n)
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
        
        #----------------------------- Binomial Negativa ---------------------------------
        
        if(input$Distribucion == "Binomial Negativa"){
            r <- input$r_nbin
            p <- input$p_nbin
            
            if(input$Propede == "Cuantil"){
                
                probabilidad <- input$Probabilidad
                
                # k <- 5  # numero de desviaciones
                # curve(dnorm(x, media, desvi), xlim=media+c(-k,k)*desvi, lwd=3,
                #       main='Distribución normal', ylab="", xlab="", axes=FALSE)
                # axis(1, at=seq(media-k*desvi, media+k*desvi, desvi), pos=0)
                # axis(2, las=1)
                # secuencia <- seq(media-k*desvi, percentil, length.out=10000)
                # cord.x <- c(media-k*desvi, secuencia, percentil)
                # cord.y <- c(0, dnorm(secuencia, media, desvi), 0)
                # polygon(cord.x, cord.y, col='steelblue')
                # shadowtext(x=percentil, y=0, round(percentil, 2), col="chartreuse", cex=2)
                # title(sub=bquote(P(X<.(percentil))==.(proba)), cex.sub=2)
                
            }
            
            else {
                
                cuantil <- input$Cuantil
                
                
                if(input$Acumulado == "acumulada"){
                    

                    probabilidad <- pnbinom(cuantil-r, r, p)
                    prob <- pnbinom(0:(cuantil-r), r, p)
                    barplot(prob, ylim=c(0, 1), names.arg=r:cuantil,
                            xlab=" ", ylab=expression(P(X<=x)), col="#D95914", las=1)
                    grid()
                    
                    title(sub=bquote(P(X <= .(cuantil))==.(probabilidad)), cex.sub=2)
                    
                }else if(input$Acumulado == "supervivencia"){
                
        
                    
                    
                }else{
                    
                    colores <- rep("cyan4", cuantil-r+1)
                    colores[cuantil- r + 1] <- "#D95914"
                    
                    probabilidad <- dnbinom(cuantil-r, r, p)
                    
                    prob <- dnbinom(x=0:(cuantil-r), size=r, prob=p)
                    barplot(prob, ylim=c(0, 1), names.arg=r:cuantil,
                            xlab=" ", ylab=expression(P(X==x)), col=colores, las=1)
                    grid()
                    title(sub=bquote(P(X == .(cuantil))==.(probabilidad)), cex.sub=2)
                }
                
                
            }}
        
        

    })
})
